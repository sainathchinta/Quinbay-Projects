package com.gdn.x.productcategorybase.service.brand;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.x.productcategorybase.Constants.HYPHEN;
import static com.gdn.x.productcategorybase.Constants.SPACE;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationWip;
import com.gdn.x.productcategorybase.repository.brand.BrandAuthorisationWipRepository;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.time.DateUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import com.gdn.common.util.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.ApplicationContext;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.x.productcategorybase.BrandAuthorisationActivity;
import com.gdn.x.productcategorybase.BrandAuthorisationStatus;
import com.gdn.x.productcategorybase.CacheNames;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthBulkDownloadResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthFilterRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthFilterResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.ProductBrandValidationRequest;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.BrandAuthorisationDetailResponse;
import com.gdn.x.productcategorybase.dto.response.ProtectedBrandResponse;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisation;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import com.gdn.x.productcategorybase.repository.brand.BrandAuthorisationRepository;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.impl.ApplicationCacheServiceBean;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class BrandAuthorisationServiceBean implements BrandAuthorisationService {

  @Value("${brand.auth.end.date.years.add}")
  private int numberOfYears;

  @Value("${relax.takeDown.for.protected.brand.switch}")
  private boolean relaxTakeDownForProtectedBrandSwitch;

  @Value("${brand.auth.near.expiry.days.threshold}")
  private Integer brandAuthNearExpiryDaysThreshold;

  @Value("${fetch.brand.code.from.product}")
  private boolean fetchBrandCodeFromProduct;

  @Autowired
  private BrandAuthorisationRepository authorisationRepository;

  @Autowired
  private BrandAuthorisationWipRepository brandAuthorisationWipRepository;

  @Autowired
  @Lazy
  private BrandService brandService;

  @Autowired
  private ApplicationContext applicationContext;

  @Autowired
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Autowired
  private BrandAuthHistoryService brandAuthHistoryService;

  @Autowired
  private DomainEventPublisherService domainEventPublisherService;

  private static final String ACTIVE = "ACTIVE";
  private static final String INACTIVE = "INACTIVE";
  private static final String EXPIRED = "EXPIRED";

  @Override
  public boolean checkBrandAuthBySellerCode(String storeId, String sellerCode, String brandCode,
    boolean toTakeDown) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), String.valueOf(ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK));
    checkArgument(StringUtils.isNotBlank(sellerCode), String.valueOf(ErrorMessage.SELLER_CODE_MUST_NOT_BE_BLANK));
    checkArgument(StringUtils.isNotBlank(brandCode), String.valueOf(ErrorMessage.BRAND_CODE_MUST_NOT_BE_BLANK));
    BrandResponse brand = brandService.findByBrandCodeCached(storeId, brandCode);
    if (!Constants.INTERNAL.equalsIgnoreCase(sellerCode) && Objects.nonNull(brand)
      && brand.isProtectedBrand() && getRelaxValueForTakeDownInCaseOfProtectedBrand(brand)) {
      if (toTakeDown || !brand.isSkuCreationAllowedForAllSellers()) {
        List<BrandAuthorisation> brands = getBrandAuthorisationServiceBean()
          .findBrandAuthorisationBySellerCodeAndBrandCode(storeId, brandCode);
        if (CollectionUtils.isNotEmpty(brands)) {
          return brands.stream()
              .filter(brandAuth -> StringUtils.equals(brandAuth.getSellerCode(), sellerCode))
              .anyMatch(this::isBrandSellerAuthValid);
        }
      } else {
        return brand.isSkuCreationAllowedForAllSellers();
      }
      return false;
    } else {
      return true;
    }
  }

  private boolean getRelaxValueForTakeDownInCaseOfProtectedBrand(BrandResponse brand) {
    return !relaxTakeDownForProtectedBrandSwitch || !brand.isSkuCreationAllowedForAllSellers();
  }

  private boolean isBrandSellerAuthValid(BrandAuthorisation brandAuthorisation) {
    Date now = new Date();
    return (Objects.nonNull(brandAuthorisation) && BrandAuthorisationStatus.ACTIVE.name()
        .equals(brandAuthorisation.getAuthorisationStatus().name()) && brandAuthorisation.getAuthStartDate().before(now)
        && brandAuthorisation.getAuthExpireDate().after(now));
  }

  @Override
  @Cacheable(value = CacheNames.BRAND_AUTHORISATION, key = "#brandCode", unless = "#result == null")
  public List<BrandAuthorisation> findBrandAuthorisationBySellerCodeAndBrandCode(String storeId,
      String brandCode) {
    checkArgument(StringUtils.isNotBlank(storeId), String.valueOf(ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK));
    checkArgument(StringUtils.isNotBlank(brandCode), String.valueOf(ErrorMessage.BRAND_CODE_MUST_NOT_BE_BLANK));
    List<BrandAuthorisation> brands =
        authorisationRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(storeId, brandCode);
    return brands;
  }

  @Override
  public Page<BrandAuthFilterResponse> findBrandAuthorisationByFilter(String storeId, BrandAuthFilterRequest request,
      int page, int size) {
    checkArgument(StringUtils.isNotBlank(storeId), String.valueOf(ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK));
    Pageable pageable = PageRequest.of(page, size);
    if (request.isBrandAuthorise()) {
      Page<BrandAuthorisation> response =
          authorisationRepository.findBrandAuthorisationBySellerCodeAndBrandNameAndAuthorisationStatus(
              storeId, request.getSellerCode(), request.getBrandName(), request.getStatus(),
              pageable, brandAuthNearExpiryDaysThreshold);
      if (Objects.nonNull(response) && CollectionUtils.isNotEmpty(response.getContent())) {
        List<BrandAuthFilterResponse> brandAuthFilterResponses =
            ConverterUtil.toBrandAuthFilterResponse(response, request.getStatus(), brandAuthNearExpiryDaysThreshold);
        return new PageImpl<>(brandAuthFilterResponses, pageable, response.getTotalElements());
      }
    } else {
      Page<BrandAuthorisationWip> response =
          brandAuthorisationWipRepository.findBrandAuthorisationWipBySellerCodeAndBrandNameAndAuthorisationStatus(
              storeId, request.getSellerCode(), request.getBrandName(), request.getStatus(),
              pageable);
      if (Objects.nonNull(response) && CollectionUtils.isNotEmpty(response.getContent())) {
        List<BrandAuthFilterResponse> brandAuthFilterResponses =
            ConverterUtil.toBrandAuthFilterResponseForWip(response, request.getStatus());
        return new PageImpl<>(brandAuthFilterResponses, pageable, response.getTotalElements());
      }
    }
    return new PageImpl<>(new ArrayList<>(), pageable, 0);
  }

  @Override
  public BrandAuthorisationDetailResponse getBrandAuthDetailByBrandCodeAndSellerCode(String storeId,
    String brandCode, String sellerCode) throws Exception{
    checkArgument(StringUtils.isNotBlank(storeId), String.valueOf(ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK));
    checkArgument(StringUtils.isNotBlank(sellerCode), String.valueOf(ErrorMessage.SELLER_CODE_MUST_NOT_BE_BLANK));
    checkArgument(StringUtils.isNotBlank(brandCode), String.valueOf(ErrorMessage.BRAND_CODE_MUST_NOT_BE_BLANK));
    BrandAuthorisationDetailResponse brandAuthorisationDetailResponse = null;
    List<BrandAuthorisation> brands =
      getBrandAuthorisationServiceBean().findBrandAuthorisationBySellerCodeAndBrandCode(storeId, brandCode);
    if(CollectionUtils.isNotEmpty(brands)){
      BrandAuthorisation brandAuthorisation =
        brands.stream().filter(brand -> StringUtils.equals(brand.getSellerCode(), sellerCode))
          .findFirst().orElse(null);
      log.info("BrandAuthorisation for sellerCode : {} and brandCode : {} is : {}", sellerCode,
        brandCode, brandAuthorisation);
      if(Objects.nonNull(brandAuthorisation)){
        brandAuthorisationDetailResponse =
          ConverterUtil.convertToBrandAuthDetailResponse(brandAuthorisation);
      }
    }
    return brandAuthorisationDetailResponse;
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public String deleteMappingByBrandCodeAndSellerCode(String storeId, String username,
    String sellerCode, String brandCode, String status, String id) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId),
      String.valueOf(ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK));
    if (Objects.isNull(username) || StringUtils.isAllBlank(username)) {
      username = GdnMandatoryParameterUtil.getUsername();
    }
    if (Constants.UPCOMING.equals(status)) {
      BrandAuthorisationWip brandAuthorisationWip =
        brandAuthorisationWipRepository.findById(id).orElse(null);
      if (Objects.nonNull(brandAuthorisationWip)) {
        brandAuthorisationWipRepository.deleteUpcomingBrandAuthById(brandAuthorisationWip.getId(),
          username);
        this.domainEventPublisherService.publishBrandAuthHistoryEvent(
          brandAuthorisationWip.getBrandCode(), brandAuthorisationWip.getSellerCode(),
          generateBrandAuthHistoryForDelete(brandAuthorisationWip.getSellerCode(),
            brandAuthorisationWip.getBrandCode(),
            brandAuthorisationWip.getAuthorisationStatus().name()));
      }
    } else {
      List<BrandAuthorisation> brandsByBrandCode =
        getBrandAuthorisationServiceBean().findBrandAuthorisationBySellerCodeAndBrandCode(storeId,
          brandCode);
      if (CollectionUtils.isNotEmpty(brandsByBrandCode) && brandsByBrandCode.stream().anyMatch(
        brandAuthorisation -> StringUtils.equals(brandAuthorisation.getSellerCode(), sellerCode))) {
        log.info("Deleting Brand Auth details for seller code : {} ", sellerCode);
        authorisationRepository.deleteByStoreIdAndBrandCodeAndSellerCode(storeId, username,
          brandCode, sellerCode);

        String brandStatus = brandsByBrandCode.stream().filter(
            brandAuthorisation -> StringUtils.equals(brandAuthorisation.getSellerCode(), sellerCode))
          .findFirst().map(auth -> auth.getAuthorisationStatus().name()).orElse(StringUtils.EMPTY);

        applicationCacheServiceBean.evictBrandAuthorizationCache(brandCode);
        this.domainEventPublisherService.publishBrandAuthHistoryEvent(brandCode, sellerCode,
          generateBrandAuthHistoryForDelete(sellerCode, brandCode, brandStatus));
        log.info(
          "Delete Brand Auth Event published and Cache Evicted for Brand-Seller code : {} , ",
          brandCode + HYPHEN + sellerCode);
      } else {
        log.error("Error, No mapping found for Brand-Seller code : {} in Brand Auth Table",
          brandCode + HYPHEN + sellerCode);
        return "Seller Code: ".concat(sellerCode).concat(SPACE).concat("Brand Code: ")
          .concat(brandCode);
      }
      return StringUtils.EMPTY;
    }
    return StringUtils.EMPTY;
  }

  @Override
  public List<BrandAuthBulkDownloadResponse> getBrandAuthBulkResponse(String storeId, List<String> ids) {
    checkArgument(CollectionUtils.isNotEmpty(ids),
      String.valueOf(ErrorMessage.BULK_DOWNLOAD_IDS_MUST_NOT_BE_EMPTY));
    List<BrandAuthBulkDownloadResponse> bulkDownloadResponseList;
    List<BrandAuthorisation> brandList =
      authorisationRepository.findByStoreIdAndIdInAndMarkForDeleteFalse(storeId,
      new HashSet<>(ids));
    if(CollectionUtils.isNotEmpty(brandList)){
      bulkDownloadResponseList = ConverterUtil.convertToBrandAuthBulkResponse(brandList);
      return bulkDownloadResponseList;
    }
    return new ArrayList<>();
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public BrandAuthorisation editBrandAuthDetails(String storeId, BrandAuthorisation savedAuthData,
    BrandAuthUpdateRequest updateRequest) throws Exception {
    BrandAuthorisation finalAuthData;
    if(Objects.nonNull(savedAuthData)) {
      finalAuthData = processEditRequest(savedAuthData, updateRequest);
      log.info("Final Data for BrandAuth Update for Seller : {} , was : {} ", finalAuthData.getBrandCode(),
        finalAuthData);
    } else {
      log.error(
        "Auth Update Failed, saved Data could not be found for Seller : {}, brand : {} ",
        updateRequest.getSellerCode(), updateRequest.getBrandCode());
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
        ErrorCategory.DATA_NOT_FOUND.getCode());
    }
    finalAuthData.setUpdatedDate(new Date());
    log.info("Saving updated Brand Auth details : {} , for seller : {} ", finalAuthData,
      finalAuthData.getBrandCode());
    return authorisationRepository.save(finalAuthData);
  }


  private BrandAuthorisation processEditRequest(BrandAuthorisation savedAuthData,
    BrandAuthUpdateRequest updateRequest) throws Exception {
    BrandAuthorisation finalBrandAuthData = new BrandAuthorisation();
    BeanUtils.copyProperties(savedAuthData, finalBrandAuthData, "updatedDate", "updatedBy");
    if (StringUtils.isNotBlank(updateRequest.getAuthorisationStatus())) {
      if(BrandAuthorisationStatus.UPCOMING.name().equals(updateRequest.getAuthorisationStatus())){
        finalBrandAuthData.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE);
      }
      else {
        finalBrandAuthData.setAuthorisationStatus(
            BrandAuthorisationStatus.valueOf(updateRequest.getAuthorisationStatus()));
      }
    }
    if (CollectionUtils.isNotEmpty(updateRequest.getDocumentLinks())) {
      Set<String> documentLinkSet = new HashSet<>();
      documentLinkSet.addAll(updateRequest.getDocumentLinks());
      if (StringUtils.isNotEmpty(savedAuthData.getDocumentLink())) {
        documentLinkSet.addAll(Arrays.asList(savedAuthData.getDocumentLink().split(Constants.COMMA)));
      }
      finalBrandAuthData.setDocumentLink(
          documentLinkSet.stream().map(String::valueOf).collect(Collectors.joining(Constants.COMMA)));
    }
    if (Objects.nonNull(updateRequest.getAuthStartDate()) || Objects.nonNull(
      updateRequest.getAuthExpireDate())) {
      processDateUpdate(updateRequest, finalBrandAuthData);
    }
    return finalBrandAuthData;
  }

  private static void processDateUpdate(BrandAuthUpdateRequest updateRequest,
    BrandAuthorisation finalBrandAuthData) {
    if (Optional.of(updateRequest).map(BrandAuthUpdateRequest::getAuthStartDate).isPresent()) {
      finalBrandAuthData.setAuthStartDate(updateRequest.getAuthStartDate());
    }
    if (Optional.of(updateRequest).map(BrandAuthUpdateRequest::getAuthExpireDate).isPresent()
      && !updateRequest.getAuthExpireDate().before(finalBrandAuthData.getAuthStartDate())) {
      finalBrandAuthData.setAuthExpireDate(updateRequest.getAuthExpireDate());
    }
  }

  private BrandAuthorisationService getBrandAuthorisationServiceBean() {
    return applicationContext.getBean(BrandAuthorisationService.class);
  }

  @Override
  public boolean takeDownProductBasedOnBrand(String storeId,
      ProductBrandValidationRequest productBrandValidationRequest) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId), String.valueOf(ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK));
    checkArgument(StringUtils.isNotBlank(productBrandValidationRequest.getSellerCode()),
        String.valueOf(ErrorMessage.SELLER_CODE_MUST_NOT_BE_BLANK));
    if (StringUtils.isBlank(productBrandValidationRequest.getRequestedBrandCode())
        && StringUtils.isNotBlank(productBrandValidationRequest.getProductCode())
        && fetchBrandCodeFromProduct) {
      productBrandValidationRequest.setRequestedBrandCode(
          brandService.getBrandCodeByProductCode(storeId, productBrandValidationRequest.getProductCode()));
    }
    checkArgument(StringUtils.isNotBlank(productBrandValidationRequest.getRequestedBrandCode()),
        String.valueOf(ErrorMessage.REQUESTED_BRAND_CODE_MUST_NOT_BE_BLANK));
    List<ProtectedBrandResponse> protectedBrandResponseList = brandService.getProtectedBrandList(storeId);
    ProtectedBrandResponse requestedBrandCode = protectedBrandResponseList.stream().filter(
        protectedBrandResponse -> protectedBrandResponse.getBrandCode()
            .equals(productBrandValidationRequest.getRequestedBrandCode())).findFirst().orElse(null);
    if (Objects.nonNull(requestedBrandCode) && checkBrandAuthBySellerCode(storeId,
        productBrandValidationRequest.getSellerCode(), productBrandValidationRequest.getRequestedBrandCode(), true)) {
      return false;
    } else if (StringUtils.isNotBlank(productBrandValidationRequest.getPredictedBrandName())) {
      ProtectedBrandResponse predictedProtectedBrandResponse = null;
      predictedProtectedBrandResponse = protectedBrandResponseList.stream().filter(
          protectedBrandResponse -> productBrandValidationRequest.getPredictedBrandName()
              .equals(protectedBrandResponse.getBrandName())).findFirst().orElse(null);
      if (Objects.nonNull(predictedProtectedBrandResponse)) {
        if (checkBrandAuthBySellerCode(storeId, productBrandValidationRequest.getSellerCode(),
            predictedProtectedBrandResponse.getBrandCode(), true)) {
          return false;
        } else {
          return true;
        }
      }
    }
    return false;
  }

  public void updateBrandNameByBrandCode(String oldBrandName, String newBrandName, String brandCode) {
    if (!StringUtils.equals(oldBrandName.trim(), newBrandName.trim())) {
      authorisationRepository.updateBrandNameByBrandCode(newBrandName, brandCode);
      applicationCacheServiceBean.evictBrandAuthorizationCache(brandCode);
    }
  }


  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public Pair<BrandAuthCreateResponse,BrandAuthorisation> create(BrandAuthCreateRequest brandAuthCreateRequest,
    String storeId, String username) throws Exception {
    validateCreateRequest(storeId, brandAuthCreateRequest);
    BrandResponse savedBrand =
      brandService.findByBrandCodeCached(storeId, brandAuthCreateRequest.getBrandCode());
    if (Objects.isNull(savedBrand)) {
      log.error("Brand not found {} ", brandAuthCreateRequest.getBrandCode());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ErrorMessage.BRAND_IS_NOT_VALID_OR_NOT_PROTECTED.getMessage());
    }
    List<BrandAuthorisation> brands = getBrandAuthorisationServiceBean()
      .findBrandAuthorisationBySellerCodeAndBrandCode(storeId,
        brandAuthCreateRequest.getBrandCode());
    if (CollectionUtils.isNotEmpty(brands)) {
      BrandAuthorisation brandAuthorisation = brands.stream().filter(
        brand -> StringUtils.equals(brand.getSellerCode(), brandAuthCreateRequest.getSellerCode()))
        .findFirst().orElse(null);
      if (brandAuthCreateRequest.isBulkAction() && Objects.nonNull(brandAuthorisation)) {
        return Pair.of(
          BrandAuthCreateResponse.builder().brandCode(brandAuthorisation.getBrandCode())
            .bulkAction(true).build(), brandAuthorisation);
      } else if (Objects.nonNull(brandAuthorisation)) {
        log.error("Brand auth already present for seller {} and brand code {} ",
          brandAuthCreateRequest.getSellerCode(), brandAuthCreateRequest.getBrandCode());
        throw new ApplicationRuntimeException(ErrorCategory.DATA_ACCESS,
          ErrorMessage.AUTH_ALREADY_PRESENT.getMessage());
      }
    }
    brandAuthCreateRequest.setBrandName(savedBrand.getBrandName());
    BrandAuthorisation brandAuthorisation = generateCreateBrandAuthRequest(brandAuthCreateRequest);
    brandAuthorisation.setStoreId(storeId);
    authorisationRepository.save(brandAuthorisation);
    return Pair.of(BrandAuthCreateResponse.builder().brandCode(brandAuthorisation.getBrandCode())
      .bulkAction(false).build(), brandAuthorisation);
  }

  private BrandAuthorisation generateCreateBrandAuthRequest(BrandAuthCreateRequest request) throws Exception {
    BrandAuthorisation brandAuthorisation = new BrandAuthorisation();
    BeanUtils.copyProperties(request, brandAuthorisation, "authorisationStatus", "documentLinks");
    if(BrandAuthorisationStatus.UPCOMING.name().equals(request.getAuthorisationStatus())){
      brandAuthorisation.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE);
    }
    else {
      brandAuthorisation.setAuthorisationStatus(
          BrandAuthorisationStatus.valueOf(request.getAuthorisationStatus()));
    }
    if (CollectionUtils.isNotEmpty(request.getDocumentLinks())) {
      brandAuthorisation.setDocumentLink(
          request.getDocumentLinks().stream().map(String::valueOf).collect(Collectors.joining(Constants.COMMA)));
    }
    return brandAuthorisation;
  }

  public void validateCreateRequest(String storeId,
    BrandAuthCreateRequest brandAuthorisationRequest) throws ApplicationException {
    checkArgument(StringUtils.isNotBlank(storeId),
      String.valueOf(ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK));
    checkArgument(StringUtils.isNotBlank(brandAuthorisationRequest.getSellerCode()),
      String.valueOf(ErrorMessage.SELLER_CODE_MUST_NOT_BE_BLANK));
    checkArgument(StringUtils.isNotBlank(brandAuthorisationRequest.getBrandCode()),
      String.valueOf(ErrorMessage.BRAND_CODE_MUST_NOT_BE_BLANK));
    if (Objects.isNull(brandAuthorisationRequest.getAuthStartDate())) {
      brandAuthorisationRequest.setAuthStartDate(new Date());
    }
    if (Objects.isNull(brandAuthorisationRequest.getAuthExpireDate())) {
      brandAuthorisationRequest.setAuthExpireDate(DateUtils.addYears(new Date(), numberOfYears));
    }
    if(brandAuthorisationRequest.getAuthExpireDate().before(brandAuthorisationRequest.getAuthStartDate())){
      throw new ApplicationException(ErrorCategory.VALIDATION,
        ErrorMessage.AUTH_END_DATE_MUST_NOT_BE_BEFORE_START_DATE.getMessage());
    }
    if (CollectionUtils.isNotEmpty(brandAuthorisationRequest.getDocumentLinks())) {
      checkArgument(!brandAuthorisationRequest.getDocumentLinks().stream().anyMatch(StringUtils::isEmpty),
          ErrorMessage.FILE_NAME_SHOULD_NOT_BE_NULL_OR_EMPTY.getMessage());
    }
  }

  private BrandAuthorisationHistory generateBrandAuthHistoryForDelete(String sellerCode,
    String brandCode, String status){
    BrandAuthorisationHistory brandAuthorisationHistory = new BrandAuthorisationHistory();
    brandAuthorisationHistory.setBrandCode(brandCode);
    brandAuthorisationHistory.setSellerCode(sellerCode);
    brandAuthorisationHistory.setUpdatedDate(new Date());
    brandAuthorisationHistory.setActivity(BrandAuthorisationActivity.DELETE.getDescription());
    brandAuthorisationHistory.setOldStatus(status);
    brandAuthorisationHistory.setStoreId(Constants.DEFAULT_STORE_ID);
    brandAuthorisationHistory.setUpdatedDate(new Date());
    brandAuthorisationHistory.setNewStatus(StringUtils.EMPTY);
    return brandAuthorisationHistory;
  }
}
