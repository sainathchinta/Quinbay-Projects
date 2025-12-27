package com.gdn.x.productcategorybase.service.brand;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;

import com.gdn.x.productcategorybase.CacheNames;
import com.gdn.x.productcategorybase.dto.BrandCreationDTO;
import com.gdn.x.productcategorybase.dto.BrandInReviewResponse;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.context.annotation.Lazy;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.brand.BrandApproveRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectionInfoResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandServiceWrapperResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistoryResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistorySummaryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipSummaryRequest;
import com.gdn.x.productcategorybase.entity.Attribute;
import com.gdn.x.productcategorybase.entity.PredefinedAllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.brand.Brand;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;
import com.gdn.x.productcategorybase.repository.SolrBrandRepository;
import com.gdn.x.productcategorybase.repository.brand.BrandRepository;
import com.gdn.x.productcategorybase.repository.brand.BrandWipRepository;
import com.gdn.x.productcategorybase.repository.sequence.SequenceRepository;
import com.gdn.x.productcategorybase.service.AttributeService;
import com.gdn.x.productcategorybase.service.BusinessPartner.BusinessPartnerService;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.PredefinedAllowedAttributeValueService;
import com.gdn.x.productcategorybase.service.SequenceService;
import com.gdn.x.productcategorybase.service.impl.ApplicationCacheServiceBean;
import com.gdn.x.productcategorybase.util.ConverterUtil;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

@Service
@Transactional(readOnly = true)
public class BrandWipServiceBean implements BrandWipService {

  private static final String PREFIX_BRAND_REQUEST_CODE = "BR";
  private static final String LOGO_EXT = "-logo.";
  private static final String BANNER_EXT = "-banner.";
  private static final String BRAND = "Brand";
  private static final String HYPHEN = "-";
  private static final String STORE_ID_IS_EMPTY = "StoreId is empty";
  private static final String BRAND_CODE_IS_EMPTY = "BrandCode is empty";
  private static final String INTERNAL_USER = "INTERNAL";
  private static final String BRAND_NAME = "brandName";
  private static final String BRAND_DESCRIPTION = "brandDescription";
  private static final String SHOW_BRAND = "showBrand";
  private static final String BRAND_LOGO = "brandLogo";
  private static final String PROTECTED_BRAND = "protectedBrand";
  private static final String PROFILE_BANNER = "profileBanner";
  private static final String OLD_VALUE = "o: ";
  private static final String NEW_VALUE = "n: ";
  private static final String COMMA = ", ";
  private static final String DRAFT = "DRAFT";

  @Autowired
  private BrandWipRepository brandWipRepository;

  @Autowired
  private SequenceService sequenceService;

  @Autowired
  @Lazy
  private PredefinedAllowedAttributeValueService predefinedAllowedAttributeValueService;

  @Autowired
  private AttributeService attributeService;

  @Autowired
  private BrandWipHistoryService brandWipHistoryService;

  @Autowired
  private SolrBrandRepository solrBrandRepository;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private BusinessPartnerService businessPartnerService;

  @Autowired
  private BrandRepository brandRepository;

  @Autowired
  private SequenceRepository sequenceRepository;

  @Autowired
  private DomainEventPublisherService domainEventPublisherService;

  @Autowired
  @Lazy
  private BrandService brandService;

  @Autowired
  @Lazy
  private BrandAuthorisationService brandAuthorisationService;

  @Autowired
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public BrandCreationDTO create(String storeId, BrandWip brandWip)
      throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId), "Store ID is empty");
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(brandWip.getBusinessPartnerName()),
        "Business Partner name is empty");
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(brandWip.getBusinessPartnerCode()),
        "Business Partner code is empty");

    brandWip.setStoreId(storeId);
    brandWip.setBrandRequestCode(this.generateBrandRequestCode());
    brandWip.setState(BrandWipState.DRAFT);
    String username = GdnMandatoryParameterUtil.getUsername();
    Date date = Calendar.getInstance().getTime();
    if (StringUtils.isEmpty(brandWip.getId())) {
      brandWip.setCreatedBy(username);
      brandWip.setCreatedDate(date);
    }
    List<Attribute> attributeList = attributeService.findByName(brandWip.getStoreId(), BRAND);
    if (CollectionUtils.isEmpty(attributeList)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "Predefined Attribute for 'Brand' not found'");
    }
    Attribute attribute = attributeList.get(0);
    PredefinedAllowedAttributeValue predefinedAllowedAttributeValue =
        predefinedAllowedAttributeValueService.generatePredefinedAllowedAttributeValue(brandWip, attribute);
    this.predefinedAllowedAttributeValueService.save(predefinedAllowedAttributeValue);
    this.brandWipRepository.saveAndFlush(brandWip);
    return BrandCreationDTO.builder().brandWip(brandWip).attribute(attribute).build();
  }

  @Override
  public BrandWipResponse getBrandWipDetail(String storeId, String brandRequestCode) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(storeId), "Store ID is empty");
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(brandRequestCode), "Brand Request Code is empty");
    BrandWip brandWip = this.brandWipRepository.findByStoreIdAndBrandRequestCode(storeId, brandRequestCode);
    if(Objects.isNull(brandWip)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, "No such Brand Wip found");
    }
    return ConverterUtil.generateBrandWipResponseFromBrandWip(brandWip);
  }

  @Override
  public BrandRejectionInfoResponse getBrandRejectionInfoResponse(String storeId, String brandRequestCode) {
    BrandWip brandWip = this.brandWipRepository
        .findByStoreIdAndBrandRequestCodeAndState(storeId, brandRequestCode, BrandWipState.REJECTED);
    if (Objects.isNull(brandWip)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          ErrorMessage.BRAND_WIP_NOT_FOUND.getMessage());
    }
    return ConverterUtil.generateBrandRejectionInfoResponse(brandWip);
  }

  @Override
  public void deleteBrandWip(BrandWip brandWip) {
    brandWip.setMarkForDelete(true);
    brandWip.setState(BrandWipState.DELETED);
    brandWipRepository.save(brandWip);
  }

  @Override
  public BrandWip getBrandWipByStoreIdAndBrandCode(String storeId, String brandCode) {
    GdnPreconditions.checkArgument(StringUtils.isNoneBlank(storeId), STORE_ID_IS_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNoneBlank(brandCode), BRAND_CODE_IS_EMPTY);
    return brandWipRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(storeId, brandCode);
  }

  @Override
  public Page<BrandWipHistoryResponse> getBrandWipHistory(BrandWipHistorySummaryRequest brandWipHistorySummaryRequest,
      int page, int size) {
    Pageable pageable = PageRequest.of(page, size);
    Page<BrandWipHistoryResponse> brandWipHistoryResponsePage;
    if (StringUtils.isNotEmpty(brandWipHistorySummaryRequest.getBrandRequestCode())) {
      brandWipHistoryResponsePage =
          brandWipHistoryService.findByBrandRequestCode(brandWipHistorySummaryRequest.getBrandRequestCode(), pageable);
    } else if (StringUtils.isNotEmpty(brandWipHistorySummaryRequest.getBrandCode())) {
      brandWipHistoryResponsePage =
          brandWipHistoryService.findByBrandCode(brandWipHistorySummaryRequest.getBrandCode(), pageable);
    } else {
      throw new ApplicationRuntimeException(ErrorCategory.REQUIRED_PARAMETER,
          "Both brand code and brand request code is empty");
    }
    if (Objects.isNull(brandWipHistoryResponsePage)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, "No such brand wip exists");
    }
    return brandWipHistoryResponsePage;
  }

  @Override
  public BrandWipResponse filterByBrandRequestCodeIrrespectiveOfState(String storeId, String brandRequestCode) throws Exception {
    BrandWip brandWip = this.brandWipRepository.findByStoreIdAndBrandRequestCode(storeId, brandRequestCode);
    return ConverterUtil.generateBrandWipResponseFromBrandWip(brandWip);
  }

  @Override
  public Page<BrandWipResponse> getBrandWipList(BrandWipSummaryRequest brandWipSummaryRequest, int page,
      int size) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(brandWipSummaryRequest.getState()), "State is empty");
    BrandWipState state = BrandWipState.valueOf(brandWipSummaryRequest.getState());
    Pageable pageable = PageRequest.of(page, size);
    Page<BrandWip> brandWipPage;
    if (StringUtils.isEmpty(brandWipSummaryRequest.getBrandName())) {
      brandWipPage = this.brandWipRepository.findByStateAndMarkForDeleteFalse(state, pageable);
    } else {
      brandWipPage = this.brandWipRepository
          .findByBrandNameAndStateAndMarkForDeleteFalse(brandWipSummaryRequest.getBrandName(), state, pageable);
    }
    if(Objects.isNull(brandWipPage)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, "No brand wips found");
    }
    return new PageImpl<>(ConverterUtil.generateBrandWipResponses(brandWipPage), pageable,
        brandWipPage.getTotalElements());
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public BrandWip update(String storeId, BrandApproveRequest brandApproveRequest)  throws Exception {
    BrandWip savedBrandWip =
        this.brandWipRepository.findByStoreIdAndBrandRequestCode(storeId, brandApproveRequest.getBrandRequestCode());
    if(Objects.isNull(savedBrandWip)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "Brand with brand request code " + brandApproveRequest.getBrandRequestCode() + " is not found");
    }
    String reason = getBrandHistoryChanges(savedBrandWip, brandApproveRequest);
    boolean clearBrandAuthorizationCache = savedBrandWip.isProtectedBrand() != brandApproveRequest.isProtectedBrand();
    generateBrandWip(savedBrandWip, brandApproveRequest);
    String brandName = savedBrandWip.getBrandName();
    this.brandWipRepository.save(savedBrandWip);
    brandAuthorisationService.updateBrandNameByBrandCode(brandName,
        brandApproveRequest.getBrandName(), brandApproveRequest.getBrandRequestCode());
    this.brandWipHistoryService.generateBrandWipHistory(savedBrandWip, reason,
        GdnMandatoryParameterUtil.getUsername());
    if (clearBrandAuthorizationCache) {
      applicationCacheServiceBean.evictBrandAuthorizationCache(savedBrandWip.getBrandRequestCode());
      applicationCacheServiceBean.evictProtectedBrandCache(storeId);
    }
    return savedBrandWip;
  }

  private String generateBrandRequestCode() throws Exception {
    Date date = Calendar.getInstance().getTime();
    SimpleDateFormat prefixDateFormatter = new SimpleDateFormat(Constants.PREFIX_DATE_FORMATTER);
    String prefix =
        new StringBuilder().append(PREFIX_BRAND_REQUEST_CODE).append(HYPHEN).append(prefixDateFormatter.format(date))
            .toString();
    return new StringBuilder().append(prefix).append(HYPHEN)
        .append(StringUtils.leftPad(String.valueOf(this.sequenceService.findByCode(prefix)), 5, '0')).toString();
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public BrandServiceWrapperResponse approveBrand(BrandApproveRequest brandApproveRequest) throws Exception {
    Brand brand;
    BrandServiceWrapperResponse brandServiceWrapperResponse = new BrandServiceWrapperResponse();
    checkIfApprovedBrandAlreadyExists(brandApproveRequest);
    BrandWip brandWip = brandWipRepository
        .findFirstByStoreIdAndBrandRequestCodeAndMarkForDeleteFalseAndState(GdnMandatoryParameterUtil.getStoreId(),
            brandApproveRequest.getBrandRequestCode(), BrandWipState.DRAFT);
    if (Objects.isNull(brandWip)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "Brand with brand name " + brandApproveRequest.getBrandName() + " is not found");
    }
    String brandHistoryChanges = getBrandHistoryChanges(brandWip, brandApproveRequest);
    brandWip.setState(BrandWipState.APPROVED);
    boolean clearBrandAuthorizationCache = brandWip.isProtectedBrand() != brandApproveRequest.isProtectedBrand();
    generateBrandWip(brandWip, brandApproveRequest);
    Brand savedBrand = brandRepository
        .findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(GdnMandatoryParameterUtil.getStoreId(),
            brandWip.getBrandName(), true);
    if (Objects.nonNull(savedBrand)) {
      brand = brandService.undelete(ConverterUtil.brandWipToBrand(brandWip));
      brandWip.setMarkForDelete(false);
      brandWip.setBrandCode(brand.getBrandCode());
      brandServiceWrapperResponse.setUnDeleteBrand(true);
    } else {
      brand = createBrand(ConverterUtil.brandWipToBrand(brandWip), brandApproveRequest.getBrandRequestCode());
      brandWip.setBrandCode(brand.getBrandCode());
    }
    brandServiceWrapperResponse.setBrand(brand);
    brandServiceWrapperResponse.setBrandWip(brandWip);
    brandWipRepository.save(brandWip);
    brandAuthorisationService.updateBrandNameByBrandCode(brandWip.getBrandName(),
        brandApproveRequest.getBrandName(), brandApproveRequest.getBrandRequestCode());
    if (clearBrandAuthorizationCache) {
      applicationCacheServiceBean.evictBrandAuthorizationCache(brandWip.getBrandRequestCode());
      applicationCacheServiceBean.evictProtectedBrandCache(brandWip.getStoreId());
    }
    domainEventPublisherService.publishBrandApprovedOrRejectedDomainEventModel(
        ConverterUtil.generateBrandApprovedOrRejectedDomainEventModel(brandWip));
    brandWipHistoryService
        .generateBrandWipHistory(brandWip, brandHistoryChanges, GdnMandatoryParameterUtil.getUsername());
    if (!INTERNAL_USER.equals(brandWip.getBusinessPartnerCode())) {
      businessPartnerService.createBrandStatusChangeNotification(brandWip);
    }
    return brandServiceWrapperResponse;
  }

  /**
   * Check if brand with same name is already approved
   *
   * @param brandApproveRequest
   */
  private void checkIfApprovedBrandAlreadyExists(BrandApproveRequest brandApproveRequest) {
    BrandWip existingBrand = brandWipRepository
        .findByBrandRequestCodeAndStateAndMarkForDeleteFalse(brandApproveRequest.getBrandRequestCode(),
            BrandWipState.APPROVED);
    if (Objects.nonNull(existingBrand)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_ACCESS,
          "Brand with brand name " + brandApproveRequest.getBrandName() + " already exists");
    }
    existingBrand = brandWipRepository
        .findByBrandNameIgnoreCaseAndStateAndMarkForDeleteFalse(brandApproveRequest.getBrandName(),
            BrandWipState.APPROVED);
    if (Objects.nonNull(existingBrand)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_ACCESS,
          "Cannot approve, a Brand with same name " + brandApproveRequest.getBrandName()
              + ErrorMessage.BRAND_ALREADY_APPROVED.getMessage());
    }
  }

  /**
   * Create a brand without making entry in PreDefinedAllowedAttributeValue
   *
   * @param brand
   * @param brandRequestCode
   * @return
   * @throws Exception
   */
  private Brand createBrand(Brand brand, String brandRequestCode) throws Exception {
    String storeId = GdnMandatoryParameterUtil.getStoreId();
    Brand savedBrand =
        this.brandRepository.findByStoreIdAndBrandNameIgnoreCaseAndMarkForDelete(storeId, brand.getBrandName(), false);
    if (Objects.nonNull(savedBrand)) {
      throw new ApplicationException(ErrorCategory.DATA_ACCESS,
          "Brand with name " + brand.getBrandName() + " already exist");
    }
    brand.setStoreId(storeId);
    brand.setBrandCode(this.generateBrandCode());
    this.brandRepository.save(brand);
    predefinedAllowedAttributeValueService
        .updatePredefinedAllowedAttributeCodeForApprovedBrand(storeId, brandRequestCode, brand);
    domainEventPublisherService.publishBrandUpdated(brand);
    return brand;
  }

  /**
   * To generate notes for brand history
   *
   * @param oldBrand
   * @param newBrand
   * @return
   * @throws JsonProcessingException
   */
  private String getBrandHistoryChanges(BrandWip oldBrand, BrandApproveRequest newBrand) throws JsonProcessingException {
    Map<String, String> brandDiffs = new HashMap<>();
    if (!StringUtils.equals(oldBrand.getBrandName().trim(), newBrand.getBrandName().trim())) {
      brandDiffs.put(BRAND_NAME,
          new StringBuilder(OLD_VALUE).append(oldBrand.getBrandName().trim()).append(COMMA).append(NEW_VALUE)
              .append(newBrand.getBrandName().trim()).toString());
    }
    if (ArrayUtils.isEmpty(oldBrand.getBrandDescription())) {
      if (StringUtils.isNotEmpty(newBrand.getBrandDescription())) {
        brandDiffs.put(BRAND_DESCRIPTION,
            new StringBuilder(OLD_VALUE).append(COMMA).append(NEW_VALUE).append(newBrand.getBrandDescription().trim())
                .toString());
      }
    } else {
      if (!StringUtils.equals(new String(oldBrand.getBrandDescription()).trim(), newBrand.getBrandDescription().trim())) {
        brandDiffs.put(BRAND_DESCRIPTION,
            OLD_VALUE + new StringBuilder(new String(oldBrand.getBrandDescription()).trim()).append(COMMA)
                .append(NEW_VALUE).append(newBrand.getBrandDescription().trim()).toString());
      }
    }
    if (Objects.nonNull(newBrand.getBrandLogoPath())) {
      brandDiffs.put(BRAND_LOGO, new StringBuilder(NEW_VALUE).append(newBrand.getBrandLogoPath()).toString());
    }
    if (Objects.nonNull(newBrand.getProfileBannerPath())) {
      brandDiffs.put(PROFILE_BANNER, new StringBuilder(NEW_VALUE).append(newBrand.getProfileBannerPath()).toString());
    }
    if (oldBrand.isValidBrand() != newBrand.isValidBrand()) {
      brandDiffs.put(SHOW_BRAND,
          OLD_VALUE + new StringBuilder(String.valueOf(oldBrand.isValidBrand())).append(COMMA).append(NEW_VALUE)
              .append(String.valueOf(newBrand.isValidBrand())));
    }
    if (oldBrand.isProtectedBrand() != newBrand.isProtectedBrand()) {
      brandDiffs.put(PROTECTED_BRAND,
          OLD_VALUE + new StringBuilder(String.valueOf(oldBrand.isProtectedBrand())).append(COMMA).append(NEW_VALUE)
              .append(newBrand.isProtectedBrand()));
    }
    if (oldBrand.isSkuCreationAllowedForAllSellers() != newBrand.isSkuCreationAllowedForAllSellers()) {
      brandDiffs.put(Constants.SKU_CREATION_ALLOWED_FOR_ALL_SELLERS,
          OLD_VALUE + new StringBuilder(String.valueOf(oldBrand.isSkuCreationAllowedForAllSellers())).append(COMMA)
              .append(NEW_VALUE).append(newBrand.isSkuCreationAllowedForAllSellers()));
    }
    return this.objectMapper.writeValueAsString(brandDiffs);
  }

  private String generateBrandCode() {
    return BrandService.PREFIX_BRAND_CODE + HYPHEN + StringUtils
        .leftPad(String.valueOf(this.sequenceRepository.findByCode(BrandService.PREFIX_BRAND_CODE)), 5, '0');
  }

  /**
   * generate brand wip
   *
   * @param brandWip
   * @param brandApproveRequest
   */
  private void generateBrandWip(BrandWip brandWip, BrandApproveRequest brandApproveRequest) {
    if (StringUtils.isNotBlank(brandApproveRequest.getBrandName())) {
      brandWip.setBrandName(brandApproveRequest.getBrandName());
    }
    if (StringUtils.isNotBlank(brandApproveRequest.getBrandDescription())) {
      brandWip.setBrandDescription(brandApproveRequest.getBrandDescription().getBytes());
    }
    if (StringUtils.isNotBlank(brandApproveRequest.getBrandLogoPath())) {
      brandWip.setBrandLogoPath(brandApproveRequest.getBrandLogoPath());
    }
    if (StringUtils.isNotBlank(brandApproveRequest.getProfileBannerPath())) {
      brandWip.setProfileBannerPath(brandApproveRequest.getProfileBannerPath());
    }
    brandWip.setValidBrand(brandApproveRequest.isValidBrand());
    brandWip.setProtectedBrand(brandApproveRequest.isProtectedBrand());
    brandWip.setSkuCreationAllowedForAllSellers(brandApproveRequest.isSkuCreationAllowedForAllSellers());
  }

  @Override
  @Transactional(readOnly = false, rollbackFor = Exception.class)
  public BrandWipResponse rejectBrand(BrandRejectRequest brandRejectRequest) throws Exception {
    String storeId = GdnMandatoryParameterUtil.getStoreId();
    BrandWip brandWip = brandWipRepository.findFirstByStoreIdAndBrandRequestCodeAndMarkForDeleteFalseAndState(storeId,
        brandRejectRequest.getBrandRequestCode(), BrandWipState.DRAFT);
    if (Objects.isNull(brandWip)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "Brand with brand request code " + brandRejectRequest.getBrandRequestCode() + " is not found");
    }
    rejectBrandAndPublish(storeId, brandRejectRequest, brandWip);
    if (!INTERNAL_USER.equals(brandWip.getBusinessPartnerCode())) {
      businessPartnerService.createBrandStatusChangeNotification(brandWip);
    }
    BrandWipResponse brandWipResponse = ConverterUtil.generateBrandWipResponseFromBrandWip(brandWip);
    brandWipResponse.setBrandCode(brandWip.getBrandRequestCode());
    return brandWipResponse;
  }

  @Override
  public BrandWipResponse getBrandWipDetailByBrandCode(String storeId, String brandCode) {
    BrandWip brandWip = this.getBrandWipByStoreIdAndBrandCode(storeId, brandCode);
    Optional.ofNullable(brandWip)
        .orElseThrow(() -> new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, "No such Brand Wip found"));
    return ConverterUtil.generateBrandWipResponseFromBrandWip(brandWip);
  }

  @Override
  public BrandWipResponse findByBrandNameAndBusinessPartnerCode(String brandName, String businessPartnerCode)
      throws Exception {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(brandName), "Brand Name cannot be empty");
    GdnPreconditions
        .checkArgument(StringUtils.isNotEmpty(businessPartnerCode), "Business partner code cannot be empty");
    BrandWip brandWip = this.brandWipRepository
        .findByBrandNameAndBusinessPartnerCodeAndStateAndMarkForDeleteFalse(brandName, businessPartnerCode,
            BrandWipState.DRAFT);
    if(Objects.isNull(brandWip)) {
      return null;
    }
    return ConverterUtil.generateBrandWipResponseFromBrandWip(brandWip);
  }

  @Override
  public BrandWipResponse filterByBrandRequestCode(String brandRequestCode) throws Exception {
    BrandWip brandWip = this.brandWipRepository.findByBrandRequestCodeAndMarkForDeleteFalse(brandRequestCode);
    return ConverterUtil.generateBrandWipResponseFromBrandWip(brandWip);
  }

  /**
   * Change brandWip state and status in predefined allowed attribute value
   *
   * @param storeId
   * @param brandRejectRequest
   * @param brandWip
   * @throws Exception
   */
  private void rejectBrandAndPublish(String storeId, BrandRejectRequest brandRejectRequest, BrandWip brandWip)
      throws Exception {
    brandWip.setState(BrandWipState.REJECTED);
    if (StringUtils.isNotBlank(brandRejectRequest.getNotes())) {
      brandWip.setNotes(brandRejectRequest.getNotes().getBytes());
    }
    predefinedAllowedAttributeValueService
        .updatePredefinedAllowedAttributeCodeForRejectedBrand(storeId, brandRejectRequest.getBrandRequestCode());
    brandWipRepository.save(brandWip);
    domainEventPublisherService.publishBrandApprovedOrRejectedDomainEventModel(
        ConverterUtil.generateBrandApprovedOrRejectedDomainEventModel(brandWip));
    brandWipHistoryService.generateBrandWipHistory(brandWip, brandRejectRequest.getNotes(),
        GdnMandatoryParameterUtil.getUsername());
  }

  @Override
  public BrandResponse getBrandByNameFromBrandWip(String storeId, String brandName) {
    BrandWip brandWip =
        this.brandWipRepository.findTop1ByStoreIdAndBrandNameIgnoreCaseOrderByCreatedDateAsc(storeId, brandName);
    if(Objects.nonNull(brandWip)) {
      return ConverterUtil.getBrandResponseByBrandWip(brandWip);
    }
    return null;
  }

  @Override
  @Transactional(readOnly = false, propagation = Propagation.REQUIRES_NEW)
  public void updateValidBrandFlag(String updatedBy, String brandCode, boolean validBrand) {
    brandWipRepository.updateValidBrandFlagByBrandCode(updatedBy, validBrand, brandCode);
  }

  @Override
  @Cacheable(value = CacheNames.IN_REVIEW_BRANDS, key = "#storeId", unless = "#result == null")
  public List<BrandInReviewResponse> getAllInReviewBrands(String storeId) {
    checkArgument(StringUtils.isNotBlank(storeId),
      String.valueOf(ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK));
    List<BrandWip> brandWipList =
      brandWipRepository.findByStoreIdAndStateAndMarkForDeleteFalse(storeId, BrandWipState.DRAFT);
    return ConverterUtil.generateBrandInReviewResponses(brandWipList);
  }

  @Override
  public BrandWip updateBrandName(String storeId, String brandCode, String brandName) {
    BrandWip existingBrandWip =
        brandWipRepository.findByStoreIdAndBrandCodeAndMarkForDeleteFalse(storeId, brandCode);
    checkArgument(Objects.nonNull(existingBrandWip), ErrorMessage.BRAND_WIP_NOT_FOUND.getMessage());
    existingBrandWip.setBrandName(brandName);
    return brandWipRepository.save(existingBrandWip);
  }
}
