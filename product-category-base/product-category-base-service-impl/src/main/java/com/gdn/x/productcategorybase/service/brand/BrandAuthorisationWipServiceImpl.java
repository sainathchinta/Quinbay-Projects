package com.gdn.x.productcategorybase.service.brand;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.BeanUtils;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.x.productcategorybase.BrandAuthorisationActivity;
import com.gdn.x.productcategorybase.BrandAuthorisationStatus;
import com.gdn.x.productcategorybase.BrandAuthorisationWipAction;
import com.gdn.x.productcategorybase.CacheNames;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.config.KafkaPublisher;
import com.gdn.x.productcategorybase.domain.event.model.BrandAuthActivateEventModel;
import com.gdn.x.productcategorybase.dto.BrandAuthorisationStatusIdDTO;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthorisationWipListResponse;
import com.gdn.x.productcategorybase.dto.request.BrandAuthorisationWipListRequest;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisation;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import com.gdn.x.productcategorybase.repository.brand.BrandAuthorisationRepository;
import com.gdn.x.productcategorybase.BrandAuthorizationWipStatus;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateWipRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateWipResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthWipDetailResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.BrandAuthorisationWipActionRequest;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationWip;
import com.gdn.x.productcategorybase.repository.brand.BrandAuthorisationWipRepository;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import com.gdn.x.productcategorybase.util.ValidationUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.annotation.CacheEvict;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import com.gdn.x.productcategorybase.util.CommonUtil;

import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;

import static com.gdn.common.base.GdnPreconditions.checkArgument;

@Service
@Slf4j
@RequiredArgsConstructor
public class BrandAuthorisationWipServiceImpl implements BrandAuthorisationWipService {

  private final BrandAuthorisationWipRepository brandAuthorisationWipRepository;
  private final BrandAuthorisationRepository brandAuthorisationRepository;
  private final BrandService brandService;
  private final KafkaTopicProperties kafkaTopicProperties;
  private final KafkaPublisher kafkaPublisher;

  @Value("${brand.auth.end.date.years.add}")
  private int numberOfYears;

  @Value("${brand.auth.wip.threshold.in.days}")
  private int brandAuthWipThresholdInDays;

  @Value("${brand.auth.near.expiry.days.threshold}")
  private Integer brandAuthNearExpiryDaysThreshold;

  @Value("${brand.auth.near.expiry.mail.config.days}")
  private String configDay;

  @Override
  public void submitBrandAuthorisationRequest(String storeId,
    BrandAuthUpdateRequest brandAuthUpdateRequest) throws Exception {
    ValidationUtil.validateBrandAuthorisationRequest(storeId, brandAuthUpdateRequest);
    BrandAuthorisationWip brandAuthorisationWipDb = null;
    ValidationUtil.checkParameter(!BrandAuthorizationWipStatus.REJECTED.name()
            .equals(brandAuthUpdateRequest.getAuthorisationStatus()),
        ErrorMessage.REJECTED_BRAND_AUTH_REQUEST_CANNOT_BE_EDITED.getMessage());
    if (BrandAuthorisationStatus.ACTIVE.name().equals(brandAuthUpdateRequest.getAuthorisationStatus())) {
      BrandAuthorisation brandAuthorisation = new BrandAuthorisation();
      brandAuthorisation = Optional.ofNullable(
          brandAuthorisationRepository.findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
              storeId, brandAuthUpdateRequest.getBrandCode(),
              brandAuthUpdateRequest.getSellerCode())).orElse(brandAuthorisation);
      brandAuthorisationWipDb = Optional.ofNullable(brandAuthorisationWipRepository
          .findByStoreIdAndBrandCodeAndSellerCodeAndAuthorisationStatus(storeId,
              brandAuthorisation.getBrandCode(), brandAuthorisation.getSellerCode(), BrandAuthorizationWipStatus.IN_REVIEW))
          .orElse(new BrandAuthorisationWip());
        BeanUtils.copyProperties(brandAuthorisation, brandAuthorisationWipDb);
        brandAuthorisationWipDb.setAuthorisationStatus(
            BrandAuthorizationWipStatus.valueOf(brandAuthorisation.getAuthorisationStatus().name()));
    } else {
      brandAuthorisationWipDb =
          brandAuthorisationWipRepository.findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
              brandAuthUpdateRequest.getBrandCode(), brandAuthUpdateRequest.getSellerCode(),
              BrandAuthorizationWipStatus.valueOf(brandAuthUpdateRequest.getAuthorisationStatus()));
    }

   brandAuthorisationWipDb =  ConverterUtil.toUpdatedBrandAuthorisationWipEntity(brandAuthUpdateRequest,
      brandAuthorisationWipDb);
    brandAuthorisationWipRepository.save(brandAuthorisationWipDb);
  }

  @Override
  public BrandAuthWipDetailResponse fetchBrandAuthWipDetails(String storeId, String status,
      String id) {
    log.info("Fetching brand wip detail with id {}", id);
    checkArgument(StringUtils.isNotBlank(storeId),
        String.valueOf(ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK));
    BrandAuthorisationWip brandAuthorisationWip;
    Date date = new Date();
    Date currentAuthStartDate = null;
    Date currentAuthEndDate = null;
    if (BrandAuthorizationWipStatus.ACTIVE.name().equals(status)) {
     brandAuthorisationWip =
          brandAuthorisationRepository.findById(id)
              .map(brandAuthorisation -> {
                BrandAuthorisationWip authorisationWip = new BrandAuthorisationWip();
                BeanUtils.copyProperties(brandAuthorisation, authorisationWip);
                authorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.valueOf(
                    brandAuthorisation.getAuthorisationStatus().name()));
                return authorisationWip;
              })
              .orElse(null);
    } else {
      brandAuthorisationWip = brandAuthorisationWipRepository.findById(id).orElse(null);
      if (Objects.nonNull(brandAuthorisationWip)) {
        BrandAuthorisation brandAuthorisation =
            brandAuthorisationRepository.findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
                brandAuthorisationWip.getStoreId(), brandAuthorisationWip.getBrandCode(),
                brandAuthorisationWip.getSellerCode());
        if (Objects.nonNull(brandAuthorisation)) {
          if (brandAuthorisation.getAuthStartDate().before(date)
              && brandAuthorisation.getAuthExpireDate().after(date)) {
            currentAuthStartDate = brandAuthorisation.getAuthStartDate();
            currentAuthEndDate = brandAuthorisation.getAuthExpireDate();
          }
        }
      }
    }
    ValidationUtil.checkParameter(Objects.nonNull(brandAuthorisationWip),
        String.valueOf(ErrorMessage.BRAND_AUTH_WIP_NOT_FOUND));
    return ConverterUtil.convertToBrandAuthWipDetailResponse(brandAuthorisationWip,
        currentAuthStartDate, currentAuthEndDate);
  }

  @Override
  @Transactional
  @CacheEvict(value = CacheNames.BRAND_AUTHORISATION_WIP, key = "#storeId +'_'+ "
    + "#brandAuthorisationWipActionRequest.sellerCode")
  public Pair<List<BrandAuthorisationHistory>, BrandAuthorisationStatusIdDTO> brandAuthorisationWipAction(String storeId,
    String username, BrandAuthorisationWipActionRequest brandAuthorisationWipActionRequest) {
    ValidationUtil.checkParameter(StringUtils.isNotBlank(
        BrandAuthorisationWipAction.getValueOrEmpty(brandAuthorisationWipActionRequest.getAction())),
      ErrorCategory.VALIDATION, ErrorMessage.ACTION_CANNOT_BE_PERFORMED);
    BrandAuthorisationWip brandAuthorisationWip =
      findBrandAuthorisationWip(storeId, brandAuthorisationWipActionRequest);
    return switch (BrandAuthorisationWipAction.valueOf(
      brandAuthorisationWipActionRequest.getAction())) {
      case APPROVE ->
        approveBrandAuthorisationWip(brandAuthorisationWip, brandAuthorisationWipActionRequest);
      case REJECT -> performRejectionOrNeedRevisionForBrandAuthWip(brandAuthorisationWip,
        brandAuthorisationWipActionRequest.getReason(), BrandAuthorizationWipStatus.REJECTED);
      case NEED_REVISION -> performRejectionOrNeedRevisionForBrandAuthWip(brandAuthorisationWip,
        brandAuthorisationWipActionRequest.getReason(), BrandAuthorizationWipStatus.NEED_REVISION);
    };
  }

  private BrandAuthorisation findBrandAuthorisation(String storedId,
      BrandAuthorisationWipActionRequest request) {
    return brandAuthorisationRepository.findFirstByStoreIdAndBrandCodeAndSellerCode(storedId,
        request.getBrandCode(), request.getSellerCode());
  }


  @Override
  @Transactional(rollbackFor = Exception.class)
  @CacheEvict(value = CacheNames.BRAND_AUTHORISATION_WIP, key = "#storeId +'_'+ "
      + "#brandAuthCreateRequest.sellerCode")
  public Pair<BrandAuthCreateWipResponse, BrandAuthorisationWip> brandAuthCreateWipRequest(
      BrandAuthCreateWipRequest brandAuthCreateRequest, String storeId, String username)
      throws Exception {
    BrandResponse savedBrand =
        this.brandService.findByBrandCodeCached(storeId, brandAuthCreateRequest.getBrandCode());
    ValidationUtil.validateCreateBrandAuthWipRequest(storeId, brandAuthCreateRequest,
        numberOfYears, savedBrand, false);
    brandAuthCreateRequest.setBrandName(savedBrand.getBrandName());
    BrandAuthorisationWip brandAuthorisation =
        CommonUtil.generateCreateBrandAuthWipRequest(brandAuthCreateRequest, storeId);
    brandAuthorisationWipRepository.save(brandAuthorisation);
    return Pair.of(
        BrandAuthCreateWipResponse.builder().brandCode(brandAuthorisation.getBrandCode()).build(),
        brandAuthorisation);
  }

  private BrandAuthorisationWip findBrandAuthorisationWip(String storedId,
      BrandAuthorisationWipActionRequest request) {
    BrandAuthorisationWip brandAuthorisationWip =
      brandAuthorisationWipRepository.findByStoreIdAndBrandCodeAndSellerCodeAndAuthorisationStatus(
        storedId, request.getBrandCode(), request.getSellerCode(),
        BrandAuthorizationWipStatus.IN_REVIEW);
    if (Objects.isNull(brandAuthorisationWip)) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
          "Brand WIP with brand code " + request.getBrandCode() + " is not found");
    }
    return brandAuthorisationWip;
  }

  private Pair<List<BrandAuthorisationHistory>, BrandAuthorisationStatusIdDTO> approveBrandAuthorisationWip(
    BrandAuthorisationWip brandAuthorisationWip, BrandAuthorisationWipActionRequest request) {
    BrandAuthorisation existingBrandAuth;
    BrandAuthorisation updatedBrandAuth;
    List<BrandAuthorisationHistory> brandAuthorisationHistoryList = new ArrayList<>();
    ConverterUtil.validateApprovalDates(new Date(), request);
    BrandAuthorisationStatusIdDTO brandAuthorisationStatusIdDTO = new BrandAuthorisationStatusIdDTO();
    //If after present date then create in wip as upcoming
    if (CommonUtil.validateNewFlowCreation(request.getAuthStartDate(),
      brandAuthWipThresholdInDays)) {
      brandAuthorisationStatusIdDTO = formUpcomingRequest(request, brandAuthorisationWip);
    } else {
      existingBrandAuth = findBrandAuthorisation(brandAuthorisationWip.getStoreId(), request);
      updatedBrandAuth = Optional.ofNullable(existingBrandAuth).orElseGet(BrandAuthorisation::new);
      BeanUtils.copyProperties(brandAuthorisationWip, updatedBrandAuth, "id", "createdDate",
        "createdBy", "updatedBy", "updatedDate");

      updatedBrandAuth.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE);
      updatedBrandAuth.setDocumentLink(brandAuthorisationWip.getDocumentLink());
      updatedBrandAuth.setAuthStartDate(request.getAuthStartDate());
      updatedBrandAuth.setAuthExpireDate(request.getAuthExpireDate());

      brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.ACTIVE);
      brandAuthorisationWip.setMarkForDelete(true);

      updatedBrandAuth = brandAuthorisationRepository.save(updatedBrandAuth);
      brandAuthorisationHistoryList = generateBrandAuthHistory(updatedBrandAuth, existingBrandAuth);
      brandAuthorisationStatusIdDTO = BrandAuthorisationStatusIdDTO.builder()
          .status(updatedBrandAuth.getAuthorisationStatus().name()).id(updatedBrandAuth.getId())
          .build();
    }
    brandAuthorisationWipRepository.save(brandAuthorisationWip);
    return Pair.of(brandAuthorisationHistoryList, brandAuthorisationStatusIdDTO);
  }

  private BrandAuthorisationStatusIdDTO formUpcomingRequest(BrandAuthorisationWipActionRequest request,
    BrandAuthorisationWip brandAuthorisationWip) {
    BrandAuthorisationWip upComingBrandAuthorisationWip =
      brandAuthorisationWipRepository.findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
        request.getBrandCode(), request.getSellerCode(), BrandAuthorizationWipStatus.UPCOMING);
    if (Objects.nonNull(upComingBrandAuthorisationWip)) {
      upComingBrandAuthorisationWip.setAuthStartDate(request.getAuthStartDate());
      upComingBrandAuthorisationWip.setAuthExpireDate(request.getAuthExpireDate());
      upComingBrandAuthorisationWip.setDocumentLink(brandAuthorisationWip.getDocumentLink());
      brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.ACTIVE);
      brandAuthorisationWip.setMarkForDelete(true);
      brandAuthorisationWipRepository.save(upComingBrandAuthorisationWip);
    } else {
      brandAuthorisationWip.setAuthStartDate(request.getAuthStartDate());
      brandAuthorisationWip.setAuthExpireDate(request.getAuthExpireDate());
      brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.UPCOMING);
    }
    return BrandAuthorisationStatusIdDTO.builder()
        .status(brandAuthorisationWip.getAuthorisationStatus().name()).id(brandAuthorisationWip.getId())
        .build();
  }

  private Pair<List<BrandAuthorisationHistory>, BrandAuthorisationStatusIdDTO> performRejectionOrNeedRevisionForBrandAuthWip(
      BrandAuthorisationWip brandAuthorisationWip, String reason,
      BrandAuthorizationWipStatus brandAuthorizationWipStatus) {
    DateFormat historyDateFormat = new SimpleDateFormat(Constants.STANDARD_DATE_PATTERN);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(reason),
        ErrorMessage.REASON_MUST_NOT_BE_EMPTY.getMessage());
    BrandAuthorisationStatusIdDTO statusIdPair;

    brandAuthorisationWip.setReasons(reason);
    brandAuthorisationWip.setAuthorisationStatus(brandAuthorizationWipStatus);

    String activityDescription =
      brandAuthorizationWipStatus.equals(BrandAuthorizationWipStatus.REJECTED) ?
        BrandAuthorisationActivity.REJECTED.getDescription() :
        BrandAuthorisationActivity.NEED_REVISION.getDescription();
    activityDescription = activityDescription.concat(
      ConverterUtil.generateDateForHistory(brandAuthorisationWip.getAuthStartDate(),
        brandAuthorisationWip.getAuthExpireDate(), historyDateFormat));

    BrandAuthorisationHistory brandAuthorisationHistory = new BrandAuthorisationHistory();
    BeanUtils.copyProperties(brandAuthorisationWip, brandAuthorisationHistory, "id");
    brandAuthorisationHistory.setActivity(activityDescription);
    brandAuthorisationHistory.setUpdatedBy(GdnMandatoryParameterUtil.getUsername());
    brandAuthorisationWipRepository.save(brandAuthorisationWip);
    statusIdPair = BrandAuthorisationStatusIdDTO.builder()
        .status(brandAuthorisationWip.getAuthorisationStatus().name())
        .id(brandAuthorisationWip.getId()).build();
    return Pair.of(Collections.singletonList(brandAuthorisationHistory), statusIdPair);
  }


  @Override
  public boolean validateBrandAuthRequest(String storeId, String brandCode, String sellerCode,
    boolean edited) {
    log.info("Validating brand auth request for brandCode: {} , sellerCode: {}", brandCode,
      sellerCode);
    checkArgument(StringUtils.isNotBlank(storeId),
      String.valueOf(ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK));

    List<BrandAuthorisationWip> brandAuthorisationWipList =
      brandAuthorisationWipRepository.findByStoreIdAndBrandCodeAndSellerCodeAndMarkForDelete(
        storeId, brandCode, sellerCode, false);
    if (edited) {
      return brandAuthorisationWipList.stream().noneMatch(
        brandAuthorisationWip -> Constants.editAllowedStatus.contains(
          brandAuthorisationWip.getAuthorisationStatus()));
    } else {
      return brandAuthorisationWipList.stream().noneMatch(
        brandAuthorisationWip -> Constants.creationAllowedStatus.contains(
          brandAuthorisationWip.getAuthorisationStatus())) && Objects.isNull(
        brandAuthorisationRepository.findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
          storeId, brandCode, sellerCode));
    }
  }

  @Override
  public Page<BrandAuthorisationWipListResponse> getBrandAuthorisationWipListResponse(
      String storeId, BrandAuthorisationWipListRequest brandAuthorisationWipListRequest,
      Pageable pageable) {
    String sellerCode = brandAuthorisationWipListRequest.getSellerCode();
    String keyword = brandAuthorisationWipListRequest.getKeyword();
    String status = brandAuthorisationWipListRequest.getStatus();
    String tabName = brandAuthorisationWipListRequest.getTabName();
    Page<BrandAuthorisationWip> response;
    switch (tabName) {
      case Constants.UNDER_REVIEW -> response =
          findUnderReviewResponse(storeId, sellerCode, keyword, tabName, status, pageable);
      case Constants.AUTHORIZED_BRAND ->
          response = findAuthorizedBrandResponse(storeId, sellerCode, keyword, status, pageable);
      case Constants.UPCOMING ->
          response = findUpcomingBrandResponse(storeId, sellerCode, keyword, pageable);
      case null, default -> {
        return Page.empty();
      }
    }
    return response.map(this::convertToResponse);
  }

  private Page<BrandAuthorisationWip> findUnderReviewResponse(String storeId, String sellerCode,
      String keyword, String tabName, String status, Pageable pageable) {
    if (StringUtils.isBlank(status)) {
      return brandAuthorisationWipRepository.findBrandAuthorisationWipForExternalListing(storeId,
          sellerCode, keyword, tabName, Arrays.asList(BrandAuthorizationWipStatus.IN_REVIEW,
              BrandAuthorizationWipStatus.NEED_REVISION, BrandAuthorizationWipStatus.REJECTED),
          pageable);
    } else {
      return brandAuthorisationWipRepository.findBrandAuthorisationWipForExternalListing(storeId,
          sellerCode, keyword, tabName, List.of(BrandAuthorizationWipStatus.valueOf(status)),
          pageable);
    }
  }

  private Page<BrandAuthorisationWip> findUpcomingBrandResponse(String storeId, String sellerCode,
      String keyword, Pageable pageable) {
      return brandAuthorisationWipRepository.findBrandAuthorisationWipBySellerCodeAndBrandNameAndAuthorisationStatus(storeId,
          sellerCode, keyword, BrandAuthorizationWipStatus.UPCOMING.name(), pageable);
  }

  private Page<BrandAuthorisationWip> findAuthorizedBrandResponse(String storeId, String sellerCode,
      String keyword, String status, Pageable pageable) {
    Page<BrandAuthorisation> response =
        brandAuthorisationRepository.findBrandAuthorisationBySellerCodeAndBrandNameAndAuthorisationStatus(
            storeId, sellerCode, keyword, status, pageable, brandAuthNearExpiryDaysThreshold);
    if (Objects.nonNull(response) && CollectionUtils.isNotEmpty(response.getContent())) {
      List<BrandAuthorisationWip> brandAuthFilterResponseWip =
          ConverterUtil.toBrandAuthFilterResponseWip(response, status);
      return new PageImpl<>(brandAuthFilterResponseWip, pageable, response.getTotalElements());
    }
   return Page.empty();
  }

  private BrandAuthorisationWipListResponse convertToResponse(
      BrandAuthorisationWip brandAuthorisationWip) {
    BrandAuthorisationWipListResponse response = new BrandAuthorisationWipListResponse();
    BeanUtils.copyProperties(brandAuthorisationWip, response);
    if (Objects.nonNull(brandAuthorisationWip.getAuthorisationStatus())) {
      response.setAuthorisationStatus(brandAuthorisationWip.getAuthorisationStatus().name());
    }
    return response;
  }

  @Cacheable(value = CacheNames.BRAND_AUTHORISATION_WIP, key = "#storeId +'_'+ #sellerCode",
             unless = "#result == null")
  public long fetchCountOfPendingRequestsForSeller(String storeId, String sellerCode) {
    return brandAuthorisationWipRepository.countBySellerCodeAndAuthorisationStatusIn(sellerCode,
        List.of(BrandAuthorizationWipStatus.IN_REVIEW, BrandAuthorizationWipStatus.NEED_REVISION));
  }

  @Override
  public void publishUpcomingBrandAuthorisation(String storeId, int daysThreshold) {
    Date presentDate = new Date();
    Date afterDate = ConverterUtil.fetchDateFromThreshold(daysThreshold);
    List<BrandAuthorisationWip> brandAuthorisationWipList =
      brandAuthorisationWipRepository.findByStoreIdAndAuthorisationStatusAndAuthStartDateBetween(
        storeId, BrandAuthorizationWipStatus.UPCOMING, presentDate, afterDate);
    brandAuthorisationWipList.forEach(brandAuthorisationWip -> {
      BrandAuthActivateEventModel brandAuthActivateEventModel =
        ConverterUtil.toBrandAuthActivateEventModel(brandAuthorisationWip);
      log.info("Publishing brand auth activate event model {} ", brandAuthActivateEventModel);
      kafkaPublisher.send(kafkaTopicProperties.getBrandAuthActivateEvent(),
        brandAuthorisationWip.getBrandCode().concat(Constants.HYPHEN)
          .concat(brandAuthorisationWip.getSellerCode()), brandAuthActivateEventModel);
    });
  }

  @Override
  @Transactional
  public List<BrandAuthorisationHistory> activateBrandAuthorisation(
    BrandAuthActivateEventModel brandAuthActivateEventModel) {
    BrandAuthorisation savedBrandAuthorisation;
    BrandAuthorisation existingBrandAuth;
    BrandAuthorisationWip brandAuthorisationWip =
      brandAuthorisationWipRepository.findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
        brandAuthActivateEventModel.getBrandCode(), brandAuthActivateEventModel.getSellerCode(),
        BrandAuthorizationWipStatus.UPCOMING);
    if (Objects.nonNull(brandAuthorisationWip)) {
      MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER,
        brandAuthorisationWip.getCreatedBy());
      existingBrandAuth =
        brandAuthorisationRepository.findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
          brandAuthActivateEventModel.getStoreId(), brandAuthActivateEventModel.getBrandCode(),
          brandAuthActivateEventModel.getSellerCode());
      savedBrandAuthorisation = updateBrandAuth(brandAuthorisationWip, existingBrandAuth);
      return generateBrandAuthHistory(savedBrandAuthorisation, existingBrandAuth);
    }
    return new ArrayList<>();
  }

  private BrandAuthorisation updateBrandAuth(BrandAuthorisationWip brandAuthorisationWip,
    BrandAuthorisation brandAuthorisation) {
    brandAuthorisation = Objects.isNull(brandAuthorisation) ? new BrandAuthorisation() :
      brandAuthorisation;
    BeanUtils.copyProperties(brandAuthorisationWip, brandAuthorisation, "authorisationStatus", "id",
      "createdDate", "updatedDate", "createdBy");
    brandAuthorisation.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE);
    brandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.ACTIVE);
    brandAuthorisationWip.setMarkForDelete(true);
    brandAuthorisationWipRepository.save(brandAuthorisationWip);
    return brandAuthorisationRepository.save(brandAuthorisation);
  }

  private List<BrandAuthorisationHistory> generateBrandAuthHistory(
    BrandAuthorisation savedBrandAuthorisation, BrandAuthorisation existingBrandAuthorisation) {
    List<BrandAuthorisationHistory> brandAuthorisationHistoryList = new ArrayList<>();
    if (Objects.isNull(existingBrandAuthorisation)) {
      DateFormat historyDateFormat = new SimpleDateFormat(Constants.STANDARD_DATE_PATTERN);
      String description = BrandAuthorisationActivity.APPROVED.getDescription().concat(
        ConverterUtil.generateDateForHistory(savedBrandAuthorisation.getAuthStartDate(),
          savedBrandAuthorisation.getAuthExpireDate(), historyDateFormat));
      //Brand Auth Created History
      addEventToHistory(brandAuthorisationHistoryList, savedBrandAuthorisation, description,
        StringUtils.EMPTY, StringUtils.EMPTY);
    } else {
      generateHistoryForExistingBrandAuth(brandAuthorisationHistoryList, savedBrandAuthorisation,
        existingBrandAuthorisation);
    }
    return brandAuthorisationHistoryList;
  }

  private void generateHistoryForExistingBrandAuth(
    List<BrandAuthorisationHistory> brandAuthorisationHistoryList,
    BrandAuthorisation savedBrandAuthorisation, BrandAuthorisation existingBrandAuthorisation) {
    DateFormat dateFormat = new SimpleDateFormat(Constants.DEFAULT_DATE_PATTERN);
    DateFormat historyDateFormat = new SimpleDateFormat(Constants.STANDARD_DATE_PATTERN);
    //Start Date Update History
    addEventToHistory(brandAuthorisationHistoryList, savedBrandAuthorisation,
      BrandAuthorisationActivity.START_DATE_UPDATE.getDescription(),
      dateFormat.format(new Date(existingBrandAuthorisation.getAuthStartDate().getTime())),
      dateFormat.format(new Date(savedBrandAuthorisation.getAuthStartDate().getTime())));

    //End Date Update History
    addEventToHistory(brandAuthorisationHistoryList, savedBrandAuthorisation,
      BrandAuthorisationActivity.END_DATE_UPDATE.getDescription(),
      dateFormat.format(new Date(existingBrandAuthorisation.getAuthExpireDate().getTime())),
      dateFormat.format(new Date(savedBrandAuthorisation.getAuthExpireDate().getTime())));

    //Status Update History
    if (!existingBrandAuthorisation.getAuthorisationStatus().name()
      .equals(savedBrandAuthorisation.getAuthorisationStatus().name())) {
      String description = BrandAuthorisationActivity.STATUS_UPDATE.getDescription().concat(
        ConverterUtil.generateDateForHistory(savedBrandAuthorisation.getAuthStartDate(),
          savedBrandAuthorisation.getAuthExpireDate(), historyDateFormat));
      addEventToHistory(brandAuthorisationHistoryList, savedBrandAuthorisation, description,
        existingBrandAuthorisation.getAuthorisationStatus().name(),
        savedBrandAuthorisation.getAuthorisationStatus().name());
    }
  }

  private void addEventToHistory(List<BrandAuthorisationHistory> brandAuthorisationHistoryList,
    BrandAuthorisation savedBrandAuthorisation, String activityDescription, String oldStatus,
    String newStatus) {
    BrandAuthorisationHistory brandAuthDomainEventModel = new BrandAuthorisationHistory();
    BeanUtils.copyProperties(savedBrandAuthorisation, brandAuthDomainEventModel);
    brandAuthDomainEventModel.setActivity(activityDescription);
    brandAuthDomainEventModel.setOldStatus(oldStatus);
    brandAuthDomainEventModel.setNewStatus(newStatus);
    brandAuthDomainEventModel.setUpdatedBy(savedBrandAuthorisation.getUpdatedBy());
    brandAuthorisationHistoryList.add(brandAuthDomainEventModel);
  }

  @Override
  @Transactional(rollbackFor = Exception.class)
  public Pair<BrandAuthCreateResponse, BrandAuthorisation> createWipFromInternal(
      BrandAuthCreateRequest brandAuthCreateRequest, String storeId, String username)
      throws Exception {
    log.info("Create brand-auth-wip entry with the request: {}", brandAuthCreateRequest);
    BrandAuthorisationWip brandAuthorisationWip =
        brandAuthorisationWipRepository.findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
            brandAuthCreateRequest.getBrandCode(), brandAuthCreateRequest.getSellerCode(),
            BrandAuthorizationWipStatus.UPCOMING);
    if (Objects.nonNull(brandAuthorisationWip)) {
      updateAndSaveWipRequest(brandAuthorisationWip, brandAuthCreateRequest, username);
      return Pair.of(new BrandAuthCreateResponse(brandAuthCreateRequest.getBrandCode(), false),
          null);
    }
    BrandResponse savedBrand =
        brandService.findByBrandCodeCached(storeId, brandAuthCreateRequest.getBrandCode());
    ValidationUtil.validateCreateBrandAuthWipRequest(storeId,
        convertBrandAuthCreateRequestToBrandAuthCreateWipRequest(brandAuthCreateRequest),
        numberOfYears, savedBrand, true);
    brandAuthorisationWip =
        ConverterUtil.convertBrandAuthCreateRequestToBrandAuthWip(storeId, brandAuthCreateRequest);
    brandAuthorisationWip.setBrandName(savedBrand.getBrandName());
    brandAuthorisationWipRepository.save(brandAuthorisationWip);
    return Pair.of(new BrandAuthCreateResponse(brandAuthCreateRequest.getBrandCode(), false), null);
  }

  @Override
  public void updateWipEntryForActivation(String brandCode, String sellerCode) {
    BrandAuthorisationWip existingBrandAuthorisationWip =
        brandAuthorisationWipRepository.findByBrandCodeAndSellerCodeAndAuthorisationStatusAndMarkForDeleteFalse(
            brandCode, sellerCode, BrandAuthorizationWipStatus.UPCOMING);
    existingBrandAuthorisationWip.setAuthorisationStatus(BrandAuthorizationWipStatus.ACTIVE);
    existingBrandAuthorisationWip.setMarkForDelete(true);
    brandAuthorisationWipRepository.save(existingBrandAuthorisationWip);
  }

  @Override
  public List<BrandAuthorisation> fetchBrandAuthorisationForNearExpiry(String storeId) {
    Integer[] configDays = CommonUtil.convertToIntArray(configDay);
    List<BrandAuthorisation> brandAuthorisations = new ArrayList<>();
    for (int day : configDays) {
      List<BrandAuthorisation> brandAuthorisation =
          brandAuthorisationRepository.findSellerCodesByAuthorisationStatusAndConfiguration(
              storeId, BrandAuthorisationStatus.ACTIVE.name(), day);
      brandAuthorisations.addAll(brandAuthorisation);
    }
    return brandAuthorisations;
  }

  private BrandAuthCreateWipRequest convertBrandAuthCreateRequestToBrandAuthCreateWipRequest(
      BrandAuthCreateRequest brandAuthCreateRequest) {
    BrandAuthCreateWipRequest brandAuthCreateWipRequest = new BrandAuthCreateWipRequest();
    BeanUtils.copyProperties(brandAuthCreateRequest, brandAuthCreateWipRequest);
    return brandAuthCreateWipRequest;
  }

  private void updateAndSaveWipRequest(BrandAuthorisationWip brandAuthorisationWip,
      BrandAuthCreateRequest brandAuthCreateRequest, String username) {
    brandAuthorisationWip.setAuthStartDate(brandAuthCreateRequest.getAuthStartDate());
    brandAuthorisationWip.setAuthExpireDate(brandAuthCreateRequest.getAuthExpireDate());
    if (CollectionUtils.isNotEmpty(brandAuthCreateRequest.getDocumentLinks())) {
      brandAuthorisationWip.setDocumentLink(
          String.join(Constants.COMMA, brandAuthCreateRequest.getDocumentLinks()));
    }
    brandAuthorisationWipRepository.save(brandAuthorisationWip);
  }
}