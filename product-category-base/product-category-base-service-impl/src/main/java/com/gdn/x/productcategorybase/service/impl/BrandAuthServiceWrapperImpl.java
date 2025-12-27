package com.gdn.x.productcategorybase.service.impl;

import static com.gdn.common.base.GdnPreconditions.checkArgument;
import static com.gdn.x.productcategorybase.Constants.HYPHEN;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.BeanUtils;
import com.gdn.x.productcategorybase.BrandAuthorisationActivity;
import com.gdn.x.productcategorybase.BrandAuthorisationStatus;
import com.gdn.x.productcategorybase.BrandAuthorizationWipStatus;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateWipRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateWipResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthDeleteRequest;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.BrandAuthorisationWipActionRequest;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisation;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationWip;
import com.gdn.x.productcategorybase.entity.brand.BrandAuthorisationHistory;
import com.gdn.x.productcategorybase.repository.brand.BrandAuthorisationRepository;
import com.gdn.x.productcategorybase.service.DomainEventPublisherService;
import com.gdn.x.productcategorybase.service.brand.BrandAuthHistoryService;
import com.gdn.x.productcategorybase.service.brand.BrandAuthServiceWrapper;
import com.gdn.x.productcategorybase.service.brand.BrandAuthorisationService;
import com.gdn.x.productcategorybase.service.brand.BrandAuthorisationWipService;
import com.gdn.x.productcategorybase.util.CommonUtil;
import com.gdn.x.productcategorybase.util.ConverterUtil;
import lombok.extern.slf4j.Slf4j;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.tuple.Pair;
import org.joda.time.DateTimeComparator;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Objects;
import java.util.Set;
import java.util.stream.Collectors;

@Component
@Slf4j
public class BrandAuthServiceWrapperImpl implements BrandAuthServiceWrapper {

  @Autowired
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Autowired
  private BrandAuthorisationService brandAuthorisationService;

  @Autowired
  private DomainEventPublisherService domainEventPublisherService;

  @Autowired
  private BrandAuthorisationRepository authorisationRepository;

  @Autowired
  private BrandAuthHistoryService brandAuthHistoryService;

  @Autowired
  private BrandAuthorisationWipService brandAuthorisationWipService;


  @Value("${brand.auth.wip.switch}")
  private boolean brandAuthWipSwitch;

  @Value("${brand.auth.wip.threshold.in.days}")
  private int brandAuthWipThresholdInDays;

  @Override
  public void editBrandAuthDetailsAndEvictCache(String storeId,
    BrandAuthUpdateRequest updateRequest, String username) throws Exception {
    checkArgument(StringUtils.isNotBlank(storeId),String.valueOf(ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK));
    checkArgument(StringUtils.isNotBlank(updateRequest.getBrandCode()),
      String.valueOf(ErrorMessage.BRAND_CODE_MUST_NOT_BE_BLANK));
    checkArgument(StringUtils.isNotBlank(updateRequest.getSellerCode()),
      String.valueOf(ErrorMessage.SELLER_CODE_MUST_NOT_BE_BLANK));
    if (CollectionUtils.isNotEmpty(updateRequest.getDocumentLinks())) {
      checkArgument(!updateRequest.getDocumentLinks().stream().anyMatch(StringUtils::isEmpty),
          ErrorMessage.FILE_NAME_SHOULD_NOT_BE_NULL_OR_EMPTY.getMessage());
    }
    BrandAuthorisation savedAuthData =
        authorisationRepository.findFirstByStoreIdAndBrandCodeAndSellerCodeAndMarkForDeleteFalse(
            storeId, updateRequest.getBrandCode(), updateRequest.getSellerCode());
    BrandAuthCreateRequest brandAuthCreateRequest = new BrandAuthCreateRequest();
    BeanUtils.copyProperties(updateRequest, brandAuthCreateRequest);
    if (brandAuthWipSwitch && (Objects.isNull(savedAuthData) || (
        checkToCreateNew(updateRequest.getAuthStartDate(), savedAuthData.getAuthStartDate(),
            updateRequest.getAuthorisationStatus()) && CommonUtil.validateNewFlowCreation(
            brandAuthCreateRequest.getAuthStartDate(), brandAuthWipThresholdInDays)))) {
      createBrandAuthAndEvictCache(brandAuthCreateRequest, storeId, username);
    } else {
      BrandAuthorisation authDetails =
          brandAuthorisationService.editBrandAuthDetails(storeId, savedAuthData, updateRequest);
      if (BrandAuthorizationWipStatus.UPCOMING.name()
          .equals(brandAuthCreateRequest.getAuthorisationStatus())) {
        brandAuthorisationWipService.updateWipEntryForActivation(
            brandAuthCreateRequest.getBrandCode(), brandAuthCreateRequest.getSellerCode());
      }
      if (Objects.nonNull(authDetails)) {
        clearCacheForUpdateRequest(authDetails, savedAuthData, updateRequest);
      } else {
        log.error("Saving Action Failed for update Request : {} ", updateRequest);
      }
    }
  }

  private boolean checkToCreateNew(Date updatedBrandAuthStartDate,
      Date existingBrandAuthStartDate, String authorisationStatus) {
    return updatedBrandAuthStartDate.compareTo(existingBrandAuthStartDate) != Constants.ZERO
        && !BrandAuthorisationStatus.INACTIVE.getValue().equals(authorisationStatus);
  }

  @Override
  public void deleteBrandAuthAndEvictCache(String storeId, String username,
    List<BrandAuthDeleteRequest> brandAuthDeleteRequestList) {
    checkArgument(StringUtils.isNotBlank(storeId),
      String.valueOf(ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK));
    Set<String> failedSellerCodes = new HashSet<>();
    for (BrandAuthDeleteRequest brandAuthDeleteRequest : brandAuthDeleteRequestList) {
      try {
        String failedSellerCode =
          brandAuthorisationService.deleteMappingByBrandCodeAndSellerCode(storeId, username,
            brandAuthDeleteRequest.getSellerCode(), brandAuthDeleteRequest.getBrandCode(),
            brandAuthDeleteRequest.getStatus(), brandAuthDeleteRequest.getId());
        if (StringUtils.isNotEmpty(failedSellerCode)) {
          failedSellerCodes.add(failedSellerCode);
        }
      } catch (Exception e) {
        log.error(
          "Error while deleting Brand Auth mapping for brand : {} for seller code : {} , error :  {}",
          brandAuthDeleteRequest.getBrandCode(), brandAuthDeleteRequest.getSellerCode(),
          e.getStackTrace());
      }
    }
    if (!failedSellerCodes.isEmpty()) {
      throw new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
        ErrorMessage.BRAND_AUTH_NOT_FOUND_FOR_SELLER_AND_BRAND_CODE.getMessage() + failedSellerCodes);
    }
  }

  @Override
  public BrandAuthCreateResponse createBrandAuthAndEvictCache(
    BrandAuthCreateRequest brandAuthCreateRequest, String storeId, String username)
    throws Exception {
    Pair<BrandAuthCreateResponse, BrandAuthorisation> brandAuthorisationPair;
    if (brandAuthWipSwitch && CommonUtil.validateNewFlowCreation(
      brandAuthCreateRequest.getAuthStartDate(), brandAuthWipThresholdInDays)) {
      // proceed with the new approach for brand-auth creation
      brandAuthorisationPair =
          brandAuthorisationWipService.createWipFromInternal(brandAuthCreateRequest, storeId,
              username);
    }
    else{
      brandAuthorisationPair =
          brandAuthorisationService.create(brandAuthCreateRequest, storeId, username);
      if (brandAuthorisationPair.getLeft().isBulkAction()) {
        BrandAuthUpdateRequest brandAuthUpdateRequest =
            CommonUtil.formUpdateRequestFromCreation(brandAuthCreateRequest);
        BrandAuthorisation savedBrandAuth = brandAuthorisationService.editBrandAuthDetails(storeId,
            brandAuthorisationPair.getRight(), brandAuthUpdateRequest);
        clearCacheForUpdateRequest(savedBrandAuth, brandAuthorisationPair.getRight(),
            brandAuthUpdateRequest);
      } else {
        brandAuthHistoryService.generateBrandAuthHistoryDomainEventModel(
            generateCreateHistory(brandAuthCreateRequest, username, storeId));
        applicationCacheServiceBean.evictBrandAuthorizationCache(
            brandAuthCreateRequest.getBrandCode());
      }
      if (BrandAuthorizationWipStatus.UPCOMING.name()
          .equals(brandAuthCreateRequest.getAuthorisationStatus())) {
        brandAuthorisationWipService.updateWipEntryForActivation(
            brandAuthCreateRequest.getBrandCode(), brandAuthCreateRequest.getSellerCode());
      }
    }
    return brandAuthorisationPair.getLeft();
  }

  private void publishHistoryUpdateEvent(BrandAuthorisation finalAuthData,
    BrandAuthorisation savedAuthData,
    BrandAuthorisationHistory authorisationHistory, BrandAuthUpdateRequest updateRequest) throws ParseException {
    if (!savedAuthData.getAuthorisationStatus().name()
      .equals(finalAuthData.getAuthorisationStatus().name())) {
      authorisationHistory.setActivity(BrandAuthorisationActivity.STATUS_UPDATE.getDescription());
      this.domainEventPublisherService.publishBrandAuthHistoryEvent(finalAuthData.getBrandCode(),
        finalAuthData.getSellerCode(), authorisationHistory);
      log.info("Published Status Update Event for brand-seller-code : {} ",
        finalAuthData.getBrandCode() + HYPHEN + finalAuthData.getSellerCode());
    }
    if (CollectionUtils.isNotEmpty(updateRequest.getDocumentLinks())) {
      String newFileNames = updateRequest.getDocumentLinks().stream().map(String::toString)
        .collect(Collectors.joining(","));
      authorisationHistory.setActivity(BrandAuthorisationActivity.DOCUMENT_LINK_UPDATE.getDescription());
      authorisationHistory.setNewStatus(newFileNames);
      authorisationHistory.setOldStatus(StringUtils.EMPTY);
      this.domainEventPublisherService.publishBrandAuthHistoryEvent(finalAuthData.getBrandCode(),
          finalAuthData.getSellerCode(), authorisationHistory);
      log.info("Published Document Link Update Event for brand-seller-code : {} ",
          finalAuthData.getBrandCode() + HYPHEN + finalAuthData.getSellerCode());
    }
    if (DateTimeComparator.getDateOnlyInstance()
      .compare(savedAuthData.getAuthStartDate(), finalAuthData.getAuthStartDate()) != 0) {
      DateFormat dateFormat = new SimpleDateFormat(Constants.DEFAULT_DATE_PATTERN);
      authorisationHistory.setActivity(BrandAuthorisationActivity.START_DATE_UPDATE.getDescription());
      authorisationHistory.setOldStatus(dateFormat.format(new Date(savedAuthData.getAuthStartDate().getTime())));
      authorisationHistory.setNewStatus(dateFormat.format(new Date(finalAuthData.getAuthStartDate().getTime())));
      this.domainEventPublisherService.publishBrandAuthHistoryEvent(finalAuthData.getBrandCode(),
        finalAuthData.getSellerCode(), authorisationHistory);
      log.info("Published Date change Update Event for brand-seller-code : {} ",
        finalAuthData.getBrandCode() + HYPHEN + finalAuthData.getSellerCode());
    }
    if (DateTimeComparator.getDateOnlyInstance()
        .compare(savedAuthData.getAuthExpireDate(), finalAuthData.getAuthExpireDate()) != 0) {
      DateFormat dateFormat = new SimpleDateFormat(Constants.DEFAULT_DATE_PATTERN);
      authorisationHistory.setActivity(BrandAuthorisationActivity.END_DATE_UPDATE.getDescription());
      authorisationHistory.setOldStatus(dateFormat.format(new Date(savedAuthData.getAuthExpireDate().getTime())));
      authorisationHistory.setNewStatus(dateFormat.format(new Date(finalAuthData.getAuthExpireDate().getTime())));
      this.domainEventPublisherService.publishBrandAuthHistoryEvent(finalAuthData.getBrandCode(),
        finalAuthData.getSellerCode(), authorisationHistory);
      log.info("Published Date change Update Event for brand-seller-code : {} ",
        finalAuthData.getBrandCode() + HYPHEN + finalAuthData.getSellerCode());
    }
  }

  private BrandAuthorisationHistory generateCreateHistory(BrandAuthCreateRequest createRequest,
    String username, String storeId){
    BrandAuthorisationHistory brandAuthorisationHistory = new BrandAuthorisationHistory();
    brandAuthorisationHistory.setActivity(BrandAuthorisationActivity.CREATE.getDescription());
    brandAuthorisationHistory.setNewStatus(StringUtils.EMPTY);
    brandAuthorisationHistory.setOldStatus(StringUtils.EMPTY);
    brandAuthorisationHistory.setBrandCode(createRequest.getBrandCode());
    brandAuthorisationHistory.setSellerCode(createRequest.getSellerCode());
    brandAuthorisationHistory.setStoreId(storeId);
    brandAuthorisationHistory.setUpdatedBy(username);
    return brandAuthorisationHistory;
  }

  public void clearCacheForUpdateRequest(BrandAuthorisation savedBrandAuthorisation,
    BrandAuthorisation existingBrandAuthorisation, BrandAuthUpdateRequest updateRequest)
    throws ParseException {
    if (Objects.nonNull(savedBrandAuthorisation)) {
      applicationCacheServiceBean.evictBrandAuthorizationCache(
        savedBrandAuthorisation.getBrandCode());
      log.info("Cache evicted and Update was success for Auth details : {} ",
        savedBrandAuthorisation);
      BrandAuthorisationHistory authorisationHistory =
        ConverterUtil.toBrandAuthHistoryEventModel(savedBrandAuthorisation,
          existingBrandAuthorisation);
      publishHistoryUpdateEvent(savedBrandAuthorisation, existingBrandAuthorisation,
        authorisationHistory, updateRequest);
    }
  }

  @Override
  public BrandAuthCreateWipResponse createBrandAuthWip(
      BrandAuthCreateWipRequest brandAuthCreateWipRequest, String storeId, String username)
      throws Exception {
    DateFormat historyDateFormat = new SimpleDateFormat(Constants.STANDARD_DATE_PATTERN);
    Pair<BrandAuthCreateWipResponse, BrandAuthorisationWip> brandAuthorisationPair =
        brandAuthorisationWipService.brandAuthCreateWipRequest(brandAuthCreateWipRequest, storeId,
            username);
    brandAuthHistoryService.generateBrandAuthHistoryDomainEventModel(
      CommonUtil.generateCreateWipHistory(brandAuthorisationPair.getRight(), username, storeId,
        historyDateFormat));
    return brandAuthorisationPair.getLeft();
  }
}
