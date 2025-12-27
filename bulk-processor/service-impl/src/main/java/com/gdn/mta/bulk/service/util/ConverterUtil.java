package com.gdn.mta.bulk.service.util;


import static com.gdn.mta.bulk.dto.BulkProcessType.QR_GENERATION;
import static com.gdn.mta.bulk.util.BulkUpdateServiceUtil.AND_SYMBOL;
import static com.gdn.mta.bulk.util.BulkUpdateServiceUtil.IS;
import static com.gdn.partners.bulk.util.BulkWorkOrderConstants.FAILURE_REASON;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Set;

import com.gdn.mta.bulk.dto.GenericTemplateFileType;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ItemPickupPointListingL3Request;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.util.GdnUUIDHelper;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.dto.BulkActivityStatus;
import com.gdn.mta.bulk.dto.BulkProcessDataDTO;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkUpdateErrorDTO;
import com.gdn.mta.bulk.dto.ProductDetailsRequest;
import com.gdn.mta.bulk.dto.ProductUpdateErrorMessages;
import com.gdn.mta.bulk.dto.QrCodeRowInfo;
import com.gdn.mta.bulk.dto.QrCodeRowItemInfo;
import com.gdn.mta.bulk.dto.ShippingTypeEligibility;
import com.gdn.mta.bulk.dto.product.DownloadQRCodeRequest;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.entity.WorkOrderEventModel;
import com.gdn.mta.bulk.models.DeletePickupPointResponseEventModel;
import com.gdn.mta.bulk.models.FailedItemAndReasonResponse;
import com.gdn.mta.bulk.models.RestrictedKeywordRequestData;
import com.gdn.mta.bulk.models.download.CampaignProductDownloadRequest;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.kafka.notification.NotificationKafka;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.message.mq.model.MessageEmailRequest;
import com.gdn.x.mta.model.enums.ProductType;
import com.gdn.x.product.rest.web.model.request.ProductSkuSummaryRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordUpdateRequestList;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordsUpdateRequest;
import com.google.common.collect.ImmutableMap;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class ConverterUtil {
  private static final ObjectMapper objectMapper = new ObjectMapper();
  private static final String DELETE_PICKUP_POINT_NOT_ALLOWED = "Delete not allowed, Item will be left with no active L5 ";
  private static final Map<String, Integer> RESTRICTED_KEYWORD_ACTION_TYPE =
      ImmutableMap.of("Straight rejection", 3, "Need revision", 2, "No action", 1, "Change category to", 0);

  public static NotificationKafka getNotificationKafka(String notificationDetail, String message,
      String notificationType, String destinationKey) {
    NotificationKafka notificationKafka = new NotificationKafka();
    notificationKafka.setTimestamp(System.currentTimeMillis());
    notificationKafka.setNotificationDetail(notificationDetail);
    notificationKafka.setNotificationMessage(message);
    notificationKafka.setNotificationType(notificationType);
    notificationKafka.setDestinationKey(destinationKey);
    Set<String> appNames = new HashSet<>();
    appNames.add(Constant.CLIENT_ID);
    notificationKafka.setAppNames(appNames);
    return notificationKafka;
  }

  public static void setMandatoryParamsToMessageEmailRequest(MessageEmailRequest messageEmailRequest) {
    messageEmailRequest.setStoreId(GdnMandatoryRequestParameterUtil.getStoreId());
    messageEmailRequest.setRequestId(GdnMandatoryRequestParameterUtil.getRequestId());
    messageEmailRequest.setChannelId(GdnMandatoryRequestParameterUtil.getChannelId());
    messageEmailRequest.setUsername(GdnMandatoryRequestParameterUtil.getUsername());
    messageEmailRequest.setClientId(GdnMandatoryRequestParameterUtil.getClientId());
    if (StringUtils.isBlank(messageEmailRequest.getStoreId())) {
      messageEmailRequest.setStoreId(Constant.STORE_ID);
    }
  }

  public static ItemPickupPointListingL3Request toItemPickupPointListingL3Request(
      ProductLevel3SummaryRequest productLevel3SummaryRequest, String businessPartnerCode, boolean needCorrection) {
    ItemPickupPointListingL3Request itemPickupPointListingL3Request = new ItemPickupPointListingL3Request();
    itemPickupPointListingL3Request.setNeedCorrection(needCorrection);
    itemPickupPointListingL3Request.setBusinessPartnerCode(businessPartnerCode);
    itemPickupPointListingL3Request.setProductSku(productLevel3SummaryRequest.getProductSkuList().get(0));
    itemPickupPointListingL3Request.setKeyword(StringUtils.EMPTY);
    return itemPickupPointListingL3Request;
  }

  public static boolean checkIfMPPIsAllowed(String mppAllowedSellers,
    ProfileResponse profileResponse) {
    return Objects.nonNull(profileResponse) && Objects.nonNull(profileResponse.getCompany()) && (
      (profileResponse.getCompany().isCncActivated()) || (
        Boolean.TRUE.equals(profileResponse.getMultiDefaultAddressFlag()) && mppAllowedSellers
          .contains(profileResponse.getCompany().getMerchantType())));
  }

  public static void checkIfSellerIsAllowedForPriceUpdate(ProfileResponse profileResponse,
      String bulkPriceUpdateAllowedSellers) {
    if (Objects.nonNull(profileResponse) && Objects.nonNull(profileResponse.getCompany())
        && !bulkPriceUpdateAllowedSellers.contains(profileResponse.getCompany().getMerchantType())) {
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ProductUpdateErrorMessages.BULK_PRICE_UPDATE_NOT_ALLOWED_FOR_SELLER);
    }
  }

  public static ProductSkuSummaryRequest toProductSkuSummaryRequest(
    CampaignProductDownloadRequest campaignProductDownloadRequest) {
    ProductSkuSummaryRequest productSkuSummaryRequest = new ProductSkuSummaryRequest();
    productSkuSummaryRequest.setIsArchived(false);
    if (CollectionUtils.isNotEmpty(
      campaignProductDownloadRequest.getCampaignItemSummaryRequest().getBrands())) {
      productSkuSummaryRequest.setBrand(
        campaignProductDownloadRequest.getCampaignItemSummaryRequest().getBrands());
    }
    if (CollectionUtils.isNotEmpty(campaignProductDownloadRequest.getCampaignItemSummaryRequest()
      .getCategories())) {
      productSkuSummaryRequest.setCategoryCodes(
        campaignProductDownloadRequest.getCampaignItemSummaryRequest().getCategories());
    }
    return productSkuSummaryRequest;
  }

  public static DeletePickupPointResponseEventModel toDeletePickupPointResponseEventModel(String businessPartnerCode,
      String pickupPointCode, String status, List<BulkProcessData> failedDataList,
      boolean deletePickupPointImprovementSwitch) throws IOException {
    if (deletePickupPointImprovementSwitch && (BulkProcess.STATUS_FAIL.equals(status)
        || BulkProcess.STATUS_PARTIALLY_DONE.equals(status) || BulkProcess.STATUS_ABORTED.equals(status))) {
      status = BulkActivityStatus.FAILED.name();
    }
    DeletePickupPointResponseEventModel deletePickupPointResponseEventModel = new DeletePickupPointResponseEventModel();
    deletePickupPointResponseEventModel.setBusinessPartnerCode(businessPartnerCode);
    deletePickupPointResponseEventModel.setPickupPointCode(pickupPointCode);
    deletePickupPointResponseEventModel.setStatus(status);
    deletePickupPointResponseEventModel.setFailedItemAndReasonResponseList(
        toFailedItemAndReasonResponse(failedDataList));
    return deletePickupPointResponseEventModel;
  }

  private static List<FailedItemAndReasonResponse> toFailedItemAndReasonResponse(List<BulkProcessData> failedDataList)
      throws IOException {
    List<FailedItemAndReasonResponse> failedItemAndReasonResponses = new ArrayList<>();
    for (BulkProcessData bulkProcessData : failedDataList) {
      try {
        if (StringUtils.isNotBlank(bulkProcessData.getErrorMessage())) {
          LinkedHashMap<String, String> errorMessageInfo = objectMapper.readValue(bulkProcessData.getErrorMessage(),
              new TypeReference<LinkedHashMap<String, String>>() {
              });
          failedItemAndReasonResponses.add(
              new FailedItemAndReasonResponse(errorMessageInfo.get(BulkParameters.ITEM_SKU_REQUEST),
                  errorMessageInfo.get(BulkParameters.FAILURE_REASON)));
        }
      } catch (Exception e) {
        log.error("Error while forming error message for failed items with bulkProcessCode : {} ",
            bulkProcessData.getBulkProcessCode());
      }
    }
    return failedItemAndReasonResponses;
  }

  public static CategoryKeywordUpdateRequestList toRestrictedKeywordAdditionRequest(
      List<BulkInternalProcessData> internalProcessDataList) throws JsonProcessingException {
    List<CategoryKeywordsUpdateRequest> addCategoryKeywordsUpdateRequests = new ArrayList<>();
    for (BulkInternalProcessData bulkInternalProcessData : internalProcessDataList) {
      RestrictedKeywordRequestData restrictedKeywordRequestData =
          objectMapper.readValue(bulkInternalProcessData.getData(), RestrictedKeywordRequestData.class);
      addCategoryKeywordsUpdateRequests.add(
          CategoryKeywordsUpdateRequest.builder().keyword(restrictedKeywordRequestData.getKeyword())
              .exclusionList(restrictedKeywordRequestData.getExclusionList())
              .message(restrictedKeywordRequestData.getMessage())
              .destinationCategory(restrictedKeywordRequestData.getDestinationCategory())
              .type(restrictedKeywordRequestData.getKeywordType())
              .action(RESTRICTED_KEYWORD_ACTION_TYPE.get(restrictedKeywordRequestData.getKeywordAction())).build());

    }
    return CategoryKeywordUpdateRequestList.builder().addedKeywords(addCategoryKeywordsUpdateRequests).deletedKeywords(new ArrayList<>()).build();
  }

  public static CategoryKeywordUpdateRequestList toRestrictedKeywordDeletionRequest(
      List<BulkInternalProcessData> internalProcessDataList) throws JsonProcessingException {
    List<CategoryKeywordsUpdateRequest> deleteCategoryKeywordsUpdateRequests = new ArrayList<>();
    for (BulkInternalProcessData bulkInternalProcessData : internalProcessDataList) {
      RestrictedKeywordRequestData restrictedKeywordRequestData =
          objectMapper.readValue(bulkInternalProcessData.getData(), RestrictedKeywordRequestData.class);
      deleteCategoryKeywordsUpdateRequests.add(
          CategoryKeywordsUpdateRequest.builder().keyword(restrictedKeywordRequestData.getKeyword())
              .exclusionList(restrictedKeywordRequestData.getExclusionList()).build());
    }
    return CategoryKeywordUpdateRequestList.builder().deletedKeywords(deleteCategoryKeywordsUpdateRequests).addedKeywords(new ArrayList<>()).build();
  }

  public static BulkProcess populateBulkProcess(DownloadQRCodeRequest downloadQRCodeRequest)
      throws Exception {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBusinessPartnerCode(downloadQRCodeRequest.getMerchantCode());
    bulkProcess.setStartDate(new Date());
    bulkProcess.setBulkProcessType(QR_GENERATION.getValue());
    bulkProcess.setErrorCount(0);
    bulkProcess.setSuccessCount(0);
    bulkProcess.setTotalCount(0);
    bulkProcess.setInputErrorCount(0);
    bulkProcess.setSystemErrorCount(0);
    if (Constant.STORE.equalsIgnoreCase(downloadQRCodeRequest.getQrGenerationType())
        || Constant.ALL_PRODUCTS.equalsIgnoreCase(downloadQRCodeRequest.getQrGenerationType())) {
      bulkProcess.setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
    } else {
      bulkProcess.setStatus(BulkProcess.STATUS_PENDING);
    }
    bulkProcess.setBulkUpdate(false);
    bulkProcess.setBulkProcessCode(GdnUUIDHelper.generateUUID());
    bulkProcess.setStoreId(downloadQRCodeRequest.getStoreId());
    bulkProcess.setRequestId(downloadQRCodeRequest.getRequestId());
    bulkProcess.setInternationalMerchant(false);
    bulkProcess.setDescription(downloadQRCodeRequest.getQrGenerationType());
    bulkProcess.setNotes(objectMapper.writeValueAsString(downloadQRCodeRequest));
    return bulkProcess;
  }

  public static QrCodeRowInfo constructQrCodeRowInfo(DownloadQRCodeRequest request) {
    return QrCodeRowInfo.builder()
        .merchantCode(request.getMerchantCode())
        .allStores(request.isAllStores())
        .isDarkTheme(request.getIsDarkTheme())
        .merchantName(request.getMerchantName())
        .printPrice(request.isPrintPrice())
        .qrPerPage(request.getQrPerPage())
        .templateSize(request.getTemplateSize())
        .qrGenerationType(request.getQrGenerationType())
        .build();
  }

  public static List<QrCodeRowItemInfo> constructQrCodeRowItemInfo(
      List<ProductDetailsRequest> productDetailsRequestList, boolean isCncActivated) {
    List<QrCodeRowItemInfo> qrCodeRowItemInfoList = new ArrayList<>();
    if (CollectionUtils.isEmpty(productDetailsRequestList)) {
      return qrCodeRowItemInfoList;
    }
    for (ProductDetailsRequest productDetailRequest : productDetailsRequestList) {
      qrCodeRowItemInfoList.add(QrCodeRowItemInfo.builder().itemSku(productDetailRequest.getItemSku())
          .productSku(productDetailRequest.getProductSku()).pickupPointCode(productDetailRequest.getPickupPointCode())
          .pickupPointName(productDetailRequest.getPickupPointName()).cncActivated(isCncActivated)
          .productPrice(productDetailRequest.getProductPrice()).productName(productDetailRequest.getItemName())
          .build());
    }
    return qrCodeRowItemInfoList;
  }

  public static BulkProcessData constructQrCodeBulkProcessData(QrCodeRowInfo qrCodeRowInfo,
      int rowNumber, BulkProcess bulkProcess) throws Exception{
    BulkProcessData bulkProcessData =
        BulkProcessData.builder().bulkRequestData(objectMapper.writeValueAsString(qrCodeRowInfo)).rowNumber(rowNumber)
            .startDate(new Date()).status(ProcessStatus.PENDING.name()).bulkProcessId(bulkProcess.getId())
            .bulkProcessCode(bulkProcess.getBulkProcessCode()).build();
    bulkProcessData.setStoreId(bulkProcess.getStoreId());
    return bulkProcessData;
  }

  public static ShippingTypeEligibility getShippingEligibility(ProfileResponse profileResponse,
    boolean bpBopisRestrictionEnabled) {
    ShippingTypeEligibility shippingTypeEligibility = new ShippingTypeEligibility();
    if (bpBopisRestrictionEnabled) {
      shippingTypeEligibility.setEligibleForBigProduct(
        BooleanUtils.toBooleanDefaultIfNull(profileResponse.getBigProductFlag(), true));
      shippingTypeEligibility.setEligibleForBopisProduct(
        BooleanUtils.toBooleanDefaultIfNull(profileResponse.getBopisFlag(), true));
    }
    return shippingTypeEligibility;
  }

  public static boolean checkIfEligibleForShippingType(Integer productType,
    ProfileResponse profileResponse, boolean bpBopisRestrictionEnabled) {
    if (bpBopisRestrictionEnabled) {
      if (productType.equals(ProductType.BIGPRODUCT.getCode())
        && !BooleanUtils.toBooleanDefaultIfNull(profileResponse.getBigProductFlag(), true)) {
        return false;
      } else if (productType.equals(ProductType.BOPIS.getCode())
        && !BooleanUtils.toBooleanDefaultIfNull(profileResponse.getBopisFlag(), true)) {
        return false;
      }
    }
    return true;
  }

  public static WorkOrderEventModel convertToWorkOrderEventModel(String storeId,
      BulkProcessDataDTO bulkProcessDataDTO) {
    WorkOrderEventModel workOrderEventModel = new WorkOrderEventModel();
    workOrderEventModel.setBulkProcessId(bulkProcessDataDTO.getBulkProcessId());
    workOrderEventModel.setBulkProcessCode(bulkProcessDataDTO.getBulkProcessCode());
    workOrderEventModel.setRowNumber(bulkProcessDataDTO.getRowNumber());
    workOrderEventModel.setId(bulkProcessDataDTO.getId());
    workOrderEventModel.setStoreId(storeId);
    workOrderEventModel.setBusinessPartnerCode(getBusinessPartnerCodeFromItemSku(bulkProcessDataDTO.getParentCode()));
    return workOrderEventModel;
  }

  private static String getBusinessPartnerCodeFromItemSku(String itemSku) {
    return itemSku.substring(0, itemSku.indexOf(Constant.DASH));
  }

  public static void setBulkProcessStatus(BulkProcess bulkProcess, int totalRows, int successRows) {
    if (successRows == totalRows) {
      bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    } else {
      String status = BulkProcess.STATUS_ABORTED;
      if (successRows > 0) {
        status = BulkProcess.STATUS_PARTIALLY_DONE;
      }
      bulkProcess.setStatus(status);
    }
  }

  public static void setBulkProcessNotesForWorkOrderUpload(List<BulkUpdateErrorDTO> bulkUpdateErrorDTOList, String storeId, BulkProcess bulkProcess) {
    for (BulkUpdateErrorDTO bulkUpdateErrorDTO : bulkUpdateErrorDTOList) {
      BulkProcessNotes bulkProcessNotes = createBulkProcessNotes(bulkUpdateErrorDTO, storeId, bulkProcess);
      addBulkProcessNotes(bulkProcessNotes, bulkProcess);
    }
  }

  public static void setBulkProcessNotesForBulkBasicInfoUpdate(List<BulkProcessData> failedDataList,
      String storeId, BulkProcess bulkProcess) {
    List<BulkProcessNotes> bulkProcessNotesList = new ArrayList<>();
    for (BulkProcessData failedBulkProcessData : failedDataList) {
      bulkProcessNotesList.add(
          createBulkProcessNotesForBasicBulkInfoUpdate(failedBulkProcessData, storeId,
              bulkProcess));
    }
    bulkProcess.setBulkProcessNotes(bulkProcessNotesList);
  }

  private static BulkProcessNotes createBulkProcessNotesForBasicBulkInfoUpdate(
      BulkProcessData failedBulkProcessData, String storeId, BulkProcess bulkProcess) {
    BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
    bulkProcessNotes.setStoreId(storeId);
    bulkProcessNotes.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    bulkProcessNotes.setBulkProcess(bulkProcess);
    String notes = new StringBuilder(
        Constant.ROW + failedBulkProcessData.getRowNumber() + Constant.PERIOD).append(
            BulkParameters.BLIBLI_SKU).append(IS).append(failedBulkProcessData.getParentProduct())
        .append(AND_SYMBOL).append(BulkUpdateServiceUtil.FAILURE_REASON).append(IS)
        .append(failedBulkProcessData.getErrorMessage()).toString();
    bulkProcessNotes.setNotes(notes);
    return bulkProcessNotes;
  }

  private static BulkProcessNotes createBulkProcessNotes(BulkUpdateErrorDTO bulkUpdateErrorDTO, String storeId, BulkProcess bulkProcess) {
    BulkProcessNotes bulkProcessNotes = new BulkProcessNotes();
    bulkProcessNotes.setStoreId(storeId);
    bulkProcessNotes.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    bulkProcessNotes.setBulkProcess(bulkProcess);
    String reason = Boolean.TRUE.equals(bulkProcess.getInternationalMerchant()) ? FAILURE_REASON :
      BulkUpdateServiceUtil.FAILURE_REASON;
    String notes = new StringBuilder(BulkParameters.BLIBLI_SKU).append(IS)
      .append(bulkUpdateErrorDTO.getProductSku()).append(AND_SYMBOL).append(reason)
      .append(IS).append(bulkUpdateErrorDTO.getReason()).toString();
    bulkProcessNotes.setNotes(notes);
    return bulkProcessNotes;
  }

  private static void addBulkProcessNotes(BulkProcessNotes bulkProcessNotes, BulkProcess bulkProcess) {
    if (bulkProcess.getBulkProcessNotes() != null) {
      bulkProcess.getBulkProcessNotes().add(bulkProcessNotes);
    } else {
      bulkProcess.setBulkProcessNotes(Collections.singletonList(bulkProcessNotes));
    }
  }

  public static String getVariableValueByProcessTypeForAbortion(String bulkProcessType) {
    String variableNameForStruckProcess = SystemParameterConfigNames.ABORT_PENDING_BULK_PROCESS_LIMIT;
    switch (BulkProcessType.getBulkProcessType(bulkProcessType)){
      case PRODUCT_CREATION_UPLOAD:
        variableNameForStruckProcess =
          SystemParameterConfigNames.PRODUCT_CREATION_UPLOAD_STRUCK_PROCESS_IN_MINUTES;
        break;
      case PRODUCT_CREATION_UPLOAD_PRIORITY:
        variableNameForStruckProcess =
          SystemParameterConfigNames.PRODUCT_CREATION_UPLOAD_PRIORITY_ABORT_STRUCK_PROCESS_IN_MINUTES;
        break;
      case PRODUCT_CREATION_UPLOAD_PRIORITY_1:
        variableNameForStruckProcess =
          SystemParameterConfigNames.PRODUCT_CREATION_UPLOAD_PRIORITY_1_ABORT_STRUCK_PROCESS_IN_MINUTES;
        break;
      case PRODUCT_CREATION_UPLOAD_PRIORITY_2:
        variableNameForStruckProcess =
          SystemParameterConfigNames.PRODUCT_CREATION_UPLOAD_PRIORITY_2_ABORT_STRUCK_PROCESS_IN_MINUTES;
        break;
      case CONVERTED_PRODUCT_CREATION_UPLOAD:
        variableNameForStruckProcess =
          SystemParameterConfigNames.CONVERTED_PRODUCT_CREATION_UPLOAD_ABORT_STRUCK_PROCESS_IN_MINUTES;
        break;
      case PRODUCT_LEVEL_3:
        variableNameForStruckProcess =
          SystemParameterConfigNames.PRODUCT_LEVEL3_ABORT_STRUCK_PROCESS_IN_MINUTES;
        break;
      case PRODUCT_LEVEL_3_UPDATE_PRIORITY_1:
        variableNameForStruckProcess =
          SystemParameterConfigNames.PRODUCT_LEVEL3_UPDATE_PRIORITY1_ABORT_STRUCK_PROCESS_IN_MINUTES;
        break;
      case PRODUCT_LEVEL_3_UPDATE_PRIORITY_2:
        variableNameForStruckProcess =
          SystemParameterConfigNames.PRODUCT_LEVEL3_UPDATE_PRIORITY2_ABORT_STRUCK_PROCESS_IN_MINUTES;
        break;
      case PRODUCT_LEVEL_3_GENERIC:
        variableNameForStruckProcess =
          SystemParameterConfigNames.PRODUCT_LEVEL_3_GENERIC_STRUCK_PROCESS_IN_MINUTES;
        break;
      case INSTANT_PICKUP_PRODUCT_UPSERT:
        variableNameForStruckProcess =
          SystemParameterConfigNames.INSTANT_PICKUP_PRODUCT_UPSERT_ABORT_STRUCK_PROCESS_IN_MINUTES;
        break;
      case INSTANT_PICKUP_PRODUCT_DELETE:
        variableNameForStruckProcess =
          SystemParameterConfigNames.INSTANT_PICKUP_PRODUCT_DELETE_ABORT_STRUCK_PROCESS_IN_MINUTES;
        break;
      case IN_STORE:
        variableNameForStruckProcess =
          SystemParameterConfigNames.IN_STORE_ABORT_STRUCK_PROCESS_IN_MINUTES;
        break;
      case DELETE_PICKUP_POINT:
        variableNameForStruckProcess =
          SystemParameterConfigNames.DELETE_PICKUP_POINT_STRUCK_PROCESS_IN_MINUTES;
        break;
      case TRANSFER_REQUEST:
        variableNameForStruckProcess =
          SystemParameterConfigNames.TRANSFER_REQUEST_STRUCK_PROCESS_IN_MINUTES;
        break;
      case ASSEMBLY_REQUEST:
        variableNameForStruckProcess =
          SystemParameterConfigNames.ASSEMBLY_REQUEST_STRUCK_PROCESS_IN_MINUTES;
        break;
      case ARCHIVE:
        variableNameForStruckProcess =
          SystemParameterConfigNames.ARCHIVE_ABORT_STRUCK_PROCESS_IN_MINUTES;
        break;
      case SUBJECT_TO_VAT:
        variableNameForStruckProcess =
          SystemParameterConfigNames.SUBJECT_TO_VAT_PROCESS_IN_MINUTES;
        break;
      case CAMPAIGN:
        variableNameForStruckProcess =
          SystemParameterConfigNames.CAMPAIGN_STRUCK_PROCESS_IN_MINUTES;
        break;
      case DISASSEMBLY_REQUEST:
        variableNameForStruckProcess =
          SystemParameterConfigNames.DISASSEMBLY_REQUEST_STRUCK_PROCESS_IN_MINUTES;
        break;
    }
    return variableNameForStruckProcess;
  }

  public static GenericTemplateFileType convertToGenericTemplateFileType(String fileType) {
    if(StringUtils.isBlank(fileType)) {
      return null;
    }
    try {
      return GenericTemplateFileType.valueOf(fileType.toUpperCase());
    } catch (IllegalArgumentException e) {
      return null;
    }
  }
}
