package com.gdn.mta.bulk.util;

import static com.gdn.mta.bulk.helper.ProductLevel3ProcessorValidationMessageHelper.errorMessage;
import static com.gdn.partners.bulk.util.Constant.ZERO;

import java.io.IOException;
import java.net.URL;
import java.net.URLConnection;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.StringJoiner;
import java.util.function.BiConsumer;
import java.util.function.BooleanSupplier;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.BulkUploadOption;
import com.gdn.mta.bulk.ValidationException;
import com.gdn.mta.bulk.dto.BrandAndCategoryPredictionRequest;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.bulk.dto.BulkProcessExternalUploadRequest;
import com.gdn.mta.bulk.dto.BulkUpdateErrorDTO;
import com.gdn.mta.bulk.dto.ValidateImageDTO;
import com.gdn.mta.bulk.models.ColumnConfig;
import com.gdn.mta.bulk.models.ExcelHeaderNames;
import com.gdn.mta.bulk.models.PlatformConfig;
import com.gdn.mta.bulk.models.SheetConfig;
import com.gdn.mta.bulk.entity.BulkProcessImageQC;
import com.gdn.mta.product.enums.ProductCreationType;
import com.gdn.x.businesspartner.constant.ProfileFlagNames;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.collections4.MapUtils;
import org.apache.commons.lang3.ObjectUtils;
import org.apache.commons.lang3.StringUtils;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ItemPickupPointRequest;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gda.mta.product.dto.generator.GenerateShippingWeightRequest;
import com.gda.mta.product.dto.response.B2BResponse;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.BulkUpdateChangeType;
import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.dto.BulkBasicInfoRequest;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.GenericTemplateFileType;
import com.gdn.mta.bulk.dto.ProductUpdateErrorMessages;
import com.gdn.mta.bulk.dto.ValidateExcelRowsRequest;
import com.gdn.mta.bulk.dto.product.BulkBasicInfoVideoDownloadResponseModel;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.entity.BulkProcessImage;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.entity.BulkProcessVideo;
import com.gdn.mta.bulk.entity.KafkaEventLog;
import com.gdn.mta.bulk.models.BulkUploadErrorCounter;
import com.gdn.mta.bulk.models.ImageDownloadResult;
import com.gdn.mta.bulk.models.ProductBasicDetail;
import com.gdn.mta.bulk.models.download.responsedata.BulkDownloadProductBasicInfoResponse;
import com.gdn.mta.bulk.models.download.responsedata.ImageBasicInfoResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductBasicInfoResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductBasicResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductImageAndVideoResponse;
import com.gdn.mta.bulk.service.util.ConverterUtil;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import lombok.extern.slf4j.Slf4j;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;

@Slf4j
public class CommonUtils {

  private static final String FAAS_ACTIVATED = "faasActivated";
  public static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
  private static final String ACTIVE_MERCHANT = "ACTIVE";
  private static final String FILE_FROM_INACTIVE_SELLER = "File uploaded by inactive seller";
  public static final String PROCESS_ABORTED = "Proses dibatalkan";
  public static final String ROW_NUMBER = "RowNumber";
  public static final String DEFAULT_ROW_NUMBER = "0";
  public static final String BASIC_INFO_SHEET_TYPE = "basicInfo";
  public static final String MEDIA_SHEET_TYPE = "media";
  public static final String SHIPPING_SHEET_TYPE = "shipping";
  public static final String SALES_SHEET_TYPE = "sales";
  private static final String EXTERNAL_UPLOAD_DATE_FORMAT = "yyMMdd";
  private static final String EXTERNAL_UPLOAD_TIME_FORMAT = "HHmmss";

  public static void checkForMerchantSkuChanged(Set<BulkUpdateChangeType> bulkUpdateChangeTypeSet,
      Map<String, String> productL5Data, ItemPickupPointListingL3Response itemPickupPointListingL3Response) {
    if (!StringUtils.equals(itemPickupPointListingL3Response.getMerchantSku(),
        productL5Data.get(BulkParameters.SELLER_SKU))) {
      bulkUpdateChangeTypeSet.add(BulkUpdateChangeType.MERCHANT_SKU_CHANGED);
    }
  }

  public static void checkForPriceChanged(Set<BulkUpdateChangeType> bulkUpdateChangeTypeSet,
      Map<String, String> productL5Data, ItemPickupPointListingL3Response itemPickupPointListingL3Response) {
    Double newPrice = Double.parseDouble(productL5Data.get(BulkParameters.PRICE_HEADER));
    Double existingPrice = itemPickupPointListingL3Response.getPrices().get(0).getPrice();
    Double newSalePrice = Double.parseDouble(productL5Data.get(BulkParameters.SELLING_PRICE_HEADER));
    Double existingSalePrice = itemPickupPointListingL3Response.getPrices().get(0).getSalePrice();
    if (!newPrice.equals(existingPrice) || !newSalePrice.equals(existingSalePrice)) {
      bulkUpdateChangeTypeSet.add(BulkUpdateChangeType.PRICE_CHANGED);
    }
  }

  public static void checkForStockChanged(Set<BulkUpdateChangeType> bulkUpdateChangeTypeSet,
      ItemPickupPointListingL3Response itemPickupPointListingL3Response,
      ItemPickupPointRequest itemPickupPointRequest) {
    if (itemPickupPointListingL3Response.isWebSyncStock()) {
      return;
    }
    int newStockValue =
        Objects.isNull(itemPickupPointRequest.getStock()) ? Constant.ZERO : itemPickupPointRequest.getStock();
    if (newStockValue != Constant.ZERO) {
      bulkUpdateChangeTypeSet.add(BulkUpdateChangeType.STOCK_CHANGED);
    }
  }

  public static void checkForPoQuotaChanged(Set<BulkUpdateChangeType> bulkUpdateChangeTypeSet,
      ItemPickupPointListingL3Response itemPickupPointListingL3Response,
      ItemPickupPointRequest itemPickupPointRequest) {
    if (itemPickupPointListingL3Response.isWebSyncStock()) {
      return;
    }
    if (itemPickupPointRequest.getInitialPreOrderQuota() != Constant.ZERO) {
      bulkUpdateChangeTypeSet.add(BulkUpdateChangeType.PO_QUOTA_CHANGED);
    }
  }

  public static void checkForBfbFieldsChanged(Set<BulkUpdateChangeType> bulkUpdateChangeTypeSet,
      Map<String, String> productL5Data, ItemPickupPointListingL3Response itemPickupPointListingL3Response,
      ProductLevel3ViewConfigResponse bfbItemViewConfigs) {
    if (Objects.isNull(bfbItemViewConfigs) && (productL5Data.containsKey(BulkParameters.EXTERNAL_BFB_STATUS)
        || productL5Data.containsKey(BulkParameters.AMPHI_BFB_STATUS))) {
      bulkUpdateChangeTypeSet.add(BulkUpdateChangeType.BFB_STATUS_CHANGED);
    }
    B2BResponse b2BResponse = itemPickupPointListingL3Response.getB2bFields();
    if (Objects.nonNull(b2BResponse)) {
      if (productL5Data.containsKey(BulkParameters.BFB_BASE_PRICE)) {
        Double newBfbPrice = Double.parseDouble(productL5Data.get(BulkParameters.BFB_BASE_PRICE));
        if (!newBfbPrice.equals(b2BResponse.getBasePrice())) {
          bulkUpdateChangeTypeSet.add(BulkUpdateChangeType.BFB_PRICE_CHANGED);
        }
        if (productL5Data.containsKey(BulkParameters.BFB_MANAGED)) {
          boolean newBfbManaged = Boolean.parseBoolean(productL5Data.get(BulkParameters.BFB_MANAGED));
          if (newBfbManaged != b2BResponse.isManaged()) {
            bulkUpdateChangeTypeSet.add(BulkUpdateChangeType.BFB_MANAGED_CHANGED);
          }
        }
      }
    }
  }

  public static void checkForCncChanged(Set<BulkUpdateChangeType> bulkUpdateChangeTypeSet,
      ItemPickupPointListingL3Response itemPickupPointListingL3Response,
      ItemPickupPointRequest itemPickupPointRequest) {
    if (itemPickupPointListingL3Response.isCncActive() != itemPickupPointRequest.isCncActive()) {
      bulkUpdateChangeTypeSet.add(BulkUpdateChangeType.CNC_CHANGED);
    }
  }

  public static boolean isOMGSeller(ProfileResponse profileResponse) {
    return Optional.ofNullable(profileResponse).map(ProfileResponse::getFlags)
        .filter(MapUtils::isNotEmpty).map(flags -> flags.get(ProfileFlagNames.BLIBLI_OMG))
        .map(Object::toString).map(Boolean::parseBoolean).orElse(false);
  }

  public static boolean isPreOrderActive(PreOrderDTO preOrderDTO){
    if (Objects.nonNull(preOrderDTO)) {
      boolean isPreOrder = Optional.ofNullable(preOrderDTO.getIsPreOrder()).orElse(false);
      Date preOrderDate = preOrderDTO.getPreOrderDate();
      return isPreOrder && Objects.nonNull(preOrderDate) && new Date().before(preOrderDate);
    }
    return false;
  }
  public static boolean isFaasSeller(ProfileResponse profileResponse) {
    return Optional.ofNullable(profileResponse).map(ProfileResponse::getFlags).filter(MapUtils::isNotEmpty)
        .map(flags -> flags.get(FAAS_ACTIVATED)).map(Object::toString).map(Boolean::parseBoolean).orElse(false);
  }

  public static boolean isNotFaasEligible(boolean faasFeatureSwitch, ProfileResponse profileResponse) {
    return !faasFeatureSwitch || !CommonUtils.isFaasSeller(profileResponse);
  }

  public static boolean isInstoreEligibleSeller(ProfileResponse profileResponse) {
    return Objects.nonNull(profileResponse) && Objects.nonNull(profileResponse.getCompany())
        && profileResponse.getCompany().isOfflineToOnlineFlag();
  }

  public static boolean isCncSeller(ProfileResponse profileResponse) {
    return Objects.nonNull(profileResponse) && Objects.nonNull(profileResponse.getCompany())
      && profileResponse.getCompany().isCncActivated();
  }

  public static boolean isInstoreEligibleSellerWithSwitch(boolean instoreNewFlowEnabled,
      ProfileResponse profileResponse) {
    return instoreNewFlowEnabled && isInstoreEligibleSeller(profileResponse);
  }

  public static boolean isEligibleForShippingWeightGeneration(
      GenerateShippingWeightRequest generateShippingWeightRequest) {
    return !Stream.of(generateShippingWeightRequest.getLength(), generateShippingWeightRequest.getWidth(),
            generateShippingWeightRequest.getHeight(), generateShippingWeightRequest.getWeight())
        .allMatch(dimension -> Objects.nonNull(dimension) && Double.compare(dimension, 0.0) == 0);
  }


  public static ValidateExcelRowsRequest getValidateExcelRowsRequest(BulkProcess bulkProcess,
      BulkUploadErrorCounter bulkUploadErrorCounter, long maxStockLimit, MerchantStatusType merchantStatusType,
      String commonImageErrorMessage, int minimumPrice, boolean isInternationalMerchant, boolean instoreSeller,
      boolean pureInstoreProduct, Map<String, Object> row, BulkProcessNotes bulkProcessNotes, String merchantType) {
    ValidateExcelRowsRequest validateExcelRowsRequest = new ValidateExcelRowsRequest();
    validateExcelRowsRequest.setRaw(row);
    validateExcelRowsRequest.setBulkProcessNotes(bulkProcessNotes);
    validateExcelRowsRequest.setBulkUploadErrorCounter(bulkUploadErrorCounter);
    validateExcelRowsRequest.setInternationalMerchant(isInternationalMerchant);
    validateExcelRowsRequest.setMinimumPrice(minimumPrice);
    validateExcelRowsRequest.setMaxStockLimit(maxStockLimit);
    validateExcelRowsRequest.setMerchantStatusType(merchantStatusType);
    validateExcelRowsRequest.setMerchantType(merchantType);
    validateExcelRowsRequest.setCommonImageErrorMessage(commonImageErrorMessage);
    validateExcelRowsRequest.setPureInstoreProduct(pureInstoreProduct);
    validateExcelRowsRequest.setInstoreSeller(instoreSeller);
    validateExcelRowsRequest.setPrimaryIdentifier(bulkProcess.getPrimaryIdentifier());
    return validateExcelRowsRequest;
  }
  public static boolean isDormantSeller(ProfileResponse profileResponse) {
    return Objects.nonNull(profileResponse) && Objects.nonNull(profileResponse.getCompany())
        && profileResponse.getCompany().isDormantFlag();
  }

  public static boolean getDimensionMissingFromMissingFields(
      ItemPickupPointListingL3Response itemPickupPointListingL3Response) {
    return Objects.nonNull(itemPickupPointListingL3Response) && CollectionUtils.isNotEmpty(
        itemPickupPointListingL3Response.getMissingFields()) && itemPickupPointListingL3Response.getMissingFields()
        .contains(Constant.DIMENSIONS_MISSING);
  }
  public static GenericTemplateFileType validateAndGetGenericTemplateFileType(String fileType, String requestId) {
    GenericTemplateFileType genericTemplateFileType = ConverterUtil.convertToGenericTemplateFileType(fileType);
    if (Objects.isNull(genericTemplateFileType)) {
      log.error("Generic template file type invalid for requestId : {} ", requestId);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
          ProductUpdateErrorMessages.INVALID_GENERIC_TEMPLATE_FILE_TYPE);
    }
    return genericTemplateFileType;
  }

  public static boolean skipCategoryRegeneration(boolean templateFileExist,
      List<KafkaEventLog> changeEvent, boolean regenerateExplicitly) {
    return templateFileExist && CollectionUtils.isEmpty(changeEvent) && !regenerateExplicitly;
  }


  public static boolean validateStockAtL5ForPPUpdateNonMppSeller(
    boolean validateWarehouseVariantDeletionEnabled, boolean isPPCodeChangedForNonMppSeller,
    ItemPickupPointListingL3Response itemPickupPointListingL3Response,
    String supportedMerchantsForWarehouseStockValidation, ProfileResponse profileResponse) {
    Set<String> supportedMerchants =
      Optional.ofNullable(supportedMerchantsForWarehouseStockValidation).map(
        value -> Arrays.stream(value.split(Constant.COMMA)).map(String::trim)
          .collect(Collectors.toSet())).orElse(Collections.emptySet());
    boolean merchantSupportedForValidation = supportedMerchants.contains(
      Optional.ofNullable(profileResponse).map(ProfileResponse::getCompany)
        .map(CompanyDTO::getMerchantType).orElse(StringUtils.EMPTY)) && Optional.ofNullable(
      profileResponse).map(ProfileResponse::isFbbActivated).orElse(false);
    if (merchantSupportedForValidation && validateWarehouseVariantDeletionEnabled
      && isPPCodeChangedForNonMppSeller) {
      return Optional.ofNullable(itemPickupPointListingL3Response)
        .map(ItemPickupPointListingL3Response::getAvailableStockLevel1).orElse(ZERO) > ZERO;
    }
    return false;
  }

  public static void filterHideFromSellerAttributes(boolean productSuitabilityFeatureEnabled,
      CategoryDetailResponse category) {
    if (productSuitabilityFeatureEnabled && CollectionUtils.isNotEmpty(category.getCategoryAttributes())) {
      category.getCategoryAttributes().removeIf(
          categoryAttribute -> Optional.ofNullable(categoryAttribute).map(CategoryAttributeResponse::getAttribute)
              .map(AttributeResponse::isHideForSeller).orElse(false));
    }
  }

  public static void populateCategoryCodeAndHierarchyMap(List<CategoryHierarchyResponse> categoryHierarchyResponses,
      Map<String, String> categoryCodeAndHierarchyMap, Map<String, Integer> categoryCodeAndLogisticAdjustmentMap) {
    for (CategoryHierarchyResponse categoryHierarchyResponse : categoryHierarchyResponses) {
      List<CategoryResponse> categoryHierarchyList = categoryHierarchyResponse.getCategoryHierarchy();
      if (CollectionUtils.isEmpty(categoryHierarchyList)) {
        continue;
      }
      CategoryResponse cnCategoryResponse = categoryHierarchyList.get(0);
      String cnCategoryCode = cnCategoryResponse.getCategoryCode();
      Integer logisticAdjustment = cnCategoryResponse.getLogisticAdjustment();
      StringJoiner categoryHierarchyJoiner = new StringJoiner(Constant.CATEGORY_HIERARCHY_DELIMITER);
      for (int i = categoryHierarchyList.size() - 1; i >= 0; i--) {
        categoryHierarchyJoiner.add(categoryHierarchyList.get(i).getName());
      }
      categoryCodeAndHierarchyMap.put(cnCategoryCode, categoryHierarchyJoiner.toString());
      categoryCodeAndLogisticAdjustmentMap.put(cnCategoryCode, logisticAdjustment);
    }
  }

  public static void populateCategoryHierarchyByCnCategoryCodeForGenericCreation(
      List<CategoryHierarchyResponse> categoryHierarchyResponses,
      Map<String, String> categoryHierarchy) {
    GdnPreconditions.checkArgument(ObjectUtils.isNotEmpty(categoryHierarchyResponses.getFirst()),Constant.ERROR_IN_CATEGORY_PREDICTION);
    List<CategoryResponse> categoryHierarchyList =
        categoryHierarchyResponses.getFirst().getCategoryHierarchy();
    if (CollectionUtils.isEmpty(categoryHierarchyList)) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, Constant.ERROR_IN_CATEGORY_PREDICTION);
    }
    int size = categoryHierarchyList.size();
    if (size >= 3) {
      String cnCategoryHierarchy =
          categoryHierarchyList.reversed().stream().skip(2).map(CategoryResponse::getName)
              .collect(Collectors.joining(Constant.ARROW_DELIMITER));
      categoryHierarchy.put(GenericBulkHeaders.CN, cnCategoryHierarchy);
    }
    if (size >= 2) {
      categoryHierarchy.put(GenericBulkHeaders.C2, categoryHierarchyList.get(size - 2).getName());
    }
    categoryHierarchy.put(GenericBulkHeaders.CATEGORY,
        categoryHierarchyList.getLast().getName());
  }

  public static boolean isBulkDownloadResponseEmptyOrInvalid(
      BulkDownloadProductBasicInfoResponse bulkDownloadProductBasicInfoResponse) {
    return Objects.isNull(bulkDownloadProductBasicInfoResponse) || (
        CollectionUtils.isEmpty(bulkDownloadProductBasicInfoResponse.getProductBasicInfoResponseList())
            && MapUtils.isEmpty(bulkDownloadProductBasicInfoResponse.getExceptionMap()));
  }

  public static Set<String> populateCommonImagesAndCategoryCodes(
      BulkDownloadProductBasicInfoResponse bulkDownloadProductBasicInfoResponse) {
    Set<String> categoryCodes = new HashSet<>();
    Map<Integer, BiConsumer<ProductBasicInfoResponse, String>> imageSetters = initializeImageSetters();
    if (Objects.nonNull(bulkDownloadProductBasicInfoResponse) && CollectionUtils.isNotEmpty(
        bulkDownloadProductBasicInfoResponse.getProductBasicInfoResponseList())) {
      for (ProductBasicInfoResponse product : bulkDownloadProductBasicInfoResponse.getProductBasicInfoResponseList()) {
        if (Objects.isNull(product) || CollectionUtils.isEmpty(product.getCommonImageList())) {
          continue;
        }
        if (StringUtils.isNotBlank(product.getCategoryCode())) {
          categoryCodes.add(product.getCategoryCode());
        }
        List<ImageBasicInfoResponse> commonImageList = product.getCommonImageList();
        int imageIndex = 2;
        for (ImageBasicInfoResponse image : commonImageList) {
          if (Objects.isNull(image) || StringUtils.isBlank(image.getLocationPath())) {
            continue;
          }
          if (image.isMainImage() && StringUtils.isBlank(product.getMainImage())) {
            product.setMainImage(image.getLocationPath());
          } else {
            setCommonImages(product, imageIndex++, image.getLocationPath(), imageSetters);
          }
        }
      }
    }
    return categoryCodes;
  }

  private static Map<Integer, BiConsumer<ProductBasicInfoResponse, String>> initializeImageSetters() {
    Map<Integer, BiConsumer<ProductBasicInfoResponse, String>> imageSetters = new HashMap<>();
    imageSetters.put(2, ProductBasicInfoResponse::setCommonImage2);
    imageSetters.put(3, ProductBasicInfoResponse::setCommonImage3);
    imageSetters.put(4, ProductBasicInfoResponse::setCommonImage4);
    imageSetters.put(5, ProductBasicInfoResponse::setCommonImage5);
    imageSetters.put(6, ProductBasicInfoResponse::setCommonImage6);
    imageSetters.put(7, ProductBasicInfoResponse::setCommonImage7);
    imageSetters.put(8, ProductBasicInfoResponse::setCommonImage8);
    return imageSetters;
  }

  public static void setCommonImages(ProductBasicInfoResponse product, int index, String locationPath,
      Map<Integer, BiConsumer<ProductBasicInfoResponse, String>> imageSetters) {
    if (imageSetters.containsKey(index)) {
      imageSetters.get(index).accept(product, locationPath);
    }
  }

  public static BulkProcess getBulkProcess(String storeId, String requestId, String bulkProcessCode,
      BulkBasicInfoRequest bulkProcessUpdateRequest, int successCount, int errorCount, boolean isTrustedSeller) {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkUpdate(true);
    bulkProcess.setStartDate(Calendar.getInstance().getTime());
    String bulkProcessType = isTrustedSeller ?
        BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1.getValue() :
        BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_2.getValue();
    bulkProcess.setBulkProcessType(bulkProcessType);
    bulkProcess.setBusinessPartnerCode(bulkProcessUpdateRequest.getBusinessPartnerCode());
    bulkProcess.setBulkProcessCode(bulkProcessCode);
    bulkProcess.setStoreId(storeId);
    bulkProcess.setStatus(BulkProcess.STATUS_PENDING);
    bulkProcess.setRequestId(requestId);
    bulkProcess.setSuccessCount(successCount);
    bulkProcess.setErrorCount(errorCount);
    bulkProcess.setInputErrorCount(0);
    bulkProcess.setSystemErrorCount(0);
    return bulkProcess;
  }

  public static void checkBulkProcessImagesAndVideo(BulkProcess bulkProcess,
      List<BulkProcessImage> bulkProcessImageList, List<BulkProcessVideo> bulkProcessVideoList,
      List<BulkProcess> updateBulkProcessList) {
    boolean imageDownloadComplete = isImageDownloadComplete(bulkProcessImageList);
    boolean videoDownloadComplete =
        imageDownloadComplete && isVideoDownloadComplete(bulkProcessVideoList);
    if (imageDownloadComplete && videoDownloadComplete) {
      bulkProcess.setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
      updateBulkProcessList.add(bulkProcess);
    }
  }

  public static void checkBulkProcessImagesAndImageQC(BulkProcess bulkProcess,
      List<BulkProcessImage> bulkProcessImageList, List<BulkProcessImageQC> bulkProcessImageQCList,
      List<BulkProcess> updateBulkProcessList) {
    boolean imageDownloadComplete = isImageDownloadComplete(bulkProcessImageList);
    boolean imageQCResponseRecieved =
        imageDownloadComplete && isImageQCResponseRecieved(bulkProcessImageQCList);
    if (imageDownloadComplete && imageQCResponseRecieved) {
      bulkProcess.setStatus(BulkProcess.STATUS_READY_TO_PROCESS);
      updateBulkProcessList.add(bulkProcess);
    }
  }

  private static boolean isVideoDownloadComplete(List<BulkProcessVideo> bulkProcessVideoList) {
    boolean videoDownloadComplete = true;
    if (CollectionUtils.isNotEmpty(bulkProcessVideoList)) {
      for (BulkProcessVideo bulkProcessVideo : bulkProcessVideoList) {
        if (!bulkProcessVideo.isCompleted()) {
          videoDownloadComplete = false;
          break;
        }
      }
    }
    return videoDownloadComplete;
  }

  private static boolean isImageQCResponseRecieved(
      List<BulkProcessImageQC> bulkProcessImageQCList) {
    boolean imageQCResponseRecieved = true;
    if (CollectionUtils.isNotEmpty(bulkProcessImageQCList)) {
      for (BulkProcessImageQC bulkProcessImageQC : bulkProcessImageQCList) {
        if (!bulkProcessImageQC.isCompleted()) {
          imageQCResponseRecieved = false;
          break;
        }
      }
    }
    return imageQCResponseRecieved;
  }

  private static boolean isImageDownloadComplete(List<BulkProcessImage> bulkProcessImageList) {
    boolean imageDownloadComplete = true;
    if (CollectionUtils.isNotEmpty(bulkProcessImageList)) {
      for (BulkProcessImage bulkProcessImage : bulkProcessImageList) {
        if (!bulkProcessImage.isCompleted()) {
          imageDownloadComplete = false;
          break;
        }
      }
    }
    return imageDownloadComplete;
  }

  public static void validateBasicInfoHeaders(BulkBasicInfoRequest bulkBasicInfoRequest, String storeId,
      String bulkProcessCode, String businessPartnerCode, Map<Integer, String> headers) {
    if (headers.size() == 0) {
      log.error(
          "No data found for bulk basic info update storeId: {}, bulkProcessCode: {}, " + "businessPartnerCode: {}",
          storeId, bulkProcessCode, businessPartnerCode);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, BulkUpdateServiceUtil.FILE_BLANK_ERROR);
    }
    Set<String> headerList =
        headers.values().stream().map(String::trim).map(String::toLowerCase).collect(Collectors.toSet());
    List<String> expectedHeaders = BulkParameters.getBasicInfoHeaderList(bulkBasicInfoRequest.isInstoreSeller(),
            headerList.contains(BulkParameters.ERROR_COLUMN.toLowerCase())).stream().map(String::trim)
        .map(String::toLowerCase).toList();
    if (!headerList.containsAll(expectedHeaders)) {
      log.error(
          "Invalid headers for basic info sheet , expected headers : {} , actual headers : {} , fileName : {} , username : {} , bulkProcessCode : {} ",
          expectedHeaders, headerList, bulkBasicInfoRequest.getFileName(), bulkBasicInfoRequest.getUpdatedBy(),
          bulkBasicInfoRequest.getBulkProcessCode());
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION, BulkUpdateServiceUtil.HEADER_MISMATCH);
    }
  }

  public static boolean validateRowData(Map<String, String> row, StringBuilder errors, boolean pureInstore) {
    boolean isValid = validateProductName(row, errors);
    if (row.containsKey(BulkParameters.INSTORE) && !validateInstore(row, errors))
      isValid = false;
    if (!validateDescription(row, errors, pureInstore))
      isValid = false;
    if (!validateMainImage(row, errors))
      isValid = false;
    if (!validateShippingType(row, errors))
      isValid = false;
    if (!validateDimensionFields(row, errors, pureInstore))
      isValid = false;
    return isValid;
  }

  public static boolean validateDimensionFields(Map<String, String> row, StringBuilder errors, boolean pureInstore) {
    boolean isValid = true;
    List<String> fieldsToValidate =
        Arrays.asList(BulkParameters.LENGTH_HEADER, BulkParameters.WIDTH_HEADER, BulkParameters.HEIGHT_HEADER,
            BulkParameters.ACTUAL_WEIGHT);
    for (String fieldKey : fieldsToValidate) {
      if (!validateNumericField(row, errors, fieldKey, pureInstore)) {
        isValid = false;
      }
    }
    return isValid;
  }


  public static void addErrorNote(BulkProcess bulkProcess, BulkProcessData data, Map<String, String> row,
      String errorMsg) {
    data.setInputErrorCount(1);
    data.setStatus(BulkProcessData.STATUS_FAIL);
    data.setErrorMessage(errorMsg);
  }


  public static boolean validateNumericField(Map<String, String> row, StringBuilder errorMessageBuilder,
      String fieldKey, boolean pureInstoreProduct) {
    String value = row.get(fieldKey);
    if (StringUtils.isBlank(value)) {
      if (!pureInstoreProduct) {
        errorMessageBuilder.append(fieldKey).append(" must not be blank; ");
        return false;
      } else {
        return true;
      }
    } else {
      try {
        Double.parseDouble(value);
      } catch (NumberFormatException e) {
        errorMessageBuilder.append(fieldKey).append(" must be a valid number; ");
        return false;
      }
    }
    return true;
  }


  public static boolean validateProductName(Map<String, String> row, StringBuilder errorMessageBuilder) {
    String productName = row.get(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER);
    if (StringUtils.isBlank(productName)) {
      errorMessageBuilder.append(BulkProcessValidationErrorMessages.PRODUCT_NAME_MUST_NOT_BE_BLANK)
          .append(Constant.SEMI_COLON);
      return false;
    }
    if (productName.length() > Constant.PRODUCT_NAME_LENGTH) {
      errorMessageBuilder.append(BulkProcessValidationErrorMessages.PRODUCT_NAME_MUST_NOT_BE_MORE_THAN_150_CHARACTERS)
          .append("; ");
      return false;
    }
    return true;
  }

  public static boolean validateMainImage(Map<String, String> row, StringBuilder errorMessageBuilder) {
    String mainImage = row.get(BulkParameters.MAIN_PHOTO);
    if (StringUtils.isBlank(mainImage)) {
      errorMessageBuilder.append(BulkProcessValidationErrorMessages.MAIN_IMAGE_MUST_NOT_BE_BLANK)
          .append(Constant.SEMI_COLON);
      return false;
    }
    return true;
  }

  public static boolean validateInstore(Map<String, String> row, StringBuilder errorMessageBuilder) {
    String instore = row.get(BulkParameters.INSTORE);
    if (StringUtils.isBlank(instore)) {
      errorMessageBuilder.append(BulkProcessValidationErrorMessages.INSTORE_CANNOT_BE_BLANK)
          .append(Constant.SEMI_COLON);
      return false;
    }
    try {
      Double.parseDouble(instore);
    } catch (NumberFormatException e) {
      errorMessageBuilder.append(BulkProcessValidationErrorMessages.INSTORE_MUST_BE_NUMERIC)
          .append(Constant.SEMI_COLON);
      return false;
    }

    return true;
  }

  public static boolean validateShippingType(Map<String, String> row, StringBuilder errorMessageBuilder) {
    String shippingType = row.get(BulkParameters.SHIPPING_TYPE);
    if (StringUtils.isBlank(shippingType)) {
      errorMessageBuilder.append("Shipping type must not be blank; ");
      return false;
    }
    if (!BulkParameters.ALLOWED_SHIPPING_HEADERS.contains(shippingType)) {
      errorMessageBuilder.append("Invalid shipping type: ").append(shippingType).append(Constant.SEMI_COLON);
      return false;
    }
    return true;
  }


  public static boolean validateDescription(Map<String, String> row, StringBuilder errorMessageBuilder,
      boolean pureInstoreProduct) {
    String description = row.getOrDefault(BulkParameters.DESCRIPTIONS, "");
    if (StringUtils.isBlank(description) && !pureInstoreProduct) {
      errorMessageBuilder.append(BulkProcessValidationErrorMessages.DESCRIPTION_MUST_NOT_BE_BLANK)
          .append(Constant.SEMI_COLON);
      return false;
    }
    return true;
  }


  public static String getBulkProcessTypeBasedOnTrustedSellerAndUserInputRows(String bulkProcessType, int userInputRows,
      int trustedSellerRowCount, int regularSellerMinRowCount, int regularSellerMaxCount, boolean trustedSeller) {
    if (trustedSeller) {
      return userInputRows <= trustedSellerRowCount ?
          BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1.getValue() :
          BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_2.getValue();
    }
    if (userInputRows <= regularSellerMinRowCount) {
      return BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1.getValue();
    }
    if (userInputRows <= regularSellerMaxCount) {
      return BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_2.getValue();
    }
    return bulkProcessType;
  }

  public static BulkProcessData getBulkProcessData(BulkProcess bulkProcess, int rowNumber, String productSku)
      throws JsonProcessingException {
    BulkProcessData data = new BulkProcessData();
    data.setBulkProcessId(bulkProcess.getId());
    data.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    data.setStoreId(bulkProcess.getStoreId());
    data.setStatus(BulkProcessData.STATUS_PENDING);
    data.setRequestId(bulkProcess.getRequestId());
    data.setRowNumber(rowNumber);
    data.setParentProduct(productSku);
    return data;
  }

  public static boolean validaExcelRowData(BulkProcess bulkProcess, List<BulkProcessData> requestData,
      Map<String, String> userData, BulkProcessData bulkProcessData, Boolean b2cActivated,
      String errorMessageForVideoUrl) {
    boolean pureInstoreProduct = false;
    try {
      double instoreValue = Double.parseDouble(userData.getOrDefault(BulkParameters.INSTORE, "0.0"));
      pureInstoreProduct = instoreValue == 1.0 && Boolean.FALSE.equals(b2cActivated);
    } catch (NumberFormatException e) {
      log.error("Error parsing instore value : {} , bulkProcessCode : {} , rowNumber : {}",
          userData.get(BulkParameters.INSTORE), bulkProcess.getBulkProcessCode(), bulkProcessData.getRowNumber(), e);
    }
    StringBuilder validationErrors = new StringBuilder();
    validationErrors.append(errorMessageForVideoUrl);
    boolean isValid = CommonUtils.validateRowData(userData, validationErrors, pureInstoreProduct);
    if (StringUtils.isNotBlank(validationErrors.toString())) {
      isValid = false;
    }
    if (!isValid) {
      CommonUtils.addErrorNote(bulkProcess, bulkProcessData, userData, validationErrors.toString());
    }
    requestData.add(bulkProcessData);
    return isValid;
  }

  public static void generateBulkProcessVideoList(BulkProcess bulkProcess,
      Map<String, List<String>> videoUrlToProductCodesMap, List<BulkProcessVideo> bulkProcessVideoList) {
    for (Map.Entry<String, List<String>> entry : videoUrlToProductCodesMap.entrySet()) {
      String videoUrl = entry.getKey();
      BulkProcessVideo bulkProcessVideo = new BulkProcessVideo();
      bulkProcessVideo.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      bulkProcessVideo.setCompleted(false);
      bulkProcessVideo.setUploadedURL(videoUrl);
      bulkProcessVideo.setStoreId(bulkProcess.getStoreId());
      bulkProcessVideoList.add(bulkProcessVideo);
    }
  }

  public static void generateBulkProcessImageList(BulkProcess bulkProcess,
      Map<String, List<String>> imageUrlToProductCodesMap, List<BulkProcessImage> bulkProcessImageList,
      Map<String, ProductImageAndVideoResponse> productCodeAndProductResponseMap) throws JsonProcessingException {
    int index = 1;
    for (Map.Entry<String, List<String>> entry : imageUrlToProductCodesMap.entrySet()) {
      String imageUrl = entry.getKey();
      List<String> productCodes = entry.getValue();
      BulkProcessImage bulkProcessImage = new BulkProcessImage();
      bulkProcessImage.setBulkProcessId(bulkProcess.getId());
      bulkProcessImage.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      bulkProcessImage.setCompleted(false);
      bulkProcessImage.setImageURL(imageUrl);
      bulkProcessImage.setStoreId(bulkProcess.getStoreId());
      bulkProcessImage.setSequence(index);
      List<ProductBasicDetail> productDetailsList = new ArrayList<>();
      for (String productCode : productCodes) {
        ProductImageAndVideoResponse response = productCodeAndProductResponseMap.get(productCode);
        ProductBasicDetail productDetail = new ProductBasicDetail();
        productDetail.setProductCode(productCode);
        if (Objects.nonNull(response)) {
          productDetail.setProductName(StringUtils.isBlank(response.getProductName()) ?
              Constant.SAMPLE_PRODUCT_NAME :
              response.getProductName());
          productDetail.setBrand(
              StringUtils.isBlank(response.getBrand()) ? Constant.SAMPLE_BRAND_NAME : response.getBrand());
        } else {
          productDetail.setProductName(Constant.SAMPLE_PRODUCT_NAME);
          productDetail.setBrand(Constant.SAMPLE_BRAND_NAME);
        }
        productDetailsList.add(productDetail);
      }
      String notes = OBJECT_MAPPER.writeValueAsString(productDetailsList);
      bulkProcessImage.setNotes(notes);
      bulkProcessImageList.add(bulkProcessImage);
      index++;
    }
  }

  public static void mapNewImageUrlsToProductCodes(Map<String, List<String>> imageUrlToProductCodesMap,
      Map<String, String> userData, ProductBasicResponse productInfo,
      ProductImageAndVideoResponse productImageAndVideoResponse, String imageStaticBaseUrlPrefix) {
    List<String> existingImageUrls =
        productImageAndVideoResponse.getCommonImageList().stream().map(ImageBasicInfoResponse::getLocationPath)
            .filter(StringUtils::isNotBlank).collect(Collectors.toList());
    // Get uploaded image URLs from userData
    Set<String> uploadedImages =
        BulkParameters.BASIC_INFO_IMAGE_HEADERS.stream().map(userData::get).filter(StringUtils::isNotBlank)
            .collect(Collectors.toSet());
    // Identify new image URLs and map them to product SKUs
    for (String uploadedUrl : uploadedImages) {
      boolean prefixReplaced = false;
      if (uploadedUrl.startsWith(imageStaticBaseUrlPrefix)) {
        prefixReplaced = true;
        uploadedUrl = uploadedUrl.substring(imageStaticBaseUrlPrefix.length());
      }
      boolean newImageUrl = existingImageUrls.stream().noneMatch(uploadedUrl::contains);
      if (newImageUrl) {
        if (prefixReplaced) {
          uploadedUrl = imageStaticBaseUrlPrefix + uploadedUrl;
        }
        imageUrlToProductCodesMap.computeIfAbsent(uploadedUrl, k -> new ArrayList<>())
            .add(productInfo.getProductCode());
      }
    }
  }

  public static boolean isVideoUrlExisting(String uploadedVideoUrl, String existingVideoUrl) {
    // Because existing video doesn't come as video url it comes as json of entire video object
    return StringUtils.isNotBlank(existingVideoUrl) && (existingVideoUrl.contains(uploadedVideoUrl));
  }

  public static String trimVideoUrlPrefix(String uploadedVideoUrl, String videoStaticBaseUrlPrefix) {
    if (StringUtils.isNotBlank(uploadedVideoUrl) && uploadedVideoUrl.startsWith(videoStaticBaseUrlPrefix)) {
      uploadedVideoUrl = uploadedVideoUrl.substring(videoStaticBaseUrlPrefix.length());
    }
    return uploadedVideoUrl;
  }

  public static void setBulkProcessVideoFinalStatus(
      BulkBasicInfoVideoDownloadResponseModel bulkBasicInfoVideoDownloadResponseModel,
      BulkProcessVideo bulkProcessVideo) {
    bulkProcessVideo.setVideoId(bulkBasicInfoVideoDownloadResponseModel.getVideoId());
    bulkProcessVideo.setVideoUrl(bulkBasicInfoVideoDownloadResponseModel.getSourceUrl());
    bulkProcessVideo.setErrorMessage(bulkBasicInfoVideoDownloadResponseModel.getErrorMessage());
    bulkProcessVideo.setErrorCode(bulkBasicInfoVideoDownloadResponseModel.getErrorCode());
    bulkProcessVideo.setCoverImagePath(bulkBasicInfoVideoDownloadResponseModel.getCoverImagePath());
    bulkProcessVideo.setVideoName(bulkBasicInfoVideoDownloadResponseModel.getVideoName());
    bulkProcessVideo.setCompleted(true);
  }

  public static void setImageDownloadStatus(BulkProcessImage bulkProcessImage, ImageDownloadResult imageDownloadResult,
      StringBuilder validationErrorMessage) {
    if (imageDownloadResult.isDownloadSuccess()) {
      bulkProcessImage.setLocation(imageDownloadResult.getImageFileName());
    } else {
      bulkProcessImage.setErrorMessage(String.valueOf(validationErrorMessage));
    }
    bulkProcessImage.setCompleted(true);
  }

  public static URLConnection getUrlConnection(String imageUrl, int httpConnectionTimeout,
      int httpConnectionReadTimeout) throws IOException {
    URLConnection connection = new URL(imageUrl).openConnection();
    connection.setConnectTimeout(httpConnectionTimeout);
    connection.setReadTimeout(httpConnectionReadTimeout);
    connection.setRequestProperty(Constant.ACCEPT_HEADER, Constant.IMAGE_HEADER);
    return connection;
  }

  public static void handleValidationException(String errorMessage, String imageUrl, StringBuilder validationErrorMessage) {
    String errMsg;
    if (errorMessage.contains(BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE_EN)) {
      errMsg = BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE;
    } else {
      errMsg = BulkProcessValidationErrorMessages.IMAGE_MAX_SIZE_VALIDATION_ERR_MESSAGE_EN;
    }
    validationErrorMessage.append(errorMessage(imageUrl + Constant.SPACE + Constant.HYPHEN + Constant.SPACE , errMsg, StringUtils.EMPTY)).append(Constant.PERIOD);
  }

  public static void handleInvalidType(String imageUrl, String imageKey, String contentType, List<String> images,
      StringBuilder validationErrorMessage, String bulkProcessCode) {
    images.remove(imageKey);
    validationErrorMessage.append(
            errorMessage(imageUrl + " - ", BulkProcessValidationErrorMessages.IMAGE_URL_TYPE_INVALID, StringUtils.EMPTY))
        .append(Constant.DOT);
    log.error("Invalid image type {} for URL {} and bulkProcessCode {}", contentType, imageUrl, bulkProcessCode);
  }

  public static void handleAllFailedScenario(BulkProcess bulkProcess, List<BulkProcessData> requestData) {
    if (requestData.stream()
        .allMatch(bulkProcessData -> BulkProcessData.STATUS_FAIL.equals(bulkProcessData.getStatus()))) {
      bulkProcess.setStatus(BulkProcess.STATUS_PUBLISHED);
    }
  }

  public static void validateActiveProduct(Map<String, String> productDataL5,
    ItemPickupPointListingL3Response itemPickupPointListingL3Response,
    boolean isMultiPickupPointSeller, List<BulkUpdateErrorDTO> listBulkUpdateErrorDTO,
    List<Map<String, String>> validationFailedData) {
    if (!isMultiPickupPointSeller && Objects.isNull(itemPickupPointListingL3Response)) {
      log.error("Item pickup point listing response is null for itemSku: {}, pickupPointCode: {}",
        productDataL5.get(BulkParameters.BLIBLI_SKU),
        productDataL5.get(BulkParameters.PICKUP_POINT_HEADER));
      listBulkUpdateErrorDTO.add(BulkUpdateErrorDTO.builder().productName(
          Optional.ofNullable(productDataL5.get(BulkParameters.PRODUCT_NAME))
            .orElse(StringUtils.EMPTY)).productSku(productDataL5.get(BulkParameters.BLIBLI_SKU))
        .pickupPointCode(productDataL5.get(BulkParameters.PICKUP_POINT_HEADER))
        .reason(ProductUpdateErrorMessages.INVALID_ADD_PICKUP_POINT_REQUEST).build());
      validationFailedData.add(productDataL5);
      throw new ApplicationRuntimeException(ErrorCategory.VALIDATION,
        ProductUpdateErrorMessages.INVALID_ADD_PICKUP_POINT_REQUEST);
    }
  }

  public static boolean runProductStateValidationChecks(ProductBasicResponse productInfo,
      BulkProcess bulkProcess, BulkProcessData bulkProcessData, Map<String, String> userData,
      List<BulkProcessData> requestData) {
    final Map<BooleanSupplier, String> validationMap = new LinkedHashMap<>();
    validationMap.put(productInfo::isTakenDown,
        BulkProcessValidationErrorMessages.PRODUCT_TAKEN_DOWN);
    validationMap.put(productInfo::isMarkForDelete,
        BulkProcessValidationErrorMessages.PRODUCT_DELETED);
    validationMap.put(productInfo::isArchived, BulkProcessValidationErrorMessages.PRODUCT_ARCHIVED);
    validationMap.put(() -> productInfo.isSharedProduct() || !productInfo.isSyncProduct(),
        BulkProcessValidationErrorMessages.PRODUCT_PROCESS_FAILED);
    validationMap.put(() -> !StringUtils.equals(bulkProcess.getBusinessPartnerCode(),
            productInfo.getMerchantCode()),
        BulkProcessValidationErrorMessages.PRODUCT_NOT_BELONG_TO_BUSINESS_PARTNER);

    for (Map.Entry<BooleanSupplier, String> entry : validationMap.entrySet()) {
      if (entry.getKey().getAsBoolean()) {
        CommonUtils.addErrorNote(bulkProcess, bulkProcessData, userData, entry.getValue());
        requestData.add(bulkProcessData);
        return true;
      }
    }
    return false;
  }

  public static ValidateImageDTO getValidateImageDTO(String bulkProcessCode,
    Set<String> urlImagesWithInvalidExtension, List<String> images,
    BulkUploadErrorCounter bulkUploadErrorCounter, StringBuilder validationErrorMessage,
    boolean isInternationalMerchant, Map.Entry<String, String> image, URLConnection connection) {
    ValidateImageDTO validateImageDTO = new ValidateImageDTO();
    validateImageDTO.setImage(image);
    validateImageDTO.setConnection(connection);
    validateImageDTO.setBulkProcessCode(bulkProcessCode);
    validateImageDTO.setUrlImagesWithInvalidExtension(urlImagesWithInvalidExtension);
    validateImageDTO.setImages(images);
    validateImageDTO.setBulkUploadErrorCounter(bulkUploadErrorCounter);
    validateImageDTO.setValidationErrorMessage(validationErrorMessage);
    validateImageDTO.setInternationalMerchant(isInternationalMerchant);
    return validateImageDTO;
  }

  public static int findNonNullDataRowIndexByStartRow(Sheet sheet, int startRow) {
    int lastRowNum = sheet.getLastRowNum();
    for (int i = startRow; i <= lastRowNum; i++) {
      Row row = sheet.getRow(i);
      if (Objects.nonNull(row)) {
        return i;
      }
    }
    return startRow + 1;
  }

  public static Row getOrCreateRow(Sheet sheet, int rowIndex) {
    Row row = sheet.getRow(rowIndex);
    if (Objects.isNull(row)) {
      row = sheet.createRow(rowIndex);
    }
    return row;
  }

  public static void setProcessExternalUploadRequest(String storeId, String username,
    String requestId, BulkProcessType bulkProcessType, BulkProcessExternalUploadRequest request) {
    request.setUsername(username);
    request.setBulkProcessType(bulkProcessType);
    request.setRequestId(requestId);
    request.setStoreId(storeId);
  }

  public static void validateActiveBusinessPartner(ProfileResponse businessPartner,
    BulkProcess bulkProcess, String excelFileName) throws ApplicationException {
    if (!ACTIVE_MERCHANT.equals(businessPartner.getMerchantStatus())) {
      bulkProcess.setDescription(excelFileName + ". " + FILE_FROM_INACTIVE_SELLER);
      log.error("File upload is for inactive seller. File : {} for process id : {}, "
          + "seller code : {}, merchantStatus : {} ", excelFileName,
        bulkProcess.getBulkProcessCode(),
        bulkProcess.getBusinessPartnerCode(), businessPartner.getMerchantStatus());
      throw new ApplicationException(ErrorCategory.VALIDATION,
        FILE_FROM_INACTIVE_SELLER + " " + PROCESS_ABORTED);
    }
  }

  public static void readInfoSalesSheetAndCreateBulkProcessData(
    BulkProcessExternalUploadRequest bulkProcessExternalUploadRequest,
    Map<String, List<Map<String, String>>> productSalesInfoMap, BulkProcess bulkProcess,
    Map<String, StringBuilder> productValidationErrorMap, ProfileResponse businessPartner,
    Map<String, List<Map<String, String>>> productBasicInfoMap, PlatformConfig shopeeConfig,
    Map<String, String> sourceColumnToDestinationColumnMap,
    Map<String, List<Map<String, String>>> productMediaInfoMap, Set<String> imageUrls,
    Map<String, List<Map<String, String>>> productShippingInfoMap,
    Map<String, BrandAndCategoryPredictionRequest> productIdToImageQCModelMap,
    List<BulkProcessData> bulkProcessDataList) throws JsonProcessingException {
    for (Map.Entry<String, List<Map<String, String>>> productSalesInfoEntry :
      productSalesInfoMap.entrySet()) {
      String productId = productSalesInfoEntry.getKey();
      List<Map<String, String>> variants =
        Optional.ofNullable(productSalesInfoEntry.getValue()).orElse(new ArrayList<>());
      for (Map<String, String> variant : variants) {
        StringBuilder variantErrorMessage = new StringBuilder();
        String pickupPointCode = bulkProcessExternalUploadRequest.getPickupPointCode();
        if (StringUtils.isBlank(pickupPointCode)) {
          variantErrorMessage.append("Silakan pilih alamat pengambilan yang sesuai untuk tiap "
            + "varian. ");
        }
        if (CommonUtils.hasVariantCreatingAttributeOrMultiVariant(variant, variants)) {
          variantErrorMessage.append(
            "Silakan isi atribut varian sesuai dengan kolomnya. ");
        }
        BulkProcessData bulkProcessData =
          getBulkProcessDataForExternalUpload(variant, bulkProcess, productId,
            productValidationErrorMap, variantErrorMessage);
        Map<String, Object> dataMap =
          getBulkProcessDataRequestMap(productId, pickupPointCode, businessPartner);
        populateBulkProcessDataRequestMap(variant, productBasicInfoMap, productId, shopeeConfig,
          dataMap, sourceColumnToDestinationColumnMap, productMediaInfoMap, bulkProcessData,
          imageUrls, productShippingInfoMap, productIdToImageQCModelMap, bulkProcess);
        bulkProcessData.setBulkRequestData(OBJECT_MAPPER.writeValueAsString(dataMap));
        bulkProcessDataList.add(bulkProcessData);
      }
    }
  }

  private static BulkProcessData getBulkProcessDataForExternalUpload(Map<String, String> variant,
    BulkProcess bulkProcess, String productId, Map<String, StringBuilder> productValidationErrorMap,
    StringBuilder variantErrorMessage) {
    BulkProcessData bulkProcessData = new BulkProcessData();
    bulkProcessData.setBulkProcessId(bulkProcess.getId());
    bulkProcessData.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    bulkProcessData.setStoreId(bulkProcess.getStoreId());
    bulkProcessData.setStartDate(new Date());
    bulkProcessData.setParentProduct(productId);
    bulkProcessData.setNotes(StringUtils.EMPTY);
    bulkProcessData.setRowNumber(
      Integer.valueOf(variant.getOrDefault(ROW_NUMBER, DEFAULT_ROW_NUMBER)));
    String errorMessage =
      productValidationErrorMap.getOrDefault(productId, new StringBuilder()).toString()
        + variantErrorMessage;
    if (StringUtils.isNotBlank(errorMessage)) {
      BulkUpdateServiceUtil.setFinalStatusForInputFailure(bulkProcessData, bulkProcess,
        errorMessage, 1, 0);
    } else {
      bulkProcessData.setStatus(BulkProcessData.STATUS_PENDING);
    }
    bulkProcessData.setRequestId(bulkProcess.getRequestId());
    return bulkProcessData;
  }

  private static Map<String, Object> getBulkProcessDataRequestMap(String productId,
    String pickupPointCode, ProfileResponse businessPartner) {
    Map<String, Object> dataMap = new LinkedHashMap<>();
    //Default values
    String emptyString = StringUtils.EMPTY;
    dataMap.put(GenericBulkHeaders.CATEGORY, emptyString);
    dataMap.put(GenericBulkHeaders.C2, emptyString);
    dataMap.put(GenericBulkHeaders.CN, emptyString);
    dataMap.put(GenericBulkHeaders.UPC, emptyString);
    dataMap.put(GenericBulkHeaders.USP, emptyString);
    dataMap.put(GenericBulkHeaders.BRAND, emptyString);
    dataMap.put(GenericBulkHeaders.WARNA, emptyString);
    dataMap.put(GenericBulkHeaders.FAMILY_COLOUR, emptyString);
    dataMap.put(GenericBulkHeaders.UKURAN, emptyString);
    dataMap.put(GenericBulkHeaders.PARENT, productId);
    dataMap.put(GenericBulkHeaders.VARIANT_IMAGE_ID, emptyString);
    dataMap.put(GenericBulkHeaders.URL_VIDEO, emptyString);
    dataMap.put(GenericBulkHeaders.PRODUCT_TYPE,
      BulkUploadOption.SHIPPING_TYPE_ASSIGNED_BY_BLIBLI.getdescription());
    dataMap.put(GenericBulkHeaders.PICKUP_POINT, pickupPointCode);
    dataMap.put(GenericBulkHeaders.DELIVERY_STATUS, Constant.DEFAULT_DELIVERY_STATUS);
    dataMap.put(GenericBulkHeaders.ATTRIBUTE_NAME + " 1", emptyString);
    dataMap.put(GenericBulkHeaders.ATTRIBUTE_VALUE + " 1", emptyString);
    dataMap.put(GenericBulkHeaders.ATTRIBUTE_NAME + " 2", emptyString);
    dataMap.put(GenericBulkHeaders.ATTRIBUTE_VALUE + " 2", emptyString);
    dataMap.put(GenericBulkHeaders.ATTRIBUTE_NAME + " 3", emptyString);
    dataMap.put(GenericBulkHeaders.ATTRIBUTE_VALUE + " 3", emptyString);
    dataMap.put(GenericBulkHeaders.ATTRIBUTE_NAME + " 4", emptyString);
    dataMap.put(GenericBulkHeaders.ATTRIBUTE_VALUE + " 4", emptyString);
    dataMap.put(GenericBulkHeaders.ATTRIBUTE_NAME + " 5", emptyString);
    dataMap.put(GenericBulkHeaders.ATTRIBUTE_VALUE + " 5", emptyString);
    if (CommonUtils.isInstoreEligibleSeller(businessPartner)) {
      dataMap.put(GenericBulkHeaders.INSTORE, "0");
    }
    if (CommonUtils.isCncSeller(businessPartner)) {
      dataMap.put(GenericBulkHeaders.CNC, "0");
    }
    return dataMap;
  }

  public static void populateBulkProcessDataRequestMap(Map<String, String> variant,
    Map<String, List<Map<String, String>>> productBasicInfoMap, String productId,
    PlatformConfig shopeeConfig, Map<String, Object> dataMap,
    Map<String, String> sourceColumnToDestinationColumnMap,
    Map<String, List<Map<String, String>>> productMediaInfoMap, BulkProcessData bulkProcessData,
    Set<String> imageUrls, Map<String, List<Map<String, String>>> productShippingInfoMap,
    Map<String, BrandAndCategoryPredictionRequest> productIdToImageQCModelMap,
    BulkProcess bulkProcess) {
    populateDataFromBasicInfoSheet(productBasicInfoMap, productId, shopeeConfig, dataMap,
      sourceColumnToDestinationColumnMap);
    populateDataFromMediaSheet(productId, shopeeConfig, dataMap, sourceColumnToDestinationColumnMap,
      productMediaInfoMap, bulkProcessData, imageUrls);
    populateDataFromShippingSheet(variant, productId, shopeeConfig, dataMap,
      sourceColumnToDestinationColumnMap, productShippingInfoMap);
    if (StringUtils.isNotBlank(
      String.valueOf(dataMap.getOrDefault(GenericBulkHeaders.PRODUCT_NAME, StringUtils.EMPTY)))) {
      populateProductIdToImageQCModelMap(productId, dataMap, bulkProcessData,
        productIdToImageQCModelMap, bulkProcess);
    }
  }

  public static void populateDataFromBasicInfoSheet(
    Map<String, List<Map<String, String>>> productBasicInfoMap, String productId,
    PlatformConfig shopeeConfig, Map<String, Object> dataMap,
    Map<String, String> sourceColumnToDestinationColumnMap) {
    List<Map<String, String>> productBasicInfoList =
      productBasicInfoMap.getOrDefault(productId, new ArrayList<>());
    if (CollectionUtils.isNotEmpty(productBasicInfoList)) {
      for (Map.Entry<String, String> productBasicInfoEntry : productBasicInfoList.getFirst()
        .entrySet()) {
        if (StringUtils.equals(productBasicInfoEntry.getKey(), shopeeConfig.getJoinKey())) {
          continue;
        }
        if (StringUtils.equals(productBasicInfoEntry.getKey(), ROW_NUMBER)) {
          continue;
        }
        dataMap.put(sourceColumnToDestinationColumnMap.getOrDefault(productBasicInfoEntry.getKey(),
          StringUtils.EMPTY), productBasicInfoEntry.getValue());
      }
    }
  }

  public static void populateDataFromMediaSheet(String productId, PlatformConfig shopeeConfig,
    Map<String, Object> dataMap, Map<String, String> sourceColumnToDestinationColumnMap,
    Map<String, List<Map<String, String>>> productMediaInfoMap, BulkProcessData bulkProcessData,
    Set<String> imageUrls) {
    List<Map<String, String>> productMediaList =
      productMediaInfoMap.getOrDefault(productId, new ArrayList<>());
    if (CollectionUtils.isNotEmpty(productMediaList)) {
      for (Map.Entry<String, String> productMediaInfoEntry : productMediaList.getFirst()
        .entrySet()) {
        if (StringUtils.equals(productMediaInfoEntry.getKey(), shopeeConfig.getJoinKey())) {
          continue;
        }
        if (StringUtils.equals(productMediaInfoEntry.getKey(), ROW_NUMBER)) {
          continue;
        }
        String destinationColumn =
          sourceColumnToDestinationColumnMap.getOrDefault(productMediaInfoEntry.getKey(),
            StringUtils.EMPTY);
        if (GenericBulkHeaders.IMAGE_PREFIX_SET.stream().anyMatch(destinationColumn::startsWith)
          && !StringUtils.equals(bulkProcessData.getStatus(), BulkProcessData.STATUS_FAIL)) {
          if(StringUtils.isNotBlank(productMediaInfoEntry.getValue())) {
            imageUrls.add(productMediaInfoEntry.getValue());
          }
        }
        dataMap.put(sourceColumnToDestinationColumnMap.getOrDefault(productMediaInfoEntry.getKey(),
          StringUtils.EMPTY), productMediaInfoEntry.getValue());
      }
    }
  }

  public static void populateDataFromShippingSheet(Map<String, String> variant, String productId,
    PlatformConfig shopeeConfig, Map<String, Object> dataMap,
    Map<String, String> sourceColumnToDestinationColumnMap,
    Map<String, List<Map<String, String>>> productShippingInfoMap) {
    List<Map<String, String>> productShippingList =
      productShippingInfoMap.getOrDefault(productId, new ArrayList<>());
    if (CollectionUtils.isNotEmpty(productShippingList)) {
      for (Map.Entry<String, String> productShippingInfoEntry : productShippingList.getFirst()
        .entrySet()) {
        if (StringUtils.equals(productShippingInfoEntry.getKey(), shopeeConfig.getJoinKey())) {
          continue;
        }
        if (StringUtils.equals(productShippingInfoEntry.getKey(), ROW_NUMBER)) {
          continue;
        }
        dataMap.put(
          sourceColumnToDestinationColumnMap.getOrDefault(productShippingInfoEntry.getKey(),
            StringUtils.EMPTY), productShippingInfoEntry.getValue());
      }
      for (Map.Entry<String, String> variantEntry : variant.entrySet()) {
        if (StringUtils.equals(variantEntry.getKey(), shopeeConfig.getJoinKey())) {
          continue;
        }
        if (StringUtils.equals(variantEntry.getKey(), ROW_NUMBER)) {
          continue;
        }
        dataMap.put(
          sourceColumnToDestinationColumnMap.getOrDefault(variantEntry.getKey(), StringUtils.EMPTY),
          variantEntry.getValue());
      }
    }
  }

  public static void populateProductIdToImageQCModelMap(String productId,
    Map<String, Object> dataMap, BulkProcessData bulkProcessData,
    Map<String, BrandAndCategoryPredictionRequest> productIdToImageQCModelMap,
    BulkProcess bulkProcess) {
    if (!productIdToImageQCModelMap.containsKey(bulkProcessData.getParentProduct())) {
      BrandAndCategoryPredictionRequest brandAndCategoryPredictionRequest =
        new BrandAndCategoryPredictionRequest();
      brandAndCategoryPredictionRequest.setStoreId(bulkProcess.getStoreId());
      brandAndCategoryPredictionRequest.setBulkProcessId(bulkProcess.getId());
      brandAndCategoryPredictionRequest.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      brandAndCategoryPredictionRequest.setParentProduct(bulkProcessData.getParentProduct());
      brandAndCategoryPredictionRequest.setExternalCategory(String.valueOf(
        dataMap.getOrDefault(ExcelHeaderNames.EXTERNAL_CATEGORY, StringUtils.EMPTY)));
      brandAndCategoryPredictionRequest.setProductName(
        String.valueOf(dataMap.getOrDefault(GenericBulkHeaders.PRODUCT_NAME, StringUtils.EMPTY)));
      brandAndCategoryPredictionRequest.setProductDescription(
        String.valueOf(dataMap.getOrDefault(GenericBulkHeaders.DESCRIPTION, StringUtils.EMPTY)));
      productIdToImageQCModelMap.put(productId, brandAndCategoryPredictionRequest);
    }
  }

  public static List<BulkProcessImage> getBulkProcessImageList(Set<String> imageUrls,
    BulkProcess bulkProcess) {
    List<BulkProcessImage> bulkProcessImageList = new ArrayList<>();
    int index = 1;
    for (String url : imageUrls) {
      BulkProcessImage bulkProcessImage = new BulkProcessImage();
      bulkProcessImage.setBulkProcessId(bulkProcess.getId());
      bulkProcessImage.setBulkProcessCode(bulkProcess.getBulkProcessCode());
      bulkProcessImage.setCompleted(false);
      bulkProcessImage.setImageURL(url);
      bulkProcessImage.setStoreId(bulkProcess.getStoreId());
      bulkProcessImage.setSequence(index);
      index = index + 1;
      bulkProcessImageList.add(bulkProcessImage);
    }
    return bulkProcessImageList;
  }

  public static Map<String, Integer> getRequiredColumnIndexMap(String zipFileName, String fileName,
    Map<String, String> sourceColumnToDestinationColumnMap,
    Map<String, Boolean> sourceColumnToMandatoryMap, SheetConfig config,
    Map<String, Integer> headerIndexMap) {
    Map<String, Integer> requiredColumnIndexMap = new HashMap<>();
    for (ColumnConfig colConfig : config.getColumns()) {
      String srcCol = colConfig.getSourceColumn();
      Integer index = headerIndexMap.get(srcCol.toLowerCase());
      if (Objects.isNull(index)) {
        Integer altIdx;
        for (String altSrc : colConfig.getAlternateSourceColumn()) {
          altIdx = headerIndexMap.get(altSrc.toLowerCase());
          if (Objects.nonNull(altIdx)) {
            srcCol = altSrc;
            index = altIdx;
            break;
          }
        }
      }
      sourceColumnToDestinationColumnMap.put(srcCol, colConfig.getDestinationColumn());
      sourceColumnToMandatoryMap.put(srcCol, colConfig.isMandatory());
      if (Objects.nonNull(index)) {
        requiredColumnIndexMap.put(srcCol, index);
      } else if (colConfig.isMandatory()) {
        throw new ValidationException(ErrorCategory.VALIDATION.getCode(),
          colConfig.getSourceColumn() + " tidak ditemukan di " + fileName + ". Process - "
            + zipFileName);
      }
    }
    return requiredColumnIndexMap;
  }

  public static void distributeSheetDataToMap(
    Map<String, List<Map<String, String>>> productBasicInfoMap,
    Map<String, List<Map<String, String>>> productMediaInfoMap,
    Map<String, List<Map<String, String>>> productShippingInfoMap,
    Map<String, List<Map<String, String>>> productSalesInfoMap, String fileName, String sheetType,
    Map<String, List<Map<String, String>>> sheetData) {
    switch (sheetType) {
      case BASIC_INFO_SHEET_TYPE:
        productBasicInfoMap.putAll(sheetData);
        break;
      case MEDIA_SHEET_TYPE:
        productMediaInfoMap.putAll(sheetData);
        break;
      case SHIPPING_SHEET_TYPE:
        productShippingInfoMap.putAll(sheetData);
        break;
      case SALES_SHEET_TYPE:
        productSalesInfoMap.putAll(sheetData);
        break;
      default:
        log.warn("Unknown sheetType {} for file {} ", sheetType, fileName);
    }
  }

  public static BulkProcessImageQC getBulkProcessImageQC(
    Map.Entry<String, BrandAndCategoryPredictionRequest> brandAndCategoryPredictionEntry,
    BulkProcess bulkProcess) {
    BrandAndCategoryPredictionRequest brandAndCategoryPredictionRequest =
      brandAndCategoryPredictionEntry.getValue();
    BulkProcessImageQC bulkProcessImageQC = new BulkProcessImageQC();
    bulkProcessImageQC.setBulkProcessCode(bulkProcess.getBulkProcessCode());
    bulkProcessImageQC.setParentProduct(brandAndCategoryPredictionEntry.getKey());
    bulkProcessImageQC.setExternalCategory(brandAndCategoryPredictionRequest.getExternalCategory());
    bulkProcessImageQC.setCompleted(false);
    bulkProcessImageQC.setStoreId(bulkProcess.getStoreId());
    return bulkProcessImageQC;
  }

  public static ProductCreationType getProductCreationTypeFromBulkProcessType(
    BulkProcessType bulkProcessType) {
    if (Objects.isNull(bulkProcessType)) {
      return ProductCreationType.UNIFIED_BULK_UPLOAD;
    }
    return switch (bulkProcessType) {
      case EXTERNAL_CREATION_UPLOAD -> ProductCreationType.EXTERNAL_BULK_UPLOAD;
      case CONVERTED_PRODUCT_CREATION_UPLOAD -> ProductCreationType.CONVERTED_BULK_UPLOAD;
      default -> ProductCreationType.UNIFIED_BULK_UPLOAD;
    };
  }

  public static String generateConvertedTemplateUploadPath(String gcsDownloadFailedProductsPath,
    BulkProcess bulkProcess) {
    Date createdDate = bulkProcess.getCreatedDate();
    SimpleDateFormat dateFormat = new SimpleDateFormat(EXTERNAL_UPLOAD_DATE_FORMAT);
    SimpleDateFormat timeFormat = new SimpleDateFormat(EXTERNAL_UPLOAD_TIME_FORMAT);
    String dateStr = dateFormat.format(createdDate);
    String timeStr = timeFormat.format(createdDate);
    return gcsDownloadFailedProductsPath + bulkProcess.getBulkProcessCode() + Constant.SLASH
      + Constant.CONVERTED_GENERAL_TEMPLATE + Constant.UNDERSCORE
      + bulkProcess.getBusinessPartnerCode() + Constant.UNDERSCORE + dateStr + timeStr
      + Constant.DOT + Constant.FILE_TYPE_XLSM;
  }

  public static boolean hasVariantCreatingAttributeOrMultiVariant(Map<String, String> variant,
    List<Map<String, String>> variants) {
    return variants.size() > 1 || (variants.size() == 1 && StringUtils.isNotBlank(
      variant.getOrDefault(ExcelHeaderNames.VARIASI_HEADER, StringUtils.EMPTY)));
  }

  public static String generateExternalTemplateUploadPath(
    BulkProcess bulkProcess) {
    Date createdDate = bulkProcess.getCreatedDate();
    SimpleDateFormat dateFormat = new SimpleDateFormat(EXTERNAL_UPLOAD_DATE_FORMAT);
    SimpleDateFormat timeFormat = new SimpleDateFormat(EXTERNAL_UPLOAD_TIME_FORMAT);
    String dateStr = dateFormat.format(createdDate);
    String timeStr = timeFormat.format(createdDate);
    return Constant.EXTERNAL_GENERAL_TEMPLATE + Constant.UNDERSCORE
      + bulkProcess.getBusinessPartnerCode() + Constant.UNDERSCORE + dateStr + timeStr;
  }
}
