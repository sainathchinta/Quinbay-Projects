package com.gdn.mta.bulk.util;

import java.lang.reflect.Field;
import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Collectors;

import com.gdn.mta.bulk.models.download.responsedata.BulkDownloadProductBasicInfoResponse;
import com.gdn.mta.bulk.models.download.responsedata.PickupPointModel;
import com.gdn.mta.bulk.models.download.responsedata.ProductBasicInfoResponse;

import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.DataValidation;
import org.apache.poi.ss.usermodel.DataValidationConstraint;
import org.apache.poi.ss.usermodel.DataValidationHelper;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddressList;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.BulkUploadOption;
import com.gdn.mta.bulk.MerchantStatusType;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.DownloadType;
import com.gdn.mta.bulk.models.FileType;
import com.gdn.mta.bulk.models.InternalBulkProcessFailedData;
import com.gdn.mta.bulk.models.StoreCopyFailedProducts;
import com.gdn.mta.bulk.models.UpdateSalesCategoryFailedProduct;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.InternalProcessFailedProductsDownloadRequest;
import com.gdn.mta.bulk.models.download.RecatFailedProductsDownloadRequest;
import com.gdn.mta.bulk.models.download.VendorSummaryDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.InternalProcessFailedProductResponse;
import com.gdn.mta.bulk.repository.BusinessPartnerRepository;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.mta.distributiontask.rest.model.request.FilterSummaryRequest;
import com.gdn.x.product.rest.web.model.dto.PreOrderDTO;

import lombok.extern.slf4j.Slf4j;

/**
 * Created by virajjasani on 01/09/16.
 */
@Component
@Slf4j
public class BulkDownloadServiceBeanUtil {

  private static final NumberFormat NUMBER_FORMAT = new DecimalFormat("0");
  private static final int FIRST_ROW = 1;
  private static final int LAST_ROW = 500000;
  private static final String DELIMETER = ".";
  private static final String LANGUAGE_ENGLISH = "en";
  private static final String INSTORE = "instore";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String LOGISTIC_ADJUSTMENT = "logisticAdjustment";
  private static final String VIDEO_URL = "videoUrl";
  private static final String WEIGHT = "weight";
  private static final String COMMON_IMAGES = "commonImageList";
  private static final String MAIN_IMAGE = "mainImage";
  private static final String COMMON_IMAGE_2 = "commonImage2";
  private static final String COMMON_IMAGE_3 = "commonImage3";
  private static final String COMMON_IMAGE_4 = "commonImage4";
  private static final String COMMON_IMAGE_5 = "commonImage5";
  private static final String COMMON_IMAGE_6 = "commonImage6";
  private static final String COMMON_IMAGE_7 = "commonImage7";
  private static final String COMMON_IMAGE_8 = "commonImage8";

  private static final Set<String> IMAGE_FIELDS =
      Set.of(MAIN_IMAGE, COMMON_IMAGE_2, COMMON_IMAGE_3, COMMON_IMAGE_4, COMMON_IMAGE_5, COMMON_IMAGE_6, COMMON_IMAGE_7,
          COMMON_IMAGE_8);


  @Value("${multipickuppoint.workflow.enabled}")
  private boolean multiPickupPointEnabled;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Value("${image.static.base.url.prefix}")
  private String imageStaticBaseUrlPrefix;

  @Value("${video.static.base.url.prefix}")
  private String videoStaticBaseUrlPrefix;

  @Value("${youtube.regex}")
  private String youtubeRegex;

  @Value("${pre.order.quota.feature.switch}")
  private boolean preOrderQuotaFeatureSwitch;

  @Autowired
  private BusinessPartnerRepository businessPartnerRepository;

  public List<List<String>> getAllProductValues(Map<String, Boolean> privilegedMap,
    boolean isBusinessPartnerO2O, List<ProductLevel3SummaryResponse> productSummaryResponseList,
      boolean isBlibliFulfillment, ProfileResponse profileResponse,
      List<PickupPointModel> ppCodesList) throws Exception {
    List<List<String>> productContentsForXLFile = new ArrayList<>();
    boolean isCncActivated = false;
    boolean isOnlyExternalUser = privilegedMap.getOrDefault(BulkParameters.IS_ONLY_EXTERNAL_USER, false);
    boolean isBusinessPartnerB2b = isMerchantB2b(profileResponse);
    boolean isOmgSeller = CommonUtils.isOMGSeller(profileResponse);
    Map<String, String> pickupPointCodeAndNameMap = ppCodesList.stream()
        .collect(Collectors.toMap(PickupPointModel::getCode, PickupPointModel::getName));
    if (multiPickupPointEnabled) {
      isCncActivated = profileResponse.getCompany().isCncActivated();
    }
    boolean privilegedReadWarehouseStock = false;
    if (privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_READ_WAREHOUSE_STOCK, false)) {
      privilegedReadWarehouseStock =
          (isBlibliFulfillment && privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_READ_WAREHOUSE_STOCK, false));
    }
    for (ProductLevel3SummaryResponse productSummary : productSummaryResponseList) {
      try {
        if (productSummary != null) {
          List<String> productData =
              getDefaultProductDataValues(productSummary, privilegedMap, pickupPointCodeAndNameMap);
          updatePriceStockProductType(privilegedMap, productSummary, productData, isOmgSeller);
          updatePickupDisplayBuyableCncActiveO2O(privilegedMap, isBusinessPartnerO2O, productSummary,
            productData, isCncActivated);
          if (privilegedReadWarehouseStock && productSummary.getAvailableStockLevel1() != null) {
            productData.add(productSummary.getAvailableStockLevel1().toString());
          } else if (privilegedReadWarehouseStock) {
            productData.add("-");
          }
          updateB2bFields(isOnlyExternalUser, isBusinessPartnerB2b, productSummary, productData);
          productContentsForXLFile.add(productData);
        }
      } catch (Exception ex) {
        log.error("Exception while setting data for product sku {} ", productSummary.getProductSku(),
            ex);
      }
    }
    return productContentsForXLFile;
  }

  private void updateB2bFields(boolean isOnlyExternalUser, boolean isBusinessPartnerB2b,
      ProductLevel3SummaryResponse productSummary, List<String> productData) {
    if (isBusinessPartnerB2b) {
      productData.add(setB2bBasePrice(productSummary));
      productData.add(setB2bManagedFlag(productSummary));
      productData.add(setBfbStatus(isOnlyExternalUser, productSummary));
    }
  }

  private static String setB2bManagedFlag(ProductLevel3SummaryResponse productLevel3SummaryResponse) {
    return Objects.nonNull(productLevel3SummaryResponse.getB2BResponse()) && Objects.nonNull(
        productLevel3SummaryResponse.getB2BResponse().getBasePrice()) ?
        (productLevel3SummaryResponse.getB2BResponse().isManaged() ?
            BulkParameters.BFB_MANAGED_TRUE :
            BulkParameters.BFB_MANAGED_FALSE) :
        Constant.HYPHEN;
  }

  private static String setB2bBasePrice(ProductLevel3SummaryResponse productLevel3SummaryResponse) {
    return Objects.nonNull(productLevel3SummaryResponse.getB2BResponse()) && Objects.nonNull(
        productLevel3SummaryResponse.getB2BResponse().getBasePrice()) ?
        String.valueOf(productLevel3SummaryResponse.getB2BResponse().getBasePrice()) :
        Constant.HYPHEN;
  }

  private String setBfbStatus(boolean isOnlyExternalUser, ProductLevel3SummaryResponse productLevel3SummaryResponse) {
    if (productLevel3SummaryResponse.getViewConfigs().stream()
        .anyMatch(viewConfigResponse -> Constant.B2B_CHANNEL.equals(viewConfigResponse.getChannelId()))) {
      ProductLevel3ViewConfigResponse productLevel3ViewConfigResponse =
          getItemViewConfigByChannel(productLevel3SummaryResponse, Constant.B2B_CHANNEL);
      boolean display = BooleanUtils.toBooleanDefaultIfNull(productLevel3ViewConfigResponse.getDisplay(), false);
      boolean buyable = BooleanUtils.toBooleanDefaultIfNull(productLevel3ViewConfigResponse.getBuyable(), false);
      return getSkuStatusValue(buyable, display, isOnlyExternalUser);
    } else {
      return Constant.HYPHEN;
    }
  }

  private static ProductLevel3ViewConfigResponse getItemViewConfigByChannel(
      ProductLevel3SummaryResponse productLevel3SummaryResponse, String channel) {
    return productLevel3SummaryResponse.getViewConfigs().stream()
        .filter(viewConfigResponse -> channel.equals(viewConfigResponse.getChannelId())).findFirst()
        .orElse(new ProductLevel3ViewConfigResponse());
  }

  private boolean isMerchantB2b(ProfileResponse profileResponse) {
    MerchantStatusType merchantStatusType = BulkCreationCommonUtil.getMerchantType(profileResponse);
    return merchantStatusType.getType() >= MerchantStatusType.BFB.getType();
  }

  public List<List<String>> getAllProductsDetailForStoreCopy(
      List<ProductLevel3SummaryResponse> productLevel3SummaryResponses) {
    return productLevel3SummaryResponses.stream()
        .map(productLevel3SummaryResponse -> getProductDetailForStoreCopy(productLevel3SummaryResponse))
        .collect(Collectors.toList());
  }

  private List<String> getProductDetailForStoreCopy(ProductLevel3SummaryResponse productLevel3SummaryResponse) {
    List<String> rowData = new ArrayList<>();
    rowData.add(productLevel3SummaryResponse.getProductSku());
    rowData.add(productLevel3SummaryResponse.getProductName());
    rowData.add(productLevel3SummaryResponse.getItemSku());
    rowData.add(productLevel3SummaryResponse.getItemName());
    rowData.add(productLevel3SummaryResponse.getSkuCode());
    rowData.add(productLevel3SummaryResponse.getProductCode());
    rowData.add(StringUtils.EMPTY);
    rowData.add(productLevel3SummaryResponse.getMerchantSku());
    rowData.add(String.format("%.0f", productLevel3SummaryResponse.getPrices().get(0).getPrice()));
    rowData.add(String.format("%.0f", productLevel3SummaryResponse.getOriginalSellingPrice()));
    if (productLevel3SummaryResponse.getSynchronizeStock() == Boolean.TRUE) {
      rowData.add("0");
    } else {
      rowData.add(productLevel3SummaryResponse.getAvailableStockLevel2().toString());
    }
    rowData.add(StringUtils.EMPTY);
    if (Objects.nonNull(productLevel3SummaryResponse.getMinimumStockLevel2())) {
      rowData.add(productLevel3SummaryResponse.getMinimumStockLevel2().toString());
    } else {
      rowData.add(String.valueOf(0));
    }
    rowData.add(getSkuStatusValue(productLevel3SummaryResponse.getViewConfigs().get(0).getBuyable(),
        productLevel3SummaryResponse.getViewConfigs().get(0).getDisplay(), false));
    rowData.add(getShippingType(Optional.ofNullable(productLevel3SummaryResponse.getProductType()).orElse(0)));
    return rowData;
  }

  private String getShippingType(int productType) {
    if (productType == (int) BulkUploadOption.SHIPPING_TYPE_ASSIGNED_BY_BLIBLI.getValue()) {
      return BulkUploadOption.SHIPPING_TYPE_ASSIGNED_BY_BLIBLI.getdescription();
    } else if (productType == (int) BulkUploadOption.SHIPPING_TYPE_SPECIAL_SHIPPING.getValue()) {
      return BulkUploadOption.SHIPPING_TYPE_SPECIAL_SHIPPING.getdescription();
    } if (productType == (int) BulkUploadOption.SHIPPING_TYPE_BOPIS.getValue()) {
      return BulkUploadOption.SHIPPING_TYPE_BOPIS.getdescription();
    }
    return StringUtils.EMPTY;
  }

  private void updatePickupDisplayBuyableCncActiveO2O(Map<String, Boolean> privilegedMap,
      boolean isBusinessPartnerO2O, ProductLevel3SummaryResponse productSummary,
      List<String> productData, boolean isCncActivated) {
    if (cncForWarehouseFeatureSwitch) {
      ProductLevel3ViewConfigResponse defaultViewConfigResponse =
          getItemViewConfigByChannel(productSummary, Constant.DEFAULT_CHANNEL);
      String finalStatus = "0";
      String defaultStatus =
          getSkuStatusValue(defaultViewConfigResponse.getBuyable(), defaultViewConfigResponse.getDisplay(),
              privilegedMap.getOrDefault(BulkParameters.IS_ONLY_EXTERNAL_USER, false));
      if (isCncActivated) {
        ProductLevel3ViewConfigResponse cncViewConfigResponse =
            getItemViewConfigByChannel(productSummary, Constant.CNC);
        String cncStatus = getSkuStatusValue(cncViewConfigResponse.getBuyable(), cncViewConfigResponse.getDisplay(),
            privilegedMap.getOrDefault(BulkParameters.IS_ONLY_EXTERNAL_USER, false));
        if (BulkParameters.SKU_STATUS_OFFLINE.equals(cncStatus)) {
          finalStatus = defaultStatus;
        } else if (BulkParameters.SKU_STATUS_OFFLINE.equals(defaultStatus)) {
          finalStatus = cncStatus;
        } else if (defaultStatus.equals(cncStatus)) {
          finalStatus = defaultStatus;
        }
        if (privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_READ_DISPLAY_BUYABLE, false)) {
          productData.add(finalStatus);
          productData.add(BulkParameters.SKU_STATUS_OFFLINE.equals(defaultStatus) ?
              BulkParameters.SKU_STATUS_OFFLINE :
              BulkParameters.SKU_STATUS_ONLINE);
          productData.add(BulkParameters.SKU_STATUS_OFFLINE.equals(cncStatus) ?
              BulkParameters.SKU_STATUS_OFFLINE :
              BulkParameters.SKU_STATUS_ONLINE);
        }
      } else {
        if (privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_READ_DISPLAY_BUYABLE, false)) {
          productData.add(defaultStatus);
        }
      }
    } else {
      if (privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_READ_DISPLAY_BUYABLE, false)) {
        ProductLevel3ViewConfigResponse defaultViewConfigResponse =
            getItemViewConfigByChannel(productSummary, Constant.DEFAULT_CHANNEL);
        productData.add(getSkuStatusValue(defaultViewConfigResponse.getBuyable(), defaultViewConfigResponse.getDisplay(),
            privilegedMap.getOrDefault(BulkParameters.IS_ONLY_EXTERNAL_USER, false)));
      }
      if (isCncActivated) {
        productData.add(getStringForBoolean(productSummary.isCncActive()));
      }
    }
    if (isBusinessPartnerO2O && privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_READ_O2O, false)) {
      productData.add(getStringForBoolean(productSummary.getOff2OnActiveFlag()));
    }
  }

  private void updatePriceStockProductType(Map<String, Boolean> privilegedMap,
      ProductLevel3SummaryResponse productSummary, List<String> productData, boolean isOmgSeller) {
    if (privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_READ_PRICE, false)) {
      productData.add(NUMBER_FORMAT.format(productSummary.getPrices().get(0).getPrice()));
      productData.add(NUMBER_FORMAT.format(productSummary.getPrices().get(0).getSalePrice()));
    }
    if (privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_READ_STOCK, false)) {
      if (productSummary.getSynchronizeStock() == Boolean.TRUE) {
        productData.add("-");
      } else {
        productData.add(productSummary.getAvailableStockLevel2().toString());
      }
      setPoQuota(productData, productSummary, isOmgSeller);
      productData.add(
          Optional.ofNullable(productSummary.getMinimumStockLevel2()).orElse(0).toString());
    }
  }

  private void setPoQuota(List<String> productData, ProductLevel3SummaryResponse productLevel3SummaryResponse,
      boolean isOmgSeller) {
    if (isOmgSeller && preOrderQuotaFeatureSwitch) {
      PreOrderDTO preOrderDTO = PreOrderDTO.builder().isPreOrder(productLevel3SummaryResponse.isPreOrder())
          .preOrderDate(productLevel3SummaryResponse.getPreOrderDate()).build();
      if (CommonUtils.isPreOrderActive(preOrderDTO)) {
        productData.add(String.valueOf(productLevel3SummaryResponse.getAvailableStockLevel2()));
      } else {
        productData.add(Constant.HYPHEN);
      }
    }
  }

  private String getStringForBoolean(Boolean productValue) {
    if (productValue) {
      return BulkParameters.ENABLE_VALUE;
    }
    return BulkParameters.DISABLE_VALUE;
  }

  public String getSkuStatusValue(Boolean buyable, Boolean displayable, Boolean isPureExternal) {
    if (isPureExternal) {
      if (Boolean.FALSE.equals(buyable) || Boolean.FALSE.equals(displayable)) {
        return BulkParameters.SKU_STATUS_OFFLINE;
      } else {
        return BulkParameters.SKU_STATUS_ONLINE;
      }
    } else {
      if (Boolean.FALSE.equals(buyable) && Boolean.FALSE.equals(displayable)) {
        return BulkParameters.SKU_STATUS_OFFLINE;
      } else if (Boolean.TRUE.equals(buyable) && Boolean.FALSE.equals(displayable)) {
        return BulkParameters.SKU_STATUS_B2B;
      } else if (Boolean.FALSE.equals(buyable) && Boolean.TRUE.equals(displayable)) {
        return BulkParameters.SKU_STATUS_TEASER;
      } else if (Boolean.TRUE.equals(buyable) && Boolean.TRUE.equals(displayable)) {
        return BulkParameters.SKU_STATUS_ONLINE;
      }
    }
    return BulkParameters.SKU_STATUS_OFFLINE;
  }

  private List<String> getDefaultProductDataValues(ProductLevel3SummaryResponse productSummary,
      Map<String, Boolean> privilegedMap, Map<String, String> ppCodeAndNameMap) {
    List<String> productData = new ArrayList<>();
    productData.add(productSummary.getProductSku());
    productData.add(productSummary.getProductName());
    productData.add(productSummary.getItemSku());
    if (privilegedMap.getOrDefault(BulkParameters.PRIVILEGE_READ_PICKUP_POINT, false)) {
      productData.add(productSummary.getPickupPointCode());
      productData.add(
          ppCodeAndNameMap.getOrDefault(productSummary.getPickupPointCode(), StringUtils.EMPTY));
    }
    productData.add(productSummary.getItemName());
    if (!privilegedMap.getOrDefault(BulkParameters.IS_ONLY_EXTERNAL_USER, false)) {
      productData.add(productSummary.getSkuCode());
    }
    productData.add(productSummary.getMerchantSku());
    return productData;
  }

  public void generateValidationForWorkbook(Workbook workbook, int pickupPointSize, int pickupPointIndex,
      Boolean isPrivilegedToEditPickupPoint, String sheetName) {
    Sheet dataSheet = workbook.getSheet(BulkParameters.DATA_SHEET);
    if (isPrivilegedToEditPickupPoint) {
      CellRangeAddressList cellRangeAddressList =
          new CellRangeAddressList(FIRST_ROW, LAST_ROW, pickupPointIndex, pickupPointIndex);
      DataValidationHelper dataValidationHelper = dataSheet.getDataValidationHelper();
      DataValidationConstraint dataValidationConstraint = dataValidationHelper
          .createFormulaListConstraint(sheetName + "!$A$1:$A$" + (pickupPointSize + 1));
      DataValidation dataValidation =
          dataValidationHelper.createValidation(dataValidationConstraint, cellRangeAddressList);
      dataSheet.addValidationData(dataValidation);
    }
  }

  public BulkDownloadRequest getVendorBulkDownloadRequest(String emailTo, String emailCc,
      FilterSummaryRequest filterSummaryRequest, String requestId, String username) {
    String fileName = new StringBuilder().append(requestId).append(DELIMETER)
        .append(FileType.XLSX.name().toLowerCase()).toString();
    VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder builder =
        new VendorSummaryDownloadRequest.VendorSummaryDownloadRequestBuilder();
    BulkDownloadRequest vendorSummaryDownloadRequest =
        builder.keyword(filterSummaryRequest.getKeyword())
            .timeFilterType(filterSummaryRequest.getTimeFilterType().getTimeFilterType())
            .contentPending(filterSummaryRequest.getContentPending())
            .imagePending(filterSummaryRequest.getImagePending())
            .assignment(filterSummaryRequest.getAssignment())
            .categoryCode(filterSummaryRequest.getCategoryCode())
            .isCnCategory((filterSummaryRequest.getIsCnCategory()))
            .businessPartnerCode(filterSummaryRequest.getBusinessPartnerCode())
            .assigneeEmailId(filterSummaryRequest.getAssigneeEmailId()).vendorCode(filterSummaryRequest.getVendorCode())
            .sortOrderByCreatedDate(filterSummaryRequest.getSortOrderByCreatedDate())
            .postLive(filterSummaryRequest.getPostLive())
            .faultyImageType(filterSummaryRequest.getFaultyImageType())
            .brandPending(filterSummaryRequest.getBrandPending()).unrestrictedDownload(true)
            .downloadType(DownloadType.ALL)
            .fileType(FileType.XLSX).bulkProcessType(BulkProcessEntity.VENDOR_FILTERED_PRODUCT)
            .directDownload(false).filename(fileName).emailTo(emailTo).emailCC(emailCc)
            .username(username).language(LANGUAGE_ENGLISH).request(requestId).build();
    return vendorSummaryDownloadRequest;
  }

  public BulkDownloadRequest getRecatFailedProductsDownloadRequest(String recatRequestCode, String username, String requestId) {
    RecatFailedProductsDownloadRequest request = new RecatFailedProductsDownloadRequest();
    String fileName = new StringBuilder().append(Constant.FAILED_PRODUCTS).append(DELIMETER)
        .append(FileType.XLSX.name().toLowerCase()).toString();
    request.setRecatRequestCode(recatRequestCode);
    request.setFileType(FileType.XLSX);
    request.setBulkProcessEntity(BulkProcessEntity.RECAT_FAILED_PRODUCTS);
    request.setFilename(fileName);
    request.setUsername(username);
    request.setLanguage(LANGUAGE_ENGLISH);
    request.setRequestId(requestId);
    return request;
  }

  public BulkDownloadRequest getInternalProcessFailedDownloadRequest(String internalProcessRequestCode, String username,
      String requestId, String processType) {
    InternalProcessFailedProductsDownloadRequest request = new InternalProcessFailedProductsDownloadRequest();
    String fileName = new StringBuilder().append(Constant.FAILED_PRODUCTS).append(DELIMETER)
        .append(FileType.XLSX.name().toLowerCase()).toString();
    request.setInternalProcessRequestCode(internalProcessRequestCode);
    request.setFileType(FileType.XLSX);
    request.setFilename(fileName);
    request.setUsername(username);
    request.setLanguage(LANGUAGE_ENGLISH);
    request.setRequestId(requestId);
    request.setProcessType(processType);
    if (BulkInternalProcessType.STORE_COPY.name().equals(processType)) {
      request.setBulkProcessEntity(BulkProcessEntity.STORE_COPY_FAILED_PRODUCTS);
    }
    else if(BulkInternalProcessType.SALES_CATEGORY_UPDATE.name().equals(processType)) {
      request.setBulkProcessEntity(BulkProcessEntity.UPDATE_SALES_CATEGORY_FAILED_PRODUCTS);
    }
    return request;
  }

  public BulkDownloadRequest getTargetSellerTemplateForCopyStore(String sellerCode, String username, String requestId) {
    BulkDownloadRequest request = new BulkDownloadRequest();
    String fileName =
        new StringBuilder().append(sellerCode).append(DELIMETER).append(FileType.XLSX.name().toLowerCase()).toString();
    request.setMerchantId(sellerCode);
    request.setFileType(FileType.XLSX);
    request.setBulkProcessEntity(BulkProcessEntity.STORE_COPY_UPLOAD_TEMPLATE);
    request.setFilename(fileName);
    request.setUsername(username);
    request.setLanguage(LANGUAGE_ENGLISH);
    request.setRequestId(requestId);
    return request;
  }

  public void generateStoreCopyRows(InternalProcessFailedProductResponse internalProcessFailedProductResponse,
      List<List<String>> rowData) {
    for (InternalBulkProcessFailedData internalBulkProcessFailedData : internalProcessFailedProductResponse
        .getInternalBulkProcessFailedData()) {
      StoreCopyFailedProducts storeCopyFailedProduct = (StoreCopyFailedProducts) internalBulkProcessFailedData;
      List<String> row = new ArrayList<>();
      row.add(storeCopyFailedProduct.getProductSku());
      row.add(storeCopyFailedProduct.getProductName());
      row.add(storeCopyFailedProduct.getItemSku());
      row.add(storeCopyFailedProduct.getProductName());
      row.add(storeCopyFailedProduct.getItemCode());
      row.add(storeCopyFailedProduct.getProductCode());
      row.add(storeCopyFailedProduct.getCopyProductName());
      row.add(storeCopyFailedProduct.getSellerSku());
      row.add(String.valueOf(storeCopyFailedProduct.getListPrice()));
      row.add(String.valueOf(storeCopyFailedProduct.getOfferPrice()));
      row.add(String.valueOf(storeCopyFailedProduct.getStock()));
      row.add(storeCopyFailedProduct.getPickupPointCode());
      row.add(String.valueOf(storeCopyFailedProduct.getMinimumStock()));
      row.add(String.valueOf(storeCopyFailedProduct.getStatus()));
      row.add(storeCopyFailedProduct.getShippingType());
      row.add(storeCopyFailedProduct.getErrorMessage());
      rowData.add(row);
    }
  }

  public void generateUpdateSalesCategoryRows(
      InternalProcessFailedProductResponse internalProcessFailedProductResponse, List<List<String>> rowData) {
    for (InternalBulkProcessFailedData internalBulkProcessFailedData : internalProcessFailedProductResponse
        .getInternalBulkProcessFailedData()) {
      UpdateSalesCategoryFailedProduct updateSalesCategoryFailedProduct =
          (UpdateSalesCategoryFailedProduct) internalBulkProcessFailedData;
      List<String> row = new ArrayList<>();
      row.add(updateSalesCategoryFailedProduct.getProductSku());
      row.add(updateSalesCategoryFailedProduct.getOperationType());
      row.add(updateSalesCategoryFailedProduct.getCnCategoryCode());
      row.add(updateSalesCategoryFailedProduct.getErrorMessage());
      rowData.add(row);
    }
  }


  public List<List<String>> getAllProductBasicInfoValues(boolean isBusinessPartnerO2O,
      BulkDownloadProductBasicInfoResponse result, Map<String, String> categoryCodeAndHierarchyMap,
      Map<String, Integer> categoryCodeAndLogisticAdjustmentMap) throws IllegalAccessException {
    List<List<String>> productContentsForXLFile = new ArrayList<>();
    List<ProductBasicInfoResponse> productList = result.getProductBasicInfoResponseList();
    if (CollectionUtils.isEmpty(productList)) {
      return productContentsForXLFile;
    }
    for (ProductBasicInfoResponse product : productList) {
      List<String> productData = new ArrayList<>();
      String categoryCode = product.getCategoryCode();
      for (Field field : ProductBasicInfoResponse.class.getDeclaredFields()) {
        field.setAccessible(true);
        String fieldName = field.getName();
        processFieldForProductData(isBusinessPartnerO2O, categoryCodeAndHierarchyMap,
            categoryCodeAndLogisticAdjustmentMap, product, productData, categoryCode, field, fieldName);
      }
      productContentsForXLFile.add(productData);
    }
    return productContentsForXLFile;
  }

  public void processFieldForProductData(boolean isBusinessPartnerO2O, Map<String, String> categoryCodeAndHierarchyMap,
      Map<String, Integer> categoryCodeAndLogisticAdjustmentMap, ProductBasicInfoResponse product,
      List<String> productData, String categoryCode, Field field, String fieldName) throws IllegalAccessException {
    if (INSTORE.equals(fieldName) && !isBusinessPartnerO2O) {
      return;
    }
    if (COMMON_IMAGES.equals(fieldName)) {
      return;
    }
    switch (fieldName) {
      case CATEGORY_CODE:
        productData.add(categoryCodeAndHierarchyMap.getOrDefault(categoryCode, StringUtils.EMPTY));
        break;
      case LOGISTIC_ADJUSTMENT:
        productData.add(String.valueOf(categoryCodeAndLogisticAdjustmentMap.getOrDefault(categoryCode, 0)));
        break;
      case  WEIGHT:
        productData.add(
            Optional.ofNullable(product.getWeight()).map(weight -> String.valueOf(weight * 1000))
                .orElse(StringUtils.EMPTY));
        break;
      case VIDEO_URL:
        if (Objects.nonNull(field.get(product))) {
          processVideoUrlField(product, productData, field);
          break;
        } else {
          productData.add(StringUtils.EMPTY);
          break;
        }
      default:
        if (IMAGE_FIELDS.contains(fieldName)) {
          productData.add(formatValue(field.get(product), imageStaticBaseUrlPrefix));
        } else {
          productData.add(formatValue(field.get(product), null));
        }
        break;
    }
  }

  private void processVideoUrlField(ProductBasicInfoResponse product, List<String> productData, Field field)
      throws IllegalAccessException {
    if (StringUtils.isBlank(field.get(product).toString())) {
      productData.add(StringUtils.EMPTY);
    } else if (ValidateUrlUtil.validateYouTubeRegex(field.get(product).toString(), youtubeRegex)) {
      productData.add(formatValue(field.get(product), null));
    } else {
      productData.add(formatValue(field.get(product), videoStaticBaseUrlPrefix));
    }
  }

  private String formatValue(Object fieldValue, String baseUrlPrefix) {
    return Optional.ofNullable(fieldValue)
        .map(value -> StringUtils.isNotEmpty(baseUrlPrefix) ? baseUrlPrefix + value.toString() : value.toString())
        .orElse(StringUtils.EMPTY);
  }
}
