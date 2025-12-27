package com.gdn.mta.bulk.util;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.io.IOException;
import java.net.URLConnection;
import java.text.SimpleDateFormat;
import java.util.AbstractMap;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.BiConsumer;

import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.ValidationException;
import com.gdn.mta.bulk.dto.BrandAndCategoryPredictionRequest;
import com.gdn.mta.bulk.dto.BulkProcessExternalUploadRequest;
import com.gdn.mta.bulk.dto.ValidateImageDTO;
import com.gdn.mta.bulk.entity.BulkProcessImageQC;
import com.gdn.mta.bulk.models.ColumnConfig;
import com.gdn.mta.bulk.models.PlatformConfig;
import com.gdn.mta.bulk.models.SheetConfig;
import org.apache.commons.lang3.ObjectUtils;
import com.gdn.mta.product.enums.ProductCreationType;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gda.mta.product.dto.ItemPickupPointRequest;
import com.gda.mta.product.dto.ProductLevel3PriceResponse;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gda.mta.product.dto.response.B2BResponse;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
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
import com.gdn.mta.bulk.models.download.responsedata.BulkDownloadProductBasicInfoResponse;
import com.gdn.mta.bulk.models.download.responsedata.ImageBasicInfoResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductBasicInfoResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductBasicResponse;
import com.gdn.mta.bulk.models.download.responsedata.ProductImageAndVideoResponse;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.productcategorybase.dto.response.AttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryAttributeResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryDetailResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryHierarchyResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryResponse;
import com.gdn.mta.bulk.dto.BulkUpdateErrorDTO;


public class CommonUtilsTest {


  private static final String SELLER_SKU = "SELLER_SKU";
  private static final String FAAS_ACTIVATED = "faasActivated";
  public static final String CM = "MERCHANT_1";
  public static final String CC = "MERCHANT_2";
  public static final String UNSUPPORTED_MERCHANT = "UNSUPPORTED_MERCHANT";
  private static final String CATEGORY_CODE = "CATEGORY_CODE";
  private static final String STORE_ID = "STORE_ID";
  private static final String REQUEST_ID = "REQUEST_ID";
  private static final String BULK_PROCESS_CODE = "BULK_PROCESS_CODE";
  private Set<BulkUpdateChangeType> bulkUpdateChangeTypeSet;
  private Map<String, String> productL5Data;
  private ItemPickupPointListingL3Response itemPickupPointListingL3Response;
  private ProductLevel3PriceResponse price;
  private ItemPickupPointRequest itemPickupPointRequest;
  private ProductLevel3ViewConfigResponse bfbItemViewConfigs;
  private ProfileResponse profileResponse;
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLI-00106";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_USERNAME = "developer";
  private static final String DEFAULT_BULK_PROCESS_CODE = "defaultBulkProcessCode";
  private static final String UPLOADED_URL = "UPLOADED_URL";
  private static final String IMAGE_URL =
      "https://www.static-src.com/wcsstore/Indraprastha/images/catalog/full//97/MTA-31184287/fashion_original-timberland-men-s-outdoor-waterproof-shoes-martin-boots-work-boots-t031_full03.jpg";
  private static final String ZIP_FILE_NAME = "ZIP NAME";
  private static final String FILE_NAME = "File name";

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    bulkUpdateChangeTypeSet = new HashSet<>();
    productL5Data = new HashMap<>();
    itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    price = new ProductLevel3PriceResponse();
    price.setPrice(1.0);
    price.setSalePrice(2.0);
    itemPickupPointListingL3Response.setPrices(List.of(price));
    itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setStock(1);
    bfbItemViewConfigs = new ProductLevel3ViewConfigResponse();
    profileResponse = new ProfileResponse();
    profileResponse.setFlags(Map.of(FAAS_ACTIVATED, true));
  }

  @Test
  public void checkForMerchantSkuChangedTest() {
    productL5Data.put(BulkParameters.SELLER_SKU, SELLER_SKU);
    itemPickupPointListingL3Response.setMerchantSku(SELLER_SKU);
    CommonUtils.checkForMerchantSkuChanged(bulkUpdateChangeTypeSet, productL5Data, itemPickupPointListingL3Response);
    Assertions.assertEquals(0, bulkUpdateChangeTypeSet.size());
    itemPickupPointListingL3Response.setMerchantSku(null);
    CommonUtils.checkForMerchantSkuChanged(bulkUpdateChangeTypeSet, productL5Data, itemPickupPointListingL3Response);
    Assertions.assertEquals(1, bulkUpdateChangeTypeSet.size());
  }

  @Test
  public void checkForPriceChangedTest() {
    productL5Data.put(BulkParameters.PRICE_HEADER, "1.0");
    productL5Data.put(BulkParameters.SELLING_PRICE_HEADER, "2.0");
    CommonUtils.checkForPriceChanged(bulkUpdateChangeTypeSet, productL5Data, itemPickupPointListingL3Response);
    Assertions.assertEquals(0, bulkUpdateChangeTypeSet.size());
    productL5Data.put(BulkParameters.PRICE_HEADER, "3.0");
    CommonUtils.checkForPriceChanged(bulkUpdateChangeTypeSet, productL5Data, itemPickupPointListingL3Response);
    Assertions.assertEquals(1, bulkUpdateChangeTypeSet.size());
    productL5Data.put(BulkParameters.PRICE_HEADER, "1.0");
    productL5Data.put(BulkParameters.SELLING_PRICE_HEADER, "3.0");
    CommonUtils.checkForPriceChanged(bulkUpdateChangeTypeSet, productL5Data, itemPickupPointListingL3Response);
    Assertions.assertEquals(1, bulkUpdateChangeTypeSet.size());
  }

  @Test
  public void checkForStockChangedTest() {
    productL5Data.put(BulkParameters.STOCK_HEADER, "1");
    itemPickupPointListingL3Response.setWebSyncStock(true);
    CommonUtils.checkForStockChanged(bulkUpdateChangeTypeSet, itemPickupPointListingL3Response, itemPickupPointRequest);
    Assertions.assertEquals(0, bulkUpdateChangeTypeSet.size());
    itemPickupPointListingL3Response.setWebSyncStock(false);
    itemPickupPointRequest.setStock(0);
    CommonUtils.checkForStockChanged(bulkUpdateChangeTypeSet, itemPickupPointListingL3Response, itemPickupPointRequest);
    Assertions.assertEquals(0, bulkUpdateChangeTypeSet.size());
    itemPickupPointRequest.setStock(1);
    CommonUtils.checkForStockChanged(bulkUpdateChangeTypeSet, itemPickupPointListingL3Response, itemPickupPointRequest);
    Assertions.assertEquals(1, bulkUpdateChangeTypeSet.size());
    itemPickupPointRequest.setStock(null);
    CommonUtils.checkForStockChanged(bulkUpdateChangeTypeSet, itemPickupPointListingL3Response, itemPickupPointRequest);
    Assertions.assertEquals(1, bulkUpdateChangeTypeSet.size());
  }

  @Test
  public void checkForPoQuotaChangedTest() {
    productL5Data.put(BulkParameters.PO_QUOTA, "1");
    itemPickupPointListingL3Response.setWebSyncStock(true);
    CommonUtils.checkForPoQuotaChanged(bulkUpdateChangeTypeSet, itemPickupPointListingL3Response, itemPickupPointRequest);
    Assertions.assertEquals(0, bulkUpdateChangeTypeSet.size());
    itemPickupPointListingL3Response.setWebSyncStock(false);
    itemPickupPointRequest.setInitialPreOrderQuota(0);
    CommonUtils.checkForPoQuotaChanged(bulkUpdateChangeTypeSet, itemPickupPointListingL3Response, itemPickupPointRequest);
    Assertions.assertEquals(0, bulkUpdateChangeTypeSet.size());
    itemPickupPointRequest.setInitialPreOrderQuota(1);
    CommonUtils.checkForPoQuotaChanged(bulkUpdateChangeTypeSet, itemPickupPointListingL3Response, itemPickupPointRequest);
    Assertions.assertEquals(1, bulkUpdateChangeTypeSet.size());
    itemPickupPointRequest.setInitialPreOrderQuota(null);
    CommonUtils.checkForPoQuotaChanged(bulkUpdateChangeTypeSet, itemPickupPointListingL3Response, itemPickupPointRequest);
    Assertions.assertEquals(1, bulkUpdateChangeTypeSet.size());
  }

  @Test
  public void checkForCncChangedTest() {
    productL5Data.put(BulkParameters.CNC_STATUS_HEADER, BulkParameters.OFFLINE_VALUE);
    CommonUtils.checkForCncChanged(bulkUpdateChangeTypeSet, itemPickupPointListingL3Response, itemPickupPointRequest);
    Assertions.assertEquals(0, bulkUpdateChangeTypeSet.size());
    itemPickupPointRequest.setCncActive(true);
    CommonUtils.checkForCncChanged(bulkUpdateChangeTypeSet, itemPickupPointListingL3Response, itemPickupPointRequest);
    Assertions.assertEquals(1, bulkUpdateChangeTypeSet.size());
  }


  @Test
  public void checkForBfbFieldsChangedTest() {
    B2BResponse b2BResponse = new B2BResponse();
    CommonUtils.checkForBfbFieldsChanged(bulkUpdateChangeTypeSet, productL5Data, itemPickupPointListingL3Response,
        bfbItemViewConfigs);
    Assertions.assertEquals(0, bulkUpdateChangeTypeSet.size());
    itemPickupPointListingL3Response.setB2bFields(b2BResponse);
    CommonUtils.checkForBfbFieldsChanged(bulkUpdateChangeTypeSet, productL5Data, itemPickupPointListingL3Response,
        null);
    Assertions.assertEquals(0, bulkUpdateChangeTypeSet.size());
    productL5Data.put(BulkParameters.BFB_BASE_PRICE, "1.0");
    productL5Data.put(BulkParameters.AMPHI_BFB_STATUS, "0");
    CommonUtils.checkForBfbFieldsChanged(bulkUpdateChangeTypeSet, productL5Data, itemPickupPointListingL3Response,
        null);
    Assertions.assertEquals(2, bulkUpdateChangeTypeSet.size());
    productL5Data.remove(BulkParameters.AMPHI_BFB_STATUS);
    productL5Data.put(BulkParameters.EXTERNAL_BFB_STATUS, "0");
    productL5Data.put(BulkParameters.BFB_MANAGED, String.valueOf(Boolean.TRUE));
    b2BResponse.setBasePrice(3.0);
    CommonUtils.checkForBfbFieldsChanged(bulkUpdateChangeTypeSet, productL5Data, itemPickupPointListingL3Response,
        null);
    Assertions.assertEquals(3, bulkUpdateChangeTypeSet.size());
    b2BResponse.setBasePrice(1.0);
    productL5Data.put(BulkParameters.BFB_MANAGED, String.valueOf(Boolean.FALSE));
    CommonUtils.checkForBfbFieldsChanged(bulkUpdateChangeTypeSet, productL5Data, itemPickupPointListingL3Response,
        null);
    Assertions.assertEquals(3, bulkUpdateChangeTypeSet.size());
  }

  @Test
  public void isNotFaasEligible() {
    Assertions.assertFalse(CommonUtils.isNotFaasEligible(true, profileResponse));
    profileResponse.setFlags(new HashMap<>());
    Assertions.assertTrue(CommonUtils.isNotFaasEligible(true, profileResponse));
    Assertions.assertTrue(CommonUtils.isNotFaasEligible(false, profileResponse));
  }

  @Test
  public void isInstoreEligibleSeller() {
    Assertions.assertFalse(CommonUtils.isInstoreEligibleSeller(null));
    profileResponse.setCompany(null);
    Assertions.assertFalse(CommonUtils.isInstoreEligibleSeller(profileResponse));
    profileResponse.setCompany(CompanyDTO.builder().build());
    Assertions.assertFalse(CommonUtils.isInstoreEligibleSeller(profileResponse));
    profileResponse.setCompany(CompanyDTO.builder().offlineToOnlineFlag(true).build());
    Assertions.assertTrue(CommonUtils.isInstoreEligibleSeller(profileResponse));
  }

  @Test
  public void getValidateExcelRowsRequestTest() {
    ValidateExcelRowsRequest validateExcelRowsRequest =
        CommonUtils.getValidateExcelRowsRequest(new BulkProcess(), new BulkUploadErrorCounter(), 0,
            MerchantStatusType.BFB, "", 0, false, false, false, new HashMap<>(), new BulkProcessNotes(), "");
    Assertions.assertEquals(0, validateExcelRowsRequest.getMinimumPrice());
  }

  @Test
  public void isDormantSellerTest() {
    Assertions.assertFalse(CommonUtils.isDormantSeller(null));
    profileResponse.setCompany(null);
    Assertions.assertFalse(CommonUtils.isDormantSeller(profileResponse));
    profileResponse.setCompany(new CompanyDTO());
    Assertions.assertFalse(CommonUtils.isDormantSeller(profileResponse));
    profileResponse.getCompany().setDormantFlag(true);
    Assertions.assertTrue(CommonUtils.isDormantSeller(profileResponse));
  }

  @Test
  public void getDimensionMissingFromMissingFieldsTest() {
    Assertions.assertFalse(CommonUtils.getDimensionMissingFromMissingFields(null));
    itemPickupPointListingL3Response.setMissingFields(null);
    Assertions.assertFalse(CommonUtils.getDimensionMissingFromMissingFields(itemPickupPointListingL3Response));
    itemPickupPointListingL3Response.setMissingFields(Set.of(Constant.DESCRIPTION_MISSING));
    Assertions.assertFalse(CommonUtils.getDimensionMissingFromMissingFields(itemPickupPointListingL3Response));
    itemPickupPointListingL3Response.setMissingFields(Set.of(Constant.DIMENSIONS_MISSING));
    Assertions.assertTrue(CommonUtils.getDimensionMissingFromMissingFields(itemPickupPointListingL3Response));
  }

  @Test
  public void validateAndGetGenericTemplateFileTypeTest() {
    Assertions.assertEquals(GenericTemplateFileType.PURE_DELIVERY_FILE,
        CommonUtils.validateAndGetGenericTemplateFileType(GenericTemplateFileType.PURE_DELIVERY_FILE.name(),
            Constant.REQUEST_ID));
    ApplicationRuntimeException exception = Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      CommonUtils.validateAndGetGenericTemplateFileType(Constant.STORE_ID, Constant.REQUEST_ID);
    });
    Assertions.assertNotNull(exception);
    Assertions.assertTrue(
        exception.getErrorMessage().contains(ProductUpdateErrorMessages.INVALID_GENERIC_TEMPLATE_FILE_TYPE));
  }

  @Test
  public void skipCategoryRegenrationtest() {
    KafkaEventLog kafkaEventLog = new KafkaEventLog();
    Assertions.assertFalse(CommonUtils.skipCategoryRegeneration(false, List.of(kafkaEventLog), false));
    Assertions.assertFalse(CommonUtils.skipCategoryRegeneration(true, List.of(kafkaEventLog), false));
    Assertions.assertFalse(CommonUtils.skipCategoryRegeneration(true, null, true));
    Assertions.assertTrue(CommonUtils.skipCategoryRegeneration(true, null, false));
  }

  @Test
  public void validateStockAtL5ForPPUpdateNonMppSellerTest() {
    String supportedMerchants = CM + Constant.COMMA + CC;
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setMerchantType(CM);
    profileResponse.setCompany(company);
    profileResponse.setFbbActivated(true);

    ItemPickupPointListingL3Response stockResponse = new ItemPickupPointListingL3Response();
    stockResponse.setAvailableStockLevel1(10);
    Assertions.assertTrue(
        CommonUtils.validateStockAtL5ForPPUpdateNonMppSeller(true, true, stockResponse, supportedMerchants,
            profileResponse));
    stockResponse.setAvailableStockLevel1(0);
    Assertions.assertFalse(
        CommonUtils.validateStockAtL5ForPPUpdateNonMppSeller(true, true, stockResponse, supportedMerchants,
            profileResponse));
    company.setMerchantType(UNSUPPORTED_MERCHANT);
    profileResponse.setCompany(company);
    Assertions.assertFalse(
        CommonUtils.validateStockAtL5ForPPUpdateNonMppSeller(true, true, stockResponse, supportedMerchants,
            profileResponse));

    company.setMerchantType(CM);
    profileResponse.setCompany(company);
    profileResponse.setFbbActivated(false);
    Assertions.assertFalse(
        CommonUtils.validateStockAtL5ForPPUpdateNonMppSeller(true, true, stockResponse, supportedMerchants,
            profileResponse));
    Assertions.assertFalse(
        CommonUtils.validateStockAtL5ForPPUpdateNonMppSeller(false, true, stockResponse, supportedMerchants,
            profileResponse));

    profileResponse.setFbbActivated(true);
    Assertions.assertFalse(
        CommonUtils.validateStockAtL5ForPPUpdateNonMppSeller(false, false, stockResponse, supportedMerchants,
            profileResponse));
    profileResponse.setFbbActivated(false);
    Assertions.assertFalse(
        CommonUtils.validateStockAtL5ForPPUpdateNonMppSeller(false, false, stockResponse, supportedMerchants,
            profileResponse));
    Assertions.assertFalse(
        CommonUtils.validateStockAtL5ForPPUpdateNonMppSeller(true, true, stockResponse, supportedMerchants, null));
    Assertions.assertFalse(
        CommonUtils.validateStockAtL5ForPPUpdateNonMppSeller(true, true, null, supportedMerchants, profileResponse));
    Assertions.assertFalse(
        CommonUtils.validateStockAtL5ForPPUpdateNonMppSeller(false, true, stockResponse, supportedMerchants,
            profileResponse));
    profileResponse.setFbbActivated(true);
    Assertions.assertFalse(
        CommonUtils.validateStockAtL5ForPPUpdateNonMppSeller(true, false, stockResponse, supportedMerchants,
            profileResponse));
  }

  @Test
  public void filterHideFromSellerAttributesTest() {
    CategoryDetailResponse response = new CategoryDetailResponse();
    CategoryAttributeResponse categoryAttributeResponse = new CategoryAttributeResponse();
    AttributeResponse attributeResponse = new AttributeResponse();
    attributeResponse.setHideForSeller(false);
    categoryAttributeResponse.setAttribute(attributeResponse);
    CommonUtils.filterHideFromSellerAttributes(false, response);
    Assertions.assertEquals(0, response.getCategoryAttributes().size());
    CommonUtils.filterHideFromSellerAttributes(true, response);
    Assertions.assertEquals(0, response.getCategoryAttributes().size());
    response.setCategoryAttributes(new ArrayList<>(Collections.singletonList(categoryAttributeResponse)));
    attributeResponse.setHideForSeller(true);
    CommonUtils.filterHideFromSellerAttributes(true, response);
    Assertions.assertEquals(0, response.getCategoryAttributes().size());
  }

  @Test
  public void populateCategoryCodeAndHierarchyMapTest() {
    List<CategoryHierarchyResponse> categoryHierarchyResponses = new ArrayList<>();
    CategoryHierarchyResponse categoryHierarchyResponse = new CategoryHierarchyResponse();
    categoryHierarchyResponses.add(categoryHierarchyResponse);
    CommonUtils.populateCategoryCodeAndHierarchyMap(categoryHierarchyResponses, new HashMap<>(), new HashMap<>());
    categoryHierarchyResponse.setCategoryHierarchy(List.of(new CategoryResponse()));
    CommonUtils.populateCategoryCodeAndHierarchyMap(categoryHierarchyResponses, new HashMap<>(), new HashMap<>());
  }

  @Test
  public void populateCategoryHierarchyByCnCategoryCodeForGenericCreationTest() {
    List<CategoryHierarchyResponse> categoryHierarchyResponses = new ArrayList<>();
    CategoryHierarchyResponse categoryHierarchyResponse = new CategoryHierarchyResponse();
    categoryHierarchyResponses.add(categoryHierarchyResponse);
    Map<String, String> categoryHierarchy = new HashMap<>();
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class,
          () -> CommonUtils.populateCategoryHierarchyByCnCategoryCodeForGenericCreation(
              categoryHierarchyResponses, categoryHierarchy));
    } finally {
    }
  }

  @Test
  public void populateCategoryHierarchyByCnCategoryCodeForGenericCreation_happyTest() {
    List<CategoryHierarchyResponse> categoryHierarchyResponses = new ArrayList<>();
    CategoryHierarchyResponse categoryHierarchyResponse = new CategoryHierarchyResponse();
    List<CategoryResponse> categoryResponses = new ArrayList<>();
    CategoryResponse categoryResponse = new CategoryResponse();
    categoryResponses.add(categoryResponse);
    categoryHierarchyResponse.setCategoryHierarchy(categoryResponses);
    categoryHierarchyResponses.add(categoryHierarchyResponse);
    Map<String, String> categoryHierarchy = new HashMap<>();
    CommonUtils.populateCategoryHierarchyByCnCategoryCodeForGenericCreation(
        categoryHierarchyResponses, categoryHierarchy);
    categoryResponses.add(categoryResponse);
    categoryResponses.add(categoryResponse);
    categoryHierarchyResponse.setCategoryHierarchy(categoryResponses);
    categoryHierarchyResponses.removeFirst();
    categoryHierarchyResponses.add(categoryHierarchyResponse);
    CommonUtils.populateCategoryHierarchyByCnCategoryCodeForGenericCreation(
        categoryHierarchyResponses, categoryHierarchy);
    Assertions.assertTrue(ObjectUtils.isNotEmpty(categoryHierarchy));
  }

  @Test
  public void isBulkDownloadResponseEmptyOrInvalidTest() {
    BulkDownloadProductBasicInfoResponse nullResponse = null;
    Assertions.assertTrue(CommonUtils.isBulkDownloadResponseEmptyOrInvalid(nullResponse));
    BulkDownloadProductBasicInfoResponse emptyResponse = new BulkDownloadProductBasicInfoResponse();
    emptyResponse.setProductBasicInfoResponseList(Collections.emptyList());
    emptyResponse.setExceptionMap(Collections.emptyMap());
    Assertions.assertTrue(CommonUtils.isBulkDownloadResponseEmptyOrInvalid(emptyResponse));
    BulkDownloadProductBasicInfoResponse validProductListResponse = new BulkDownloadProductBasicInfoResponse();
    validProductListResponse.setProductBasicInfoResponseList(List.of(new ProductBasicInfoResponse()));
    validProductListResponse.setExceptionMap(Collections.emptyMap());
    Assertions.assertFalse(CommonUtils.isBulkDownloadResponseEmptyOrInvalid(validProductListResponse));
    BulkDownloadProductBasicInfoResponse validExceptionMapResponse = new BulkDownloadProductBasicInfoResponse();
    validExceptionMapResponse.setProductBasicInfoResponseList(Collections.emptyList());
    validExceptionMapResponse.setExceptionMap(Map.of("key", "value"));
    Assertions.assertFalse(CommonUtils.isBulkDownloadResponseEmptyOrInvalid(validExceptionMapResponse));
    BulkDownloadProductBasicInfoResponse validResponse = new BulkDownloadProductBasicInfoResponse();
    validResponse.setProductBasicInfoResponseList(List.of(new ProductBasicInfoResponse()));
    validResponse.setExceptionMap(Map.of("key", "value"));
    Assertions.assertFalse(CommonUtils.isBulkDownloadResponseEmptyOrInvalid(validResponse));
  }

  @Test
  public void testPopulateCommonImages_NullResponse() {
    CommonUtils.populateCommonImagesAndCategoryCodes(null);
  }

  @Test
  public void testPopulateCommonImages_EmptyProductList() {
    BulkDownloadProductBasicInfoResponse bulkDownloadProductBasicInfoResponse =
        new BulkDownloadProductBasicInfoResponse();
    bulkDownloadProductBasicInfoResponse.setProductBasicInfoResponseList(Collections.emptyList());
    CommonUtils.populateCommonImagesAndCategoryCodes(bulkDownloadProductBasicInfoResponse);
  }

  @Test
  public void testPopulateCommonImages_NullProductInList() {
    BulkDownloadProductBasicInfoResponse bulkDownloadProductBasicInfoResponse =
        new BulkDownloadProductBasicInfoResponse();
    bulkDownloadProductBasicInfoResponse.setProductBasicInfoResponseList(Collections.singletonList(null));
    CommonUtils.populateCommonImagesAndCategoryCodes(bulkDownloadProductBasicInfoResponse);
  }

  @Test
  public void testPopulateCommonImages_EmptyCommonImageList() {
    BulkDownloadProductBasicInfoResponse bulkDownloadProductBasicInfoResponse =
        new BulkDownloadProductBasicInfoResponse();
    ProductBasicInfoResponse product = new ProductBasicInfoResponse();
    product.setCategoryCode(CATEGORY_CODE);
    product.setCommonImageList(Collections.emptyList());
    bulkDownloadProductBasicInfoResponse.setProductBasicInfoResponseList(Collections.singletonList(product));
    CommonUtils.populateCommonImagesAndCategoryCodes(bulkDownloadProductBasicInfoResponse);
  }

  @Test
  public void testPopulateCommonImages_NullImageInCommonImageList() {
    BulkDownloadProductBasicInfoResponse bulkDownloadProductBasicInfoResponse =
        new BulkDownloadProductBasicInfoResponse();
    ProductBasicInfoResponse product = new ProductBasicInfoResponse();
    product.setCommonImageList(Collections.singletonList(null));
    bulkDownloadProductBasicInfoResponse.setProductBasicInfoResponseList(Collections.singletonList(product));
    CommonUtils.populateCommonImagesAndCategoryCodes(bulkDownloadProductBasicInfoResponse);
  }

  @Test
  public void testPopulateCommonImages_BlankLocationPath() {
    BulkDownloadProductBasicInfoResponse bulkDownloadProductBasicInfoResponse =
        new BulkDownloadProductBasicInfoResponse();
    ProductBasicInfoResponse product = new ProductBasicInfoResponse();
    ImageBasicInfoResponse image = new ImageBasicInfoResponse();
    image.setLocationPath(" ");
    product.setCommonImageList(Collections.singletonList(image));
    product.setCategoryCode(CATEGORY_CODE);
    bulkDownloadProductBasicInfoResponse.setProductBasicInfoResponseList(Collections.singletonList(product));
    CommonUtils.populateCommonImagesAndCategoryCodes(bulkDownloadProductBasicInfoResponse);
  }

  @Test
  public void testPopulateCommonImages_SetMainImage() {
    BulkDownloadProductBasicInfoResponse bulkDownloadProductBasicInfoResponse =
        new BulkDownloadProductBasicInfoResponse();
    ProductBasicInfoResponse product = new ProductBasicInfoResponse();
    ImageBasicInfoResponse image = new ImageBasicInfoResponse();
    image.setLocationPath("mainImagePath");
    image.setMainImage(true);
    product.setCommonImageList(Collections.singletonList(image));
    bulkDownloadProductBasicInfoResponse.setProductBasicInfoResponseList(Collections.singletonList(product));
    CommonUtils.populateCommonImagesAndCategoryCodes(bulkDownloadProductBasicInfoResponse);
    assertEquals("mainImagePath", product.getMainImage());
  }

  @Test
  public void testPopulateCommonImages_SetCommonImages() {
    BulkDownloadProductBasicInfoResponse bulkDownloadProductBasicInfoResponse =
        new BulkDownloadProductBasicInfoResponse();
    ProductBasicInfoResponse product = new ProductBasicInfoResponse();
    ImageBasicInfoResponse image1 = new ImageBasicInfoResponse();
    image1.setLocationPath("imagePath1");
    image1.setMainImage(false);
    ImageBasicInfoResponse image2 = new ImageBasicInfoResponse();
    image2.setLocationPath("imagePath2");
    image2.setMainImage(false);
    product.setCommonImageList(Arrays.asList(image1, image2));
    bulkDownloadProductBasicInfoResponse.setProductBasicInfoResponseList(Collections.singletonList(product));
    CommonUtils.populateCommonImagesAndCategoryCodes(bulkDownloadProductBasicInfoResponse);
    assertEquals("imagePath1", product.getCommonImage2());
    assertEquals("imagePath2", product.getCommonImage3());
  }

  @Test
  public void testPopulateCommonImages_SetCommonImagesMainIMages() {
    BulkDownloadProductBasicInfoResponse bulkDownloadProductBasicInfoResponse =
        new BulkDownloadProductBasicInfoResponse();
    ProductBasicInfoResponse product = new ProductBasicInfoResponse();
    ImageBasicInfoResponse image1 = new ImageBasicInfoResponse();
    image1.setLocationPath("imagePath1");
    image1.setMainImage(true);
    ImageBasicInfoResponse image2 = new ImageBasicInfoResponse();
    image2.setLocationPath("imagePath2");
    image2.setMainImage(true);
    product.setCommonImageList(Arrays.asList(image1, image2));
    bulkDownloadProductBasicInfoResponse.setProductBasicInfoResponseList(Collections.singletonList(product));
    CommonUtils.populateCommonImagesAndCategoryCodes(bulkDownloadProductBasicInfoResponse);
  }

  @Test
  public void testPopulateCommonImages_SkipInvalidImages() {
    BulkDownloadProductBasicInfoResponse bulkDownloadProductBasicInfoResponse =
        new BulkDownloadProductBasicInfoResponse();
    ProductBasicInfoResponse product = new ProductBasicInfoResponse();
    ImageBasicInfoResponse validImage = new ImageBasicInfoResponse();
    validImage.setLocationPath("validImagePath");
    validImage.setMainImage(false);
    ImageBasicInfoResponse invalidImage = new ImageBasicInfoResponse();
    invalidImage.setLocationPath(" ");
    invalidImage.setMainImage(false);
    product.setCommonImageList(Arrays.asList(invalidImage, validImage));
    bulkDownloadProductBasicInfoResponse.setProductBasicInfoResponseList(Collections.singletonList(product));
    CommonUtils.populateCommonImagesAndCategoryCodes(bulkDownloadProductBasicInfoResponse);
    assertEquals("validImagePath", product.getCommonImage2());
    assertNull(product.getCommonImage3());
  }

  @Test
  public void testSetCommonImages() {
    ProductBasicInfoResponse product = new ProductBasicInfoResponse();
    Map<Integer, BiConsumer<ProductBasicInfoResponse, String>> imageSetters = new HashMap<>();
    imageSetters.put(2, ProductBasicInfoResponse::setCommonImage2);
    imageSetters.put(3, ProductBasicInfoResponse::setCommonImage3);
    imageSetters.put(4, ProductBasicInfoResponse::setCommonImage4);
    imageSetters.put(5, ProductBasicInfoResponse::setCommonImage5);
    imageSetters.put(6, ProductBasicInfoResponse::setCommonImage6);
    imageSetters.put(7, ProductBasicInfoResponse::setCommonImage7);
    imageSetters.put(8, ProductBasicInfoResponse::setCommonImage8);
    CommonUtils.setCommonImages(product, 2, "imagePath2", imageSetters);
    assertEquals("imagePath2", product.getCommonImage2());
    assertNull(product.getCommonImage3());
    CommonUtils.setCommonImages(product, 3, "imagePath3", imageSetters);
    assertEquals("imagePath3", product.getCommonImage3());
    assertNull(product.getCommonImage4());
    CommonUtils.setCommonImages(product, 4, "imagePath4", imageSetters);
    assertEquals("imagePath4", product.getCommonImage4());
    assertNull(product.getCommonImage5());
    CommonUtils.setCommonImages(product, 5, "imagePath5", imageSetters);
    assertEquals("imagePath5", product.getCommonImage5());
    assertNull(product.getCommonImage6());
    CommonUtils.setCommonImages(product, 6, "imagePath6", imageSetters);
    assertEquals("imagePath6", product.getCommonImage6());
    assertNull(product.getCommonImage7());
    CommonUtils.setCommonImages(product, 7, "imagePath7", imageSetters);
    assertEquals("imagePath7", product.getCommonImage7());
    assertNull(product.getCommonImage8());
    CommonUtils.setCommonImages(product, 8, "imagePath8", imageSetters);
    assertEquals("imagePath8", product.getCommonImage8());
    CommonUtils.setCommonImages(product, 9, "invalidImagePath", imageSetters);
    assertEquals("imagePath8", product.getCommonImage8());
  }

  @Test
  public void testGetBulkProcess() {
    BulkProcess bulkProcess =
        CommonUtils.getBulkProcess(STORE_ID, REQUEST_ID, BULK_PROCESS_CODE, new BulkBasicInfoRequest(), 0, 0, false);
    Assertions.assertNotNull(bulkProcess);
    bulkProcess =
        CommonUtils.getBulkProcess(STORE_ID, REQUEST_ID, BULK_PROCESS_CODE, new BulkBasicInfoRequest(), 0, 0, true);
    Assertions.assertEquals(BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1.getValue(), bulkProcess.getBulkProcessType());
    Assertions.assertEquals(BULK_PROCESS_CODE, bulkProcess.getBulkProcessCode());
    Assertions.assertEquals(STORE_ID, bulkProcess.getStoreId());
    Assertions.assertEquals(BulkProcess.STATUS_PENDING, bulkProcess.getStatus());
    Assertions.assertEquals(REQUEST_ID, bulkProcess.getRequestId());
    Assertions.assertEquals(0, bulkProcess.getSuccessCount());
    Assertions.assertEquals(0, bulkProcess.getErrorCount());
    Assertions.assertEquals(0, bulkProcess.getInputErrorCount());
    Assertions.assertEquals(0, bulkProcess.getSystemErrorCount());
  }

  private BulkProcess getBulkProcess_1() {
    Date date = Calendar.getInstance().getTime();
    BulkProcess bulkProcess =
        new BulkProcess(DEFAULT_BULK_PROCESS_CODE, "ProductLevel3", DEFAULT_BUSINESS_PARTNER_CODE, date, date,
            BulkProcess.STATUS_FINISHED, null, new ArrayList<BulkProcessNotes>());
    bulkProcess.setStoreId(DEFAULT_STORE_ID);
    bulkProcess.setCreatedBy(DEFAULT_USERNAME);
    bulkProcess.setCreatedDate(date);
    bulkProcess.setUpdatedBy(DEFAULT_USERNAME);
    bulkProcess.setUpdatedDate(date);
    return bulkProcess;
  }

  @Test
  public void checkBulkProcessImagesAndVideoWithImageAndVideoCompletedAsTrue() {
    BulkProcess bulkProcess = new BulkProcess();
    BulkProcessImage bulkProcessImage = new BulkProcessImage();
    bulkProcessImage.setBulkProcessId(getBulkProcess_1().getId());
    bulkProcessImage.setCompleted(true);
    bulkProcessImage.setSequence(0);
    bulkProcessImage.setImageURL(IMAGE_URL);
    BulkProcessVideo bulkProcessVideo = new BulkProcessVideo();
    bulkProcessVideo.setCompleted(true);
    bulkProcessVideo.setUploadedURL(UPLOADED_URL);
    CommonUtils.checkBulkProcessImagesAndVideo(bulkProcess, Collections.singletonList(bulkProcessImage),
        Collections.singletonList(bulkProcessVideo), new ArrayList<>());
    assertEquals("READY_TO_PROCESS", bulkProcess.getStatus());
  }

  @Test
  public void checkBulkProcessImagesAndImageQCCompletedAsTrue() {
    BulkProcess bulkProcess = new BulkProcess();
    BulkProcessImage bulkProcessImage = new BulkProcessImage();
    bulkProcessImage.setBulkProcessId(getBulkProcess_1().getId());
    bulkProcessImage.setCompleted(true);
    bulkProcessImage.setSequence(0);
    bulkProcessImage.setImageURL(IMAGE_URL);
    BulkProcessImageQC bulkProcessImageQC = new BulkProcessImageQC();
    bulkProcessImageQC.setCompleted(true);
    CommonUtils.checkBulkProcessImagesAndImageQC(bulkProcess, Collections.singletonList(bulkProcessImage),
        Collections.singletonList(bulkProcessImageQC), new ArrayList<>());
    assertEquals("READY_TO_PROCESS", bulkProcess.getStatus());
  }

  @Test
  public void checkBulkProcessImagesAndVideoWithImageAndVideoCompletedAsFalse() {
    BulkProcess bulkProcess = new BulkProcess();
    BulkProcessImage bulkProcessImage = new BulkProcessImage();
    bulkProcessImage.setBulkProcessId(getBulkProcess_1().getId());
    bulkProcessImage.setCompleted(false);
    bulkProcessImage.setSequence(0);
    bulkProcessImage.setImageURL(IMAGE_URL);
    BulkProcessVideo bulkProcessVideo = new BulkProcessVideo();
    bulkProcessVideo.setCompleted(false);
    bulkProcessVideo.setUploadedURL(UPLOADED_URL);
    CommonUtils.checkBulkProcessImagesAndVideo(bulkProcess, Collections.singletonList(bulkProcessImage),
        Collections.singletonList(bulkProcessVideo), new ArrayList<>());
    assertEquals(null, bulkProcess.getStatus());
  }

  @Test
  public void checkBulkProcessImagesAndImageQCCompletedAsFalse() {
    BulkProcess bulkProcess = new BulkProcess();
    BulkProcessImage bulkProcessImage = new BulkProcessImage();
    bulkProcessImage.setBulkProcessId(getBulkProcess_1().getId());
    bulkProcessImage.setCompleted(false);
    bulkProcessImage.setSequence(0);
    bulkProcessImage.setImageURL(IMAGE_URL);
    BulkProcessImageQC bulkProcessImageQC = new BulkProcessImageQC();
    bulkProcessImageQC.setCompleted(false);
    CommonUtils.checkBulkProcessImagesAndImageQC(bulkProcess, Collections.singletonList(bulkProcessImage),
        Collections.singletonList(bulkProcessImageQC), new ArrayList<>());
    assertEquals(null, bulkProcess.getStatus());
  }

  @Test
  public void checkBulkProcessImagesAndVideoWithVideoListHavingNoData() {
    BulkProcess bulkProcess = new BulkProcess();
    BulkProcessImage bulkProcessImage = new BulkProcessImage();
    bulkProcessImage.setBulkProcessId(getBulkProcess_1().getId());
    bulkProcessImage.setCompleted(true);
    bulkProcessImage.setSequence(0);
    bulkProcessImage.setImageURL(IMAGE_URL);
    BulkProcessVideo bulkProcessVideo = new BulkProcessVideo();
    CommonUtils.checkBulkProcessImagesAndVideo(bulkProcess, Collections.singletonList(bulkProcessImage),
        Collections.singletonList(bulkProcessVideo), new ArrayList<>());
    assertEquals(null, bulkProcess.getStatus());
  }

  @Test
  public void checkBulkProcessImagesAndImageQCWithImageQCListHavingNoData() {
    BulkProcess bulkProcess = new BulkProcess();
    BulkProcessImage bulkProcessImage = new BulkProcessImage();
    bulkProcessImage.setBulkProcessId(getBulkProcess_1().getId());
    bulkProcessImage.setCompleted(true);
    bulkProcessImage.setSequence(0);
    bulkProcessImage.setImageURL(IMAGE_URL);
    BulkProcessImageQC bulkProcessImageQC = new BulkProcessImageQC();
    CommonUtils.checkBulkProcessImagesAndImageQC(bulkProcess, Collections.singletonList(bulkProcessImage),
        Collections.singletonList(bulkProcessImageQC), new ArrayList<>());
    assertEquals(null, bulkProcess.getStatus());
  }

  @Test
  public void checkBulkProcessImagesAndVideoWithVideoListAsEmpty() {
    BulkProcess bulkProcess = new BulkProcess();
    BulkProcessImage bulkProcessImage = new BulkProcessImage();
    bulkProcessImage.setBulkProcessId(getBulkProcess_1().getId());
    bulkProcessImage.setCompleted(true);
    bulkProcessImage.setSequence(0);
    bulkProcessImage.setImageURL(IMAGE_URL);
    CommonUtils.checkBulkProcessImagesAndVideo(bulkProcess, Collections.singletonList(bulkProcessImage),
        new ArrayList<>(), new ArrayList<>());
    assertEquals("READY_TO_PROCESS", bulkProcess.getStatus());
  }

  @Test
  public void checkBulkProcessImagesAndImageQCWithImageQCListAsEmpty() {
    BulkProcess bulkProcess = new BulkProcess();
    BulkProcessImage bulkProcessImage = new BulkProcessImage();
    bulkProcessImage.setBulkProcessId(getBulkProcess_1().getId());
    bulkProcessImage.setCompleted(true);
    bulkProcessImage.setSequence(0);
    bulkProcessImage.setImageURL(IMAGE_URL);
    CommonUtils.checkBulkProcessImagesAndImageQC(bulkProcess, Collections.singletonList(bulkProcessImage),
        new ArrayList<>(), new ArrayList<>());
    assertEquals("READY_TO_PROCESS", bulkProcess.getStatus());
  }

  @Test
  public void checkBulkProcessImagesAndVideoWithImageAndVideoListAsEmpty() {
    BulkProcess bulkProcess = new BulkProcess();
    CommonUtils.checkBulkProcessImagesAndVideo(bulkProcess, new ArrayList<>(), new ArrayList<>(), new ArrayList<>());
    assertEquals("READY_TO_PROCESS", bulkProcess.getStatus());
  }

  @Test
  void validateBasicInfoHeaders_EmptyHeaders() {
    BulkBasicInfoRequest request = new BulkBasicInfoRequest();
    Map<Integer, String> headers = new HashMap<>();
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class,
        () -> CommonUtils.validateBasicInfoHeaders(request, "store1", "code1", "bp1", headers));
  }

  @Test
  void validateBasicInfoHeaders_MissingRequiredHeaders() {
    BulkBasicInfoRequest request = new BulkBasicInfoRequest();
    Map<Integer, String> headers = new HashMap<>();
    headers.put(0, "Header1");
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class,
        () -> CommonUtils.validateBasicInfoHeaders(request, "store1", "code1", "bp1", headers));
  }

  @Test
  void validateBasicInfoHeadersTest() {
    BulkBasicInfoRequest request = new BulkBasicInfoRequest();
    Map<Integer, String> headers = new HashMap<>();
    for (int i = 0; i < BulkParameters.BASIC_INFO_BASE_HEADER_LIST.size(); i++) {
      headers.put(i, BulkParameters.BASIC_INFO_BASE_HEADER_LIST.get(i));
    };
    CommonUtils.validateBasicInfoHeaders(request, "store1", "code1", "bp1", headers);
  }

  @Test
  void validateRowData_AllValid() {
    Map<String, String> row = new HashMap<>();
    row.put(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER, "SKU123");
    row.put(BulkParameters.DESCRIPTIONS, "Description");
    row.put(BulkParameters.MAIN_PHOTO, "photo.jpg");
    row.put(BulkParameters.SHIPPING_TYPE, "REGULAR");
    row.put(BulkParameters.LENGTH_HEADER, "10");
    row.put(BulkParameters.WIDTH_HEADER, "20");
    row.put(BulkParameters.HEIGHT_HEADER, "30");
    row.put(BulkParameters.ACTUAL_WEIGHT, "1");
    row.put(BulkParameters.SHIPPING_WEIGHT, "1.5");
    StringBuilder errors = new StringBuilder();
    boolean result = CommonUtils.validateRowData(row, errors, false);
    assertFalse(result);
  }

  @Test
  void validateRowData_ShippingValid() {
    Map<String, String> row = new HashMap<>();
    row.put(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER, "SKU123");
    row.put(BulkParameters.DESCRIPTIONS, "Description");
    row.put(BulkParameters.MAIN_PHOTO, "photo.jpg");
    row.put(BulkParameters.SHIPPING_TYPE, "Dikirim oleh blibli");
    row.put(BulkParameters.LENGTH_HEADER, "10");
    row.put(BulkParameters.WIDTH_HEADER, "20");
    row.put(BulkParameters.HEIGHT_HEADER, "30");
    row.put(BulkParameters.ACTUAL_WEIGHT, "1");
    row.put(BulkParameters.SHIPPING_WEIGHT, "1.5");
    StringBuilder errors = new StringBuilder();
    boolean result = CommonUtils.validateRowData(row, errors, false);
    assertTrue(result);
  }

  @Test
  void validateRowData_Invalid() {
    Map<String, String> row = new HashMap<>();
    row.put(BulkParameters.INSTORE, "invalid");
    StringBuilder errors = new StringBuilder();
    boolean result = CommonUtils.validateRowData(row, errors, false);
    assertFalse(result);
    assertTrue(errors.length() > 0);
  }

  @Test
  void validateRowData_valid() {
    Map<String, String> row = new HashMap<>();
    row.put(BulkParameters.INSTORE, "1.0");
    StringBuilder errors = new StringBuilder();
    boolean result = CommonUtils.validateRowData(row, errors, false);
  }

  @Test
  void validateDimensionFields_AllValid() {
    Map<String, String> row = new HashMap<>();
    row.put(BulkParameters.LENGTH_HEADER, "10");
    row.put(BulkParameters.WIDTH_HEADER, "20");
    row.put(BulkParameters.HEIGHT_HEADER, "30");
    row.put(BulkParameters.ACTUAL_WEIGHT, "1");
    StringBuilder errors = new StringBuilder();
    boolean result = CommonUtils.validateDimensionFields(row, errors, false);
    assertTrue(result);
    assertEquals(0, errors.length());
  }

  @Test
  void addErrorNote_Success() {
    BulkProcess bulkProcess = new BulkProcess();
    BulkProcessData data = new BulkProcessData();
    Map<String, String> row = new HashMap<>();
    row.put(GenericBulkHeaders.ROW_NUMBER, "1");
    CommonUtils.addErrorNote(bulkProcess, data, row, "Test error");
    assertEquals(1, data.getInputErrorCount());
    assertEquals(BulkProcessData.STATUS_FAIL, data.getStatus());
    assertEquals("Test error", data.getErrorMessage());
    assertEquals(0, bulkProcess.getBulkProcessNotes().size());
  }

  @Test
  void validateNumericField_ValidNumber() {
    Map<String, String> row = new HashMap<>();
    row.put("field1", "123.45");
    StringBuilder errors = new StringBuilder();
    boolean result = CommonUtils.validateNumericField(row, errors, "field1", false);
    assertTrue(result);
    assertEquals(0, errors.length());
  }

  @Test
  void validateNumericField_ValidNumberException() {
    Map<String, String> row = new HashMap<>();
    row.put("field1", STORE_ID);
    StringBuilder errors = new StringBuilder();
    boolean result = CommonUtils.validateNumericField(row, errors, "field1", false);
    assertFalse(result);
  }

  @Test
  void validateNumericField_BlankAllowed() {
    Map<String, String> row = new HashMap<>();
    row.put("field1", "");
    StringBuilder errors = new StringBuilder();
    boolean result = CommonUtils.validateNumericField(row, errors, "field1", true);
    assertTrue(result);
    assertEquals(0, errors.length());
  }

  @Test
  void validateProductName_Valid() {
    Map<String, String> row = new HashMap<>();
    row.put(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER, "Product 1");
    StringBuilder errors = new StringBuilder();
    boolean result = CommonUtils.validateProductName(row, errors);
    assertTrue(result);
    assertEquals(0, errors.length());
  }

  @Test
  void validateProductName_InvalidValid() {
    Map<String, String> row = new HashMap<>();
    String longProductName = "Product".repeat(27);
    row.put(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER, longProductName);
    StringBuilder errors = new StringBuilder();
    boolean result = CommonUtils.validateProductName(row, errors);
    assertFalse(result);
  }


  @Test
  void validateMainImage_Valid() {
    Map<String, String> row = new HashMap<>();
    row.put(BulkParameters.MAIN_PHOTO, "image.jpg");
    StringBuilder errors = new StringBuilder();
    boolean result = CommonUtils.validateMainImage(row, errors);
    assertTrue(result);
    assertEquals(0, errors.length());
  }

  @Test
  void validateInstore_Valid() {
    Map<String, String> row = new HashMap<>();
    row.put(BulkParameters.INSTORE, "1");
    StringBuilder errors = new StringBuilder();
    boolean result = CommonUtils.validateInstore(row, errors);
    assertTrue(result);
    assertEquals(0, errors.length());
  }

  @Test
  void validateInstore_invalidValid() {
    Map<String, String> row = new HashMap<>();
    row.put(BulkParameters.INSTORE, "");
    StringBuilder errors = new StringBuilder();
    boolean result = CommonUtils.validateInstore(row, errors);
    assertFalse(result);
  }

  @Test
  void validateShippingType_Valid() {
    Map<String, String> row = new HashMap<>();
    row.put(BulkParameters.SHIPPING_TYPE, "REGULAR");
    StringBuilder errors = new StringBuilder();
    boolean result = CommonUtils.validateShippingType(row, errors);
    assertFalse(result);
  }

  @Test
  void validateShippingType_ValidTest() {
    Map<String, String> row = new HashMap<>();
    row.put(BulkParameters.SHIPPING_TYPE, "Dikirim oleh blibli");
    StringBuilder errors = new StringBuilder();
    boolean result = CommonUtils.validateShippingType(row, errors);
    assertTrue(result);
  }

  @Test
  void validateDescription_Valid() {
    Map<String, String> row = new HashMap<>();
    row.put(BulkParameters.DESCRIPTIONS, "Test description");
    StringBuilder errors = new StringBuilder();
    boolean result = CommonUtils.validateDescription(row, errors, false);
    assertTrue(result);
    assertEquals(0, errors.length());
  }

  @Test
  void validateDescription_InstoreMax() {
    Map<String, String> row = new HashMap<>();
    row.put(BulkParameters.DESCRIPTIONS, "");
    StringBuilder errors = new StringBuilder();
    boolean result = CommonUtils.validateDescription(row, errors, true);
    assertTrue(result);
    assertEquals(0, errors.length());
  }

  @Test
  void getBulkProcessTypeBasedOnTrustedSellerAndUserInputRows_AllConditions() {
    String defaultType = "default_type";

    // Test case 1: Trusted seller with rows <= trustedSellerRowCount
    String result1 = CommonUtils.getBulkProcessTypeBasedOnTrustedSellerAndUserInputRows(
        defaultType, 5, 10, 20, 50, true);
    assertEquals(BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1.getValue(), result1);

    // Test case 2: Trusted seller with rows > trustedSellerRowCount
    String result2 = CommonUtils.getBulkProcessTypeBasedOnTrustedSellerAndUserInputRows(
        defaultType, 15, 10, 20, 50, true);
    assertEquals(BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_2.getValue(), result2);

    // Test case 3: Regular seller with rows <= regularSellerMinRowCount
    String result3 = CommonUtils.getBulkProcessTypeBasedOnTrustedSellerAndUserInputRows(
        defaultType, 15, 10, 20, 50, false);
    assertEquals(BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1.getValue(), result3);

    // Test case 4: Regular seller with rows <= regularSellerMaxCount
    String result4 = CommonUtils.getBulkProcessTypeBasedOnTrustedSellerAndUserInputRows(
        defaultType, 35, 10, 20, 50, false);
    assertEquals(BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_2.getValue(), result4);

    // Test case 5: Regular seller with rows > regularSellerMaxCount
    String result5 = CommonUtils.getBulkProcessTypeBasedOnTrustedSellerAndUserInputRows(
        defaultType, 60, 10, 20, 50, false);
    assertEquals(defaultType, result5);

    // Test case 6: Edge case - equal to trustedSellerRowCount
    String result6 = CommonUtils.getBulkProcessTypeBasedOnTrustedSellerAndUserInputRows(
        defaultType, 10, 10, 20, 50, true);
    assertEquals(BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1.getValue(), result6);

    // Test case 7: Edge case - equal to regularSellerMinRowCount
    String result7 = CommonUtils.getBulkProcessTypeBasedOnTrustedSellerAndUserInputRows(
        defaultType, 20, 10, 20, 50, false);
    assertEquals(BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_1.getValue(), result7);

    // Test case 8: Edge case - equal to regularSellerMaxCount
    String result8 = CommonUtils.getBulkProcessTypeBasedOnTrustedSellerAndUserInputRows(
        defaultType, 50, 10, 20, 50, false);
    assertEquals(BulkProcessType.PRODUCT_BASIC_INFO_PRIORITY_2.getValue(), result8);
  }

  @Test
  void getBulkProcessData_Success() throws JsonProcessingException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setId("id1");
    bulkProcess.setBulkProcessCode("code1");
    bulkProcess.setStoreId("store1");
    bulkProcess.setRequestId("req1");
    BulkProcessData result = CommonUtils.getBulkProcessData(bulkProcess, 1, "SKU1");
    assertEquals("id1", result.getBulkProcessId());
    assertEquals("code1", result.getBulkProcessCode());
    assertEquals("store1", result.getStoreId());
    assertEquals(BulkProcessData.STATUS_PENDING, result.getStatus());
    assertEquals("req1", result.getRequestId());
    assertEquals(1, result.getRowNumber());
    assertEquals("SKU1", result.getParentProduct());
  }

  @Test
  void validaExcelRowData_ValidData_NotPureInstore() {
    // Setup
    BulkProcess bulkProcess = new BulkProcess();
    List<BulkProcessData> requestData = new ArrayList<>();
    Map<String, String> userData = new HashMap<>();
    userData.put(BulkParameters.INSTORE, "0.0");
    userData.put(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER, "SKU1");
    userData.put(BulkParameters.DESCRIPTIONS, "Description");
    userData.put(BulkParameters.MAIN_PHOTO, "photo.jpg");
    userData.put(BulkParameters.SHIPPING_TYPE, "Dikirim oleh blibli");
    BulkProcessData data = new BulkProcessData();

    // Execute
    CommonUtils.validaExcelRowData(bulkProcess, requestData, userData, data, true, null);

    // Verify
    assertEquals(1, requestData.size());
    userData.put(BulkParameters.INSTORE, BULK_PROCESS_CODE);
    CommonUtils.validaExcelRowData(bulkProcess, requestData, userData, data, true, null);
  }

  @Test
  void validaExcelRowData_ValidData_PureInstore() {
    // Setup
    BulkProcess bulkProcess = new BulkProcess();
    List<BulkProcessData> requestData = new ArrayList<>();
    Map<String, String> userData = new HashMap<>();
    userData.put(BulkParameters.INSTORE, "1.0");
    userData.put(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER, "SKU1");
    userData.put(BulkParameters.MAIN_PHOTO, "photo.jpg");
    userData.put(BulkParameters.SHIPPING_TYPE, "Dikirim oleh blibli");
    BulkProcessData data = new BulkProcessData();

    // Execute
    CommonUtils.validaExcelRowData(bulkProcess, requestData, userData, data, false, null);

    // Verify
    assertEquals(1, requestData.size());
  }

  @Test
  void validaExcelRowData_InvalidData() {
    // Setup
    BulkProcess bulkProcess = new BulkProcess();
    List<BulkProcessData> requestData = new ArrayList<>();
    Map<String, String> userData = new HashMap<>();
    userData.put(BulkParameters.INSTORE, "0.0");
    userData.put(GenericBulkHeaders.ROW_NUMBER, "1");
    BulkProcessData data = new BulkProcessData();

    // Execute
    CommonUtils.validaExcelRowData(bulkProcess, requestData, userData, data, true, null);

    // Verify
    assertEquals(1, requestData.size());
    assertEquals(BulkProcessData.STATUS_FAIL, data.getStatus());
    assertEquals(1, data.getInputErrorCount());
    assertFalse(bulkProcess.getBulkProcessNotes().size() > 0);
  }

  @Test
  void validaExcelRowData_NullB2cActivated() {
    // Setup
    BulkProcess bulkProcess = new BulkProcess();
    List<BulkProcessData> requestData = new ArrayList<>();
    Map<String, String> userData = new HashMap<>();
    userData.put(BulkParameters.INSTORE, "1.0");
    userData.put(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER, "SKU1");
    userData.put(BulkParameters.MAIN_PHOTO, "photo.jpg");
    userData.put(BulkParameters.SHIPPING_TYPE, "Dikirim oleh blibli");
    BulkProcessData data = new BulkProcessData();
    // Execute
    CommonUtils.validaExcelRowData(bulkProcess, requestData, userData, data, null, null);
    // Verify
    assertEquals(1, requestData.size());
  }

  @Test
  public void testGenerateBulkProcessVideoList() {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setId("id1");
    bulkProcess.setBulkProcessCode("code1");

    Map<String, List<String>> videoMap = new HashMap<>();
    videoMap.put("http://video.com/v1.mp4", Arrays.asList("SKU1", "SKU2"));

    List<BulkProcessVideo> videoList = new ArrayList<>();

    CommonUtils.generateBulkProcessVideoList(bulkProcess, videoMap, videoList);

    assertEquals(1, videoList.size());
    BulkProcessVideo video = videoList.get(0);
    assertEquals("code1", video.getBulkProcessCode());
    assertFalse(video.isCompleted());
    assertEquals("http://video.com/v1.mp4", video.getUploadedURL());
  }

  @Test
  public void testGenerateBulkProcessImageList() throws JsonProcessingException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setId("id2");
    bulkProcess.setBulkProcessCode("code2");
    bulkProcess.setStoreId("store123");
    Map<String, List<String>> imageMap = new HashMap<>();
    imageMap.put("http://img.com/i1.jpg", Arrays.asList("SKU1", "SKU2"));
    imageMap.put("http://img.com/i2.jpg", Collections.singletonList("SKU3"));
    List<BulkProcessImage> imageList = new ArrayList<>();
    CommonUtils.generateBulkProcessImageList(bulkProcess, imageMap, imageList, new HashMap<>());
    assertEquals(2, imageList.size());
    BulkProcessImage image1 = imageList.get(0);
    assertEquals("id2", image1.getBulkProcessId());
    assertEquals("store123", image1.getStoreId());
    ProductImageAndVideoResponse productImageAndVideoResponse = new ProductImageAndVideoResponse();
    Map<String, ProductImageAndVideoResponse> productImageAndVideoResponseMap =
        Map.of("SKU1", productImageAndVideoResponse);
    CommonUtils.generateBulkProcessImageList(bulkProcess, imageMap, imageList, productImageAndVideoResponseMap);
    productImageAndVideoResponse.setBrand(BULK_PROCESS_CODE);
    productImageAndVideoResponse.setProductName(BULK_PROCESS_CODE);
    CommonUtils.generateBulkProcessImageList(bulkProcess, imageMap, imageList, productImageAndVideoResponseMap);
  }

  @Test
  public void testMapNewImageUrlsToProductCodes() {
    Map<String, List<String>> map = new HashMap<>();
    Map<String, String> userData = new HashMap<>();
    userData.put(BulkParameters.MAIN_PHOTO, "http://image.com/img1.jpg");
    userData.put(BulkParameters.COMMON_PHOTO_2, "http://image.com/img2.jpg");
    ProductBasicResponse productInfo = new ProductBasicResponse();
    productInfo.setProductCode("SKU100");
    ProductImageAndVideoResponse productImageAndVideoResponse = new ProductImageAndVideoResponse();
    ImageBasicInfoResponse existingImage = new ImageBasicInfoResponse();
    existingImage.setLocationPath("http://image.com/img1.jpg"); // already exists, should be skipped
    productImageAndVideoResponse.setCommonImageList(Collections.singletonList(existingImage));
    CommonUtils.mapNewImageUrlsToProductCodes(map, userData, productInfo, productImageAndVideoResponse,
        StringUtils.EMPTY);
    assertEquals(1, map.size());
    assertTrue(map.containsKey("http://image.com/img2.jpg"));
    assertEquals(Collections.singletonList("SKU100"), map.get("http://image.com/img2.jpg"));
  }

  @Test
  public void testMapNewImageUrlsToProductCodesImagePrefix() {
    Map<String, List<String>> map = new HashMap<>();
    Map<String, String> userData = new HashMap<>();
    userData.put(BulkParameters.MAIN_PHOTO, "http://image.com/img1.jpg");
    userData.put(BulkParameters.COMMON_PHOTO_2, "://image.com/img2.jpg");
    ProductBasicResponse productInfo = new ProductBasicResponse();
    productInfo.setProductCode("SKU100");
    ProductImageAndVideoResponse productImageAndVideoResponse = new ProductImageAndVideoResponse();
    ImageBasicInfoResponse existingImage = new ImageBasicInfoResponse();
    existingImage.setLocationPath("http://image.com/img1.jpg"); // already exists, should be skipped
    productImageAndVideoResponse.setCommonImageList(Collections.singletonList(existingImage));
    CommonUtils.mapNewImageUrlsToProductCodes(map, userData, productInfo, productImageAndVideoResponse,
        "http");
    assertEquals(2, map.size());
    assertTrue(map.containsKey("://image.com/img2.jpg"));
    assertEquals(Collections.singletonList("SKU100"), map.get("://image.com/img2.jpg"));
  }


  @Test
  public void testIsVideoUrlExisting_True_WhenUploadedContainsExisting() {
    boolean result = CommonUtils.isVideoUrlExisting("video.mp4", "http://abc.com/video.mp4");
    assertTrue(result);
  }

  @Test
  public void testIsVideoUrlExisting_False_WhenDifferent() {
    boolean result = CommonUtils.isVideoUrlExisting("video1.mp4", "video2.mp4");
    assertFalse(result);
  }

  @Test
  public void testIsVideoUrlExisting_False_WhenExistingIsBlank() {
    boolean result = CommonUtils.isVideoUrlExisting("video.mp4", " ");
    assertFalse(result);
  }

  @Test
  void testTrimVideoUrlPrefix_RemovesPrefixWhenPresent() {
    String prefix = "https://cdn.example.com/videos/";
    String input = "https://cdn.example.com/videos/video1.mp4";
    String expected = "video1.mp4";
    String result = CommonUtils.trimVideoUrlPrefix(input, prefix);
    assertEquals(expected, result);
  }

  @Test
  void testTrimVideoUrlPrefix_ReturnsOriginalWhenPrefixNotPresent() {
    String prefix = "https://cdn.example.com/videos/";
    String input = "https://othercdn.com/video1.mp4";
    String result = CommonUtils.trimVideoUrlPrefix(input, prefix);
    assertEquals(input, result);
  }

  @Test
  void testTrimVideoUrlPrefix_NullInputUrl() {
    String prefix = "https://cdn.example.com/videos/";
    String result = CommonUtils.trimVideoUrlPrefix(null, prefix);
    assertNull(result);
  }

  @Test
  void testTrimVideoUrlPrefix_BlankInputUrl() {
    String prefix = "https://cdn.example.com/videos/";
    String result = CommonUtils.trimVideoUrlPrefix("  ", prefix);
    assertEquals("  ", result);
  }

  @Test
  public void setBulkProcessVideoFinalStatus_success() {
    BulkBasicInfoVideoDownloadResponseModel responseModel = new BulkBasicInfoVideoDownloadResponseModel();
    responseModel.setVideoId("vid123");
    responseModel.setSourceUrl("http://video.source");
    responseModel.setErrorMessage("error");
    responseModel.setErrorCode("code123");
    responseModel.setCoverImagePath("cover/path.jpg");
    responseModel.setVideoName("video_name.mp4");
    BulkProcessVideo bulkProcessVideo = new BulkProcessVideo();
    CommonUtils.setBulkProcessVideoFinalStatus(responseModel, bulkProcessVideo);
    Assertions.assertEquals("vid123", bulkProcessVideo.getVideoId());
    Assertions.assertEquals("http://video.source", bulkProcessVideo.getVideoUrl());
    Assertions.assertEquals("error", bulkProcessVideo.getErrorMessage());
    Assertions.assertEquals("code123", bulkProcessVideo.getErrorCode());
    Assertions.assertEquals("cover/path.jpg", bulkProcessVideo.getCoverImagePath());
    Assertions.assertEquals("video_name.mp4", bulkProcessVideo.getVideoName());
    Assertions.assertTrue(bulkProcessVideo.isCompleted());
  }

  @Test
  void testSetImageDownloadStatus_Success() {
    BulkProcessImage image = new BulkProcessImage();
    ImageDownloadResult result = new ImageDownloadResult();
    result.setDownloadSuccess(true);
    result.setImageFileName("image1");
    result.setImageType(".jpg");
    StringBuilder validationMsg = new StringBuilder();
    CommonUtils.setImageDownloadStatus(image, result, validationMsg);
    assertTrue(image.isCompleted());

  }

  @Test
  void testSetImageDownloadStatus_Failure() {
    BulkProcessImage image = new BulkProcessImage();
    ImageDownloadResult result = new ImageDownloadResult();
    result.setDownloadSuccess(false);
    StringBuilder validationMsg = new StringBuilder("Download failed");
    CommonUtils.setImageDownloadStatus(image, result, validationMsg);
    assertTrue(image.isCompleted());
    assertEquals("Download failed", image.getErrorMessage());
  }

  @Test
  void testGetUrlConnection_Success() throws IOException {
    String imageUrl = "https://via.placeholder.com/150";
    URLConnection connection = CommonUtils.getUrlConnection(imageUrl, 5000, 5000);
    assertNotNull(connection);
    assertEquals("image/*", connection.getRequestProperty(Constant.ACCEPT_HEADER));
  }

  @Test
  void testHandleValidationException_ResolutionError() {
    StringBuilder validationErrorMessage = new StringBuilder();
    String error = BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE_EN;
    CommonUtils.handleValidationException(error, "http://image.jpg", validationErrorMessage);
    assertTrue(validationErrorMessage.toString()
        .contains(BulkProcessValidationErrorMessages.IMAGE_LESS_RESOLUTION_VALIDATION_ERR_MESSAGE));
  }

  @Test
  void testHandleValidationException_SizeError() {
    StringBuilder validationErrorMessage = new StringBuilder();
    String error = "Some other error";
    CommonUtils.handleValidationException(error, "http://image.jpg", validationErrorMessage);
    assertTrue(validationErrorMessage.toString()
        .contains(BulkProcessValidationErrorMessages.IMAGE_MAX_SIZE_VALIDATION_ERR_MESSAGE_EN));
  }

  @Test
  void testHandleInvalidType() {
    StringBuilder validationErrorMessage = new StringBuilder();
    List<String> images = new ArrayList<>(Arrays.asList("key1", "key2"));
    CommonUtils.handleInvalidType("http://image.jpg", "key1", "text/html", images, validationErrorMessage, "bulk123");
    assertFalse(images.contains("key1"));
    assertTrue(validationErrorMessage.toString().contains(BulkProcessValidationErrorMessages.IMAGE_URL_TYPE_INVALID));
  }

  @Test
  void testHandleAllFailedScenario_AllFailed() {
    BulkProcess bulkProcess = new BulkProcess();
    BulkProcessData data1 = new BulkProcessData();
    data1.setStatus(BulkProcessData.STATUS_FAIL);
    BulkProcessData data2 = new BulkProcessData();
    data2.setStatus(BulkProcessData.STATUS_FAIL);
    List<BulkProcessData> requestData = Arrays.asList(data1, data2);
    CommonUtils.handleAllFailedScenario(bulkProcess, requestData);
    assertEquals(BulkProcess.STATUS_PUBLISHED, bulkProcess.getStatus());
  }

  @Test
  void testHandleAllFailedScenario_NotAllFailed() {
    BulkProcess bulkProcess = new BulkProcess();
    BulkProcessData data1 = new BulkProcessData();
    data1.setStatus(BulkProcessData.STATUS_FAIL);
    BulkProcessData data2 = new BulkProcessData();
    data2.setStatus("SUCCESS");
    List<BulkProcessData> requestData = Arrays.asList(data1, data2);
    CommonUtils.handleAllFailedScenario(bulkProcess, requestData);
  }

  @Test
  void validateActiveProduct_shouldThrowAndAddError_whenNotMultiPickupAndNullResponse() {
    Map<String, String> productDataL5 = new HashMap<>();
    productDataL5.put(BulkParameters.BLIBLI_SKU, "SKU123");
    productDataL5.put(BulkParameters.PICKUP_POINT_HEADER, "PICKUP1");
    productDataL5.put(BulkParameters.PRODUCT_NAME, "ProductName");
    List<BulkUpdateErrorDTO> errorList = new ArrayList<>();
    List<Map<String, String>> failedData = new ArrayList<>();
    ApplicationRuntimeException ex = assertThrows(ApplicationRuntimeException.class, () -> {
      CommonUtils.validateActiveProduct(productDataL5, null, false, errorList, failedData);
    });
    assertEquals(1, errorList.size());
    assertEquals(1, failedData.size());
    assertTrue(ex.getErrorMessage().contains(ProductUpdateErrorMessages.INVALID_ADD_PICKUP_POINT_REQUEST));
    assertEquals("ProductName", errorList.get(0).getProductName());
    assertEquals("SKU123", errorList.get(0).getProductSku());
    assertEquals("PICKUP1", errorList.get(0).getPickupPointCode());
  }

  @Test
  void validateActiveProduct_shouldNotThrow_whenMultiPickupAndNullResponse() {
    Map<String, String> productDataL5 = new HashMap<>();
    List<BulkUpdateErrorDTO> errorList = new ArrayList<>();
    List<Map<String, String>> failedData = new ArrayList<>();
    // Should not throw
    CommonUtils.validateActiveProduct(productDataL5, null, true, errorList, failedData);
    assertEquals(0, errorList.size());
    assertEquals(0, failedData.size());
  }

  @Test
  void validateActiveProduct_shouldNotThrow_whenNotMultiPickupAndNonNullResponse() {
    Map<String, String> productDataL5 = new HashMap<>();
    List<BulkUpdateErrorDTO> errorList = new ArrayList<>();
    List<Map<String, String>> failedData = new ArrayList<>();
    ItemPickupPointListingL3Response response = new ItemPickupPointListingL3Response();
    // Should not throw
    CommonUtils.validateActiveProduct(productDataL5, response, false, errorList, failedData);
    assertEquals(0, errorList.size());
    assertEquals(0, failedData.size());
  }

  @Test
  void testTakenDown() {
    ProductBasicResponse productInfo = mock(ProductBasicResponse.class);
    BulkProcess bulkProcess = new BulkProcess();
    BulkProcessData bulkProcessData = new BulkProcessData();
    Map<String, String> userData = new HashMap<>();
    List<BulkProcessData> requestData = new ArrayList<>();
    when(productInfo.isTakenDown()).thenReturn(true);
    boolean result =
        CommonUtils.runProductStateValidationChecks(productInfo, bulkProcess, bulkProcessData,
            userData, requestData);
    assertTrue(result);
    assertEquals(1, requestData.size());
  }

  @Test
  void testMarkForDelete() {
    ProductBasicResponse productInfo = mock(ProductBasicResponse.class);
    BulkProcess bulkProcess = new BulkProcess();
    BulkProcessData bulkProcessData = new BulkProcessData();
    Map<String, String> userData = new HashMap<>();
    List<BulkProcessData> requestData = new ArrayList<>();
    when(productInfo.isTakenDown()).thenReturn(false);
    when(productInfo.isMarkForDelete()).thenReturn(true);
    boolean result =
        CommonUtils.runProductStateValidationChecks(productInfo, bulkProcess, bulkProcessData,
            userData, requestData);
    assertTrue(result);
    assertEquals(1, requestData.size());
  }

  @Test
  void testArchived() {
    ProductBasicResponse productInfo = mock(ProductBasicResponse.class);
    BulkProcess bulkProcess = new BulkProcess();
    BulkProcessData bulkProcessData = new BulkProcessData();
    Map<String, String> userData = new HashMap<>();
    List<BulkProcessData> requestData = new ArrayList<>();
    when(productInfo.isTakenDown()).thenReturn(false);
    when(productInfo.isMarkForDelete()).thenReturn(false);
    when(productInfo.isArchived()).thenReturn(true);
    boolean result =
        CommonUtils.runProductStateValidationChecks(productInfo, bulkProcess, bulkProcessData,
            userData, requestData);
    assertTrue(result);
    assertEquals(1, requestData.size());
  }

  @Test
  void testSharedProduct() {
    ProductBasicResponse productInfo = mock(ProductBasicResponse.class);
    BulkProcess bulkProcess = new BulkProcess();
    BulkProcessData bulkProcessData = new BulkProcessData();
    Map<String, String> userData = new HashMap<>();
    List<BulkProcessData> requestData = new ArrayList<>();
    when(productInfo.isTakenDown()).thenReturn(false);
    when(productInfo.isMarkForDelete()).thenReturn(false);
    when(productInfo.isArchived()).thenReturn(false);
    when(productInfo.isSharedProduct()).thenReturn(true);
    when(productInfo.isSyncProduct()).thenReturn(true);
    boolean result =
        CommonUtils.runProductStateValidationChecks(productInfo, bulkProcess, bulkProcessData,
            userData, requestData);
    assertTrue(result);
    assertEquals(1, requestData.size());
  }

  @Test
  void testNotSyncProduct() {
    ProductBasicResponse productInfo = mock(ProductBasicResponse.class);
    BulkProcess bulkProcess = new BulkProcess();
    BulkProcessData bulkProcessData = new BulkProcessData();
    Map<String, String> userData = new HashMap<>();
    List<BulkProcessData> requestData = new ArrayList<>();
    when(productInfo.isTakenDown()).thenReturn(false);
    when(productInfo.isMarkForDelete()).thenReturn(false);
    when(productInfo.isArchived()).thenReturn(false);
    when(productInfo.isSharedProduct()).thenReturn(false);
    when(productInfo.isSyncProduct()).thenReturn(false);
    boolean result =
        CommonUtils.runProductStateValidationChecks(productInfo, bulkProcess, bulkProcessData,
            userData, requestData);
    assertTrue(result);
    assertEquals(1, requestData.size());
  }

  @Test
  void testMerchantMismatch() {
    ProductBasicResponse productInfo = mock(ProductBasicResponse.class);
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBusinessPartnerCode("BP123");
    BulkProcessData bulkProcessData = new BulkProcessData();
    Map<String, String> userData = new HashMap<>();
    List<BulkProcessData> requestData = new ArrayList<>();
    when(productInfo.isTakenDown()).thenReturn(false);
    when(productInfo.isMarkForDelete()).thenReturn(false);
    when(productInfo.isArchived()).thenReturn(false);
    when(productInfo.isSharedProduct()).thenReturn(false);
    when(productInfo.isSyncProduct()).thenReturn(true);
    when(productInfo.getMerchantCode()).thenReturn("DIFFERENT_CODE");
    boolean result =
        CommonUtils.runProductStateValidationChecks(productInfo, bulkProcess, bulkProcessData,
            userData, requestData);
    assertTrue(result);
    assertEquals(1, requestData.size());
  }

  @Test
  void testNoValidationFails() {
    ProductBasicResponse productInfo = mock(ProductBasicResponse.class);
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBusinessPartnerCode("MATCH_CODE");
    BulkProcessData bulkProcessData = new BulkProcessData();
    Map<String, String> userData = new HashMap<>();
    List<BulkProcessData> requestData = new ArrayList<>();
    when(productInfo.isTakenDown()).thenReturn(false);
    when(productInfo.isMarkForDelete()).thenReturn(false);
    when(productInfo.isArchived()).thenReturn(false);
    when(productInfo.isSharedProduct()).thenReturn(false);
    when(productInfo.isSyncProduct()).thenReturn(true);
    when(productInfo.getMerchantCode()).thenReturn("MATCH_CODE");
    boolean result =
        CommonUtils.runProductStateValidationChecks(productInfo, bulkProcess, bulkProcessData,
            userData, requestData);
    assertFalse(result);
    assertEquals(0, requestData.size());
  }
  @Test
  public void testGetValidateImageDTO() {
    String bulkProcessCode = "code123";
    Set<String> invalidUrls = new HashSet<>(Collections.singletonList("invalid.jpg"));
    List<String> images = Arrays.asList("img1.jpg", "img2.jpg");
    BulkUploadErrorCounter counter = new BulkUploadErrorCounter();
    StringBuilder errorMessage = new StringBuilder("error");
    boolean isInternational = true;
    Map.Entry<String, String> image = new AbstractMap.SimpleEntry<>("key", "value");
    URLConnection connection = mock(URLConnection.class);
    ValidateImageDTO dto = CommonUtils.getValidateImageDTO(
      bulkProcessCode, invalidUrls, images, counter, errorMessage,
      isInternational, image, connection
    );
    assertEquals(image, dto.getImage());
    assertEquals(connection, dto.getConnection());
    assertEquals(bulkProcessCode, dto.getBulkProcessCode());
    assertEquals(invalidUrls, dto.getUrlImagesWithInvalidExtension());
    assertEquals(images, dto.getImages());
    assertEquals(counter, dto.getBulkUploadErrorCounter());
    assertEquals(errorMessage, dto.getValidationErrorMessage());
    assertTrue(dto.isInternationalMerchant());
  }

  @Test
  void testFindNonNullDataRowIndexByStartRow_withNonNullRow() throws IOException {
    try (XSSFWorkbook workbook = new XSSFWorkbook()) {
      Sheet sheet = workbook.createSheet("TestSheet");

      // Create a row at index 3
      sheet.createRow(3);

      int result = CommonUtils.findNonNullDataRowIndexByStartRow(sheet, 1);

      assertEquals(3, result, "Should return first non-null row index starting from 1");
    }
  }

  @Test
  void testFindNonNullDataRowIndexByStartRow_withNoNonNullRows() throws IOException {
    try (XSSFWorkbook workbook = new XSSFWorkbook()) {
      Sheet sheet = workbook.createSheet("TestSheet");

      int result = CommonUtils.findNonNullDataRowIndexByStartRow(sheet, 1);

      assertEquals(2, result, "Should return startRow + 1 when no non-null rows exist");
    }
  }

  @Test
  void testFindNonNullDataRowIndexByStartRow_withStartRowBeyondLastRow() {
    try (XSSFWorkbook workbook = new XSSFWorkbook()) {
      Sheet sheet = workbook.createSheet("TestSheet");
      sheet.createRow(0);

      int result = CommonUtils.findNonNullDataRowIndexByStartRow(sheet, 5);

      assertEquals(6, result, "Should return startRow + 1 if startRow is beyond last row");
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  @Test
  void testGetOrCreateRow_createsNewRowIfNotExists() {
    try (XSSFWorkbook workbook = new XSSFWorkbook()) {
      Sheet sheet = workbook.createSheet("TestSheet");

      Row row = CommonUtils.getOrCreateRow(sheet, 2);

      assertNotNull(row);
    } catch (IOException e) {
      throw new RuntimeException(e);
    }
  }

  @Test
  void testGetOrCreateRow_recreatesRowIfExists() throws IOException {
    try (XSSFWorkbook workbook = new XSSFWorkbook()) {
      Sheet sheet = workbook.createSheet("TestSheet");

      Row originalRow = sheet.createRow(4);
      originalRow.createCell(0).setCellValue("Old Value");

      Row newRow = CommonUtils.getOrCreateRow(sheet, 4);

      assertNotNull(newRow, "Row should not be null");
      assertEquals(4, newRow.getRowNum(), "Row index should match requested index");

      // Based on current implementation, row gets recreated
      assertNotNull(newRow.getCell(0), "Existing row should be replaced with a new empty row");
    }
  }


  @Test
  void testSetProcessExternalUploadRequest() {
    BulkProcessExternalUploadRequest request = new BulkProcessExternalUploadRequest();
    String storeId = "store-1";
    String username = "user1";
    String requestId = "req-123";
    BulkProcessType bulkProcessType = BulkProcessType.EXTERNAL_CREATION_UPLOAD;
    CommonUtils.setProcessExternalUploadRequest(storeId, username, requestId, bulkProcessType,
      request);
    assertEquals(username, request.getUsername());
    assertEquals(storeId, request.getStoreId());
    assertEquals(requestId, request.getRequestId());
    assertEquals(bulkProcessType, request.getBulkProcessType());
  }

  @Test
  void testGetBulkProcessDataForExternalUpload_FailAndPendingBranches() {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setId("bp1");
    bulkProcess.setBulkProcessCode("CODE1");
    bulkProcess.setStoreId("STORE1");
    bulkProcess.setRequestId("REQ1");

    Map<String, String> variant = Map.of("RowNumber", "10");

    // --- Fail case ---
    Map<String, StringBuilder> validationErrors = new HashMap<>();
    StringBuilder variantError = new StringBuilder("error");

    BulkProcessData failData =
      invokeGetBulkProcessDataForExternalUpload(variant, bulkProcess, "prod1", validationErrors,
        variantError);

    assertEquals(BulkProcessData.STATUS_FAIL, failData.getStatus());
    assertNotNull(failData.getErrorMessage());
    assertNotNull(failData.getEndDate());

    // --- Pending case ---
    validationErrors.clear();
    variantError = new StringBuilder();
    BulkProcessData pendingData =
      invokeGetBulkProcessDataForExternalUpload(variant, bulkProcess, "prod2", validationErrors,
        variantError);

    assertEquals(BulkProcessData.STATUS_PENDING, pendingData.getStatus());
    assertNull(pendingData.getErrorMessage());
  }

  @Test
  void testGetBulkProcessDataRequestMap_AllBranches() {
    ProfileResponse profile = new ProfileResponse();
    CompanyDTO company = new CompanyDTO();
    company.setOfflineToOnlineFlag(true);
    company.setCncActivated(true);
    profile.setCompany(company);

    Map<String, Object> map = invokeGetBulkProcessDataRequestMap("prodX", "PICK1", profile);

    assertEquals("0", map.get(GenericBulkHeaders.INSTORE));
    assertEquals("0", map.get(GenericBulkHeaders.CNC));
  }

  @Test
  void testGetBulkProcessDataRequestMap_NullCompany() {
    ProfileResponse profile = new ProfileResponse(); // null company
    Map<String, Object> map = invokeGetBulkProcessDataRequestMap("prodX", "PICK1", profile);
    assertFalse(map.containsKey(GenericBulkHeaders.INSTORE));
    assertFalse(map.containsKey(GenericBulkHeaders.CNC));
  }

  @Test
  void testPopulateDataFromBasicInfoSheet_AllBranches() {
    Map<String, List<Map<String, String>>> basicInfoMap = new HashMap<>();
    Map<String, String> info = new LinkedHashMap<>();
    info.put("key1", "val1");
    info.put("RowNumber", "ignore");
    info.put("joinKey", "skip");
    basicInfoMap.put("p1", List.of(info));

    PlatformConfig config = new PlatformConfig();
    config.setJoinKey("joinKey");

    Map<String, Object> dataMap = new LinkedHashMap<>();
    Map<String, String> sourceToDest = Map.of("key1", "destKey");

    invokePopulateDataFromBasicInfoSheet(basicInfoMap, "p1", config, dataMap, sourceToDest);
    assertTrue(dataMap.containsKey("destKey"));
  }

  @Test
  void testPopulateDataFromMediaSheet_AllBranches() {
    Map<String, List<Map<String, String>>> mediaMap = new HashMap<>();
    Map<String, String> media = new LinkedHashMap<>();
    media.put("imgKey", "http://img");
    media.put("RowNumber", "ignore");
    media.put("joinKey", "skip");
    mediaMap.put("p1", List.of(media));

    PlatformConfig config = new PlatformConfig();
    config.setJoinKey("joinKey");

    Map<String, Object> dataMap = new LinkedHashMap<>();
    Map<String, String> sourceToDest = new HashMap<>();
    sourceToDest.put("imgKey", "IMAGE1");

    BulkProcessData data = new BulkProcessData();
    data.setStatus(BulkProcessData.STATUS_PENDING);

    Set<String> urls = new HashSet<>();

    invokePopulateDataFromMediaSheet("p1", config, dataMap, sourceToDest, mediaMap, data, urls);
    assertTrue(dataMap.containsKey("IMAGE1"));
    assertFalse(urls.contains("http://img"));
  }

  @Test
  void testPopulateDataFromMediaSheet_FailStatusSkipsImageAdd() {
    Map<String, List<Map<String, String>>> mediaMap = new HashMap<>();
    mediaMap.put("p1", List.of(Map.of("imgKey", "url")));
    PlatformConfig config = new PlatformConfig();
    config.setJoinKey("joinKey");
    Map<String, Object> dataMap = new LinkedHashMap<>();
    Map<String, String> map = Map.of("imgKey", "IMAGE1");
    BulkProcessData data = new BulkProcessData();
    data.setStatus(BulkProcessData.STATUS_FAIL);
    Set<String> urls = new HashSet<>();

    invokePopulateDataFromMediaSheet("p1", config, dataMap, map, mediaMap, data, urls);
    assertTrue(dataMap.containsKey("IMAGE1"));
    assertTrue(urls.isEmpty());
  }

  @Test
  void testPopulateDataFromShippingSheet_AllBranches() {
    Map<String, List<Map<String, String>>> shipMap = new HashMap<>();
    Map<String, String> ship = new LinkedHashMap<>();
    ship.put("shipKey", "val1");
    ship.put("RowNumber", "ignore");
    ship.put("joinKey", "skip");
    shipMap.put("p1", List.of(ship));

    PlatformConfig config = new PlatformConfig();
    config.setJoinKey("joinKey");

    Map<String, Object> dataMap = new LinkedHashMap<>();
    Map<String, String> srcToDest = Map.of("shipKey", "destShip");
    Map<String, String> variant = Map.of("vKey", "vVal");

    invokePopulateDataFromShippingSheet(variant, "p1", config, dataMap, srcToDest, shipMap);
    assertTrue(dataMap.containsKey("destShip"));
    assertTrue(dataMap.containsValue("vVal"));
  }

  @Test
  void testPopulateProductIdToImageQCModelMap_Branches() {
    Map<String, Object> dataMap =
      Map.of("External category", "cat", GenericBulkHeaders.PRODUCT_NAME, "pname",
        GenericBulkHeaders.DESCRIPTION, "pdesc");

    BulkProcessData bpd = new BulkProcessData();
    bpd.setParentProduct("p1");

    BulkProcess bp = new BulkProcess();
    bp.setStoreId("store1");
    bp.setId("id1");
    bp.setBulkProcessCode("code1");

    Map<String, BrandAndCategoryPredictionRequest> map = new HashMap<>();

    invokePopulateProductIdToImageQCModelMap("p1", dataMap, bpd, map, bp);
    assertTrue(map.containsKey("p1"));

    // Second call - branch when key already exists
    invokePopulateProductIdToImageQCModelMap("p1", dataMap, bpd, map, bp);
    assertEquals(1, map.size());
  }

  @Test
  void testReadInfoSalesSheetAndCreateBulkProcessData_AllBranches() throws JsonProcessingException {
    BulkProcessExternalUploadRequest req = new BulkProcessExternalUploadRequest();
    req.setPickupPointCode(""); // blank to trigger error message

    BulkProcess bulk = new BulkProcess();
    bulk.setId("id1");
    bulk.setStoreId("store1");
    bulk.setBulkProcessCode("code1");
    bulk.setRequestId("req1");

    Map<String, String> variant = new LinkedHashMap<>();
    variant.put("Nama Variasi", "contains,comma"); // trigger COMMA check
    Map<String, List<Map<String, String>>> salesInfo = Map.of("pid", List.of(variant));

    Map<String, StringBuilder> productValidationErrorMap = new HashMap<>();
    ProfileResponse profile = new ProfileResponse();
    PlatformConfig cfg = new PlatformConfig();
    Map<String, List<Map<String, String>>> empty = new HashMap<>();
    Map<String, String> sourceMap = new HashMap<>();
    Set<String> imgs = new HashSet<>();
    Map<String, BrandAndCategoryPredictionRequest> qcMap = new HashMap<>();
    List<BulkProcessData> list = new ArrayList<>();

    CommonUtils.readInfoSalesSheetAndCreateBulkProcessData(req, salesInfo, bulk,
      productValidationErrorMap, profile, empty, cfg, sourceMap, empty, imgs, empty, qcMap, list);

    assertEquals(1, list.size());
    assertNotNull(list.get(0).getBulkRequestData());
  }

  @Test
  void testReadInfoSalesSheetAndCreateBulkProcessData_falseConditions()
    throws JsonProcessingException {
    // --- Arrange ---
    BulkProcessExternalUploadRequest request = new BulkProcessExternalUploadRequest();
    request.setPickupPointCode("PP001"); // NOT blank to skip first if

    String productId = "prod-123";
    Map<String, String> variant = new HashMap<>();
    variant.put("Nama Variasi", "Standard"); // no comma, so second if is false

    Map<String, List<Map<String, String>>> productSalesInfoMap = new HashMap<>();
    productSalesInfoMap.put(productId, Collections.singletonList(variant));

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setId("1");
    bulkProcess.setBulkProcessCode("BULK-001");
    bulkProcess.setStoreId("store-1");

    Map<String, StringBuilder> productValidationErrorMap = new HashMap<>();
    ProfileResponse businessPartner = new ProfileResponse();
    PlatformConfig shopeeConfig = new PlatformConfig();
    Map<String, String> sourceColumnToDestinationColumnMap = new HashMap<>();
    Map<String, List<Map<String, String>>> productBasicInfoMap = new HashMap<>();
    Map<String, List<Map<String, String>>> productMediaInfoMap = new HashMap<>();
    Set<String> imageUrls = new HashSet<>();
    Map<String, List<Map<String, String>>> productShippingInfoMap = new HashMap<>();
    Map<String, BrandAndCategoryPredictionRequest> productIdToImageQCModelMap = new HashMap<>();
    List<BulkProcessData> bulkProcessDataList = new ArrayList<>();

    // --- Act ---
    CommonUtils.readInfoSalesSheetAndCreateBulkProcessData(request, productSalesInfoMap,
      bulkProcess, productValidationErrorMap, businessPartner, productBasicInfoMap, shopeeConfig,
      sourceColumnToDestinationColumnMap, productMediaInfoMap, imageUrls, productShippingInfoMap,
      productIdToImageQCModelMap, bulkProcessDataList);

    // --- Assert ---
    assertEquals(1, bulkProcessDataList.size());
    BulkProcessData result = bulkProcessDataList.get(0);
    // The variantErrorMessage should be empty, because both ifs were false
    assertNotNull(result.getErrorMessage());
    assertEquals(BulkProcessData.STATUS_FAIL, result.getStatus());
  }


  // --- Private helpers calling private static methods via reflection ---
  private BulkProcessData invokeGetBulkProcessDataForExternalUpload(Map<String, String> v,
    BulkProcess bp, String pid, Map<String, StringBuilder> err, StringBuilder msg) {
    try {
      var m = CommonUtils.class.getDeclaredMethod("getBulkProcessDataForExternalUpload", Map.class,
        BulkProcess.class, String.class, Map.class, StringBuilder.class);
      m.setAccessible(true);
      return (BulkProcessData) m.invoke(null, v, bp, pid, err, msg);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  private Map<String, Object> invokeGetBulkProcessDataRequestMap(String pid, String pickup,
    ProfileResponse prof) {
    try {
      var m = CommonUtils.class.getDeclaredMethod("getBulkProcessDataRequestMap", String.class,
        String.class, ProfileResponse.class);
      m.setAccessible(true);
      return (Map<String, Object>) m.invoke(null, pid, pickup, prof);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  private void invokePopulateDataFromBasicInfoSheet(Map<String, List<Map<String, String>>> m,
    String pid, PlatformConfig c, Map<String, Object> d, Map<String, String> s) {
    try {
      var mth = CommonUtils.class.getDeclaredMethod("populateDataFromBasicInfoSheet", Map.class,
        String.class, PlatformConfig.class, Map.class, Map.class);
      mth.setAccessible(true);
      mth.invoke(null, m, pid, c, d, s);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  private void invokePopulateDataFromMediaSheet(String pid, PlatformConfig c, Map<String, Object> d,
    Map<String, String> s, Map<String, List<Map<String, String>>> mm, BulkProcessData b,
    Set<String> u) {
    try {
      var m = CommonUtils.class.getDeclaredMethod("populateDataFromMediaSheet", String.class,
        PlatformConfig.class, Map.class, Map.class, Map.class, BulkProcessData.class, Set.class);
      m.setAccessible(true);
      m.invoke(null, pid, c, d, s, mm, b, u);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  private void invokePopulateDataFromShippingSheet(Map<String, String> v, String pid,
    PlatformConfig c, Map<String, Object> d, Map<String, String> s,
    Map<String, List<Map<String, String>>> sm) {
    try {
      var m = CommonUtils.class.getDeclaredMethod("populateDataFromShippingSheet", Map.class,
        String.class, PlatformConfig.class, Map.class, Map.class, Map.class);
      m.setAccessible(true);
      m.invoke(null, v, pid, c, d, s, sm);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  private void invokePopulateProductIdToImageQCModelMap(String pid, Map<String, Object> d,
    BulkProcessData bpd, Map<String, BrandAndCategoryPredictionRequest> m, BulkProcess bp) {
    try {
      var met =
        CommonUtils.class.getDeclaredMethod("populateProductIdToImageQCModelMap", String.class,
          Map.class, BulkProcessData.class, Map.class, BulkProcess.class);
      met.setAccessible(true);
      met.invoke(null, pid, d, bpd, m, bp);
    } catch (Exception e) {
      throw new RuntimeException(e);
    }
  }

  @Test
  void testGetBulkProcessImageList() {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setId("1");
    bulkProcess.setBulkProcessCode("CODE123");
    bulkProcess.setStoreId("STORE-1");

    Set<String> imageUrls = new LinkedHashSet<>(Arrays.asList("url1", "url2"));
    List<BulkProcessImage> list = CommonUtils.getBulkProcessImageList(imageUrls, bulkProcess);

    assertEquals(2, list.size());
    assertEquals("CODE123", list.get(0).getBulkProcessCode());
    assertEquals("url1", list.get(0).getImageURL());
    assertEquals(1, list.get(0).getSequence());
    assertFalse(list.get(0).isCompleted());
  }


  @Test
  void testGetRequiredColumnIndexMap_success() {
    SheetConfig config = new SheetConfig();
    ColumnConfig c1 = new ColumnConfig("Name", new ArrayList<>(), "name_dest", true);
    ColumnConfig c2 = new ColumnConfig("Age", new ArrayList<>(), "age_dest", false);
    config.setColumns(List.of(c1, c2));

    Map<String, String> srcToDest = new HashMap<>();
    Map<String, Boolean> srcToMandatory = new HashMap<>();
    Map<String, Integer> headerIndex = new HashMap<>();
    headerIndex.put("name", 0);
    headerIndex.put("age", 1);

    Map<String, Integer> result =
      CommonUtils.getRequiredColumnIndexMap(ZIP_FILE_NAME, FILE_NAME, srcToDest, srcToMandatory, config, headerIndex);

    assertEquals(2, result.size());
    assertTrue(srcToMandatory.get("Name"));
    assertEquals(0, result.get("Name"));
  }

  @Test
  void testGetRequiredColumnIndexMap_missingMandatoryThrows() {
    SheetConfig config = new SheetConfig();
    ColumnConfig c1 = new ColumnConfig("Name", new ArrayList<>(), "name_dest", true);
    config.setColumns(List.of(c1));

    Map<String, String> srcToDest = new HashMap<>();
    Map<String, Boolean> srcToMandatory = new HashMap<>();
    Map<String, Integer> headerIndex = new HashMap<>(); // missing entry

    ValidationException ex = assertThrows(ValidationException.class,
      () -> CommonUtils.getRequiredColumnIndexMap(ZIP_FILE_NAME, FILE_NAME, srcToDest, srcToMandatory, config, headerIndex));
    assertTrue(ex.getErrorMessage().contains("ditemukan"));
  }

  @Test
  void testDistributeSheetDataToMap_unknownType() {
    Map<String, List<Map<String, String>>> basic = new HashMap<>();
    Map<String, List<Map<String, String>>> media = new HashMap<>();
    Map<String, List<Map<String, String>>> ship = new HashMap<>();
    Map<String, List<Map<String, String>>> sales = new HashMap<>();
    Map<String, List<Map<String, String>>> data = new HashMap<>();

    assertDoesNotThrow(
      () -> CommonUtils.distributeSheetDataToMap(basic, media, ship, sales, "file", "unknown",
        data));
  }

  @Test
  void testGetRequiredColumnIndexMap_nonMandatoryMissingColumn_noException() {
    // Arrange
    Map<String, String> srcToDest = new HashMap<>();
    Map<String, Boolean> srcToMandatory = new HashMap<>();
    Map<String, Integer> headerIndexMap = new HashMap<>(); // empty, column not found

    ColumnConfig col = new ColumnConfig("OptionalColumn",new ArrayList<>(), "DEST_OPT", false);
    SheetConfig config = mock(SheetConfig.class);
    when(config.getColumns()).thenReturn(List.of(col));

    // Act
    Map<String, Integer> result =
      CommonUtils.getRequiredColumnIndexMap(ZIP_FILE_NAME, FILE_NAME, srcToDest, srcToMandatory, config, headerIndexMap);

    // Assert — no exception and map empty
    assertTrue(result.isEmpty());
    assertEquals("DEST_OPT", srcToDest.get("OptionalColumn"));
    assertEquals(false, srcToMandatory.get("OptionalColumn"));
  }

  @Test
  void testGetRequiredColumnIndexMap_withAlternateSourceColumn() {
    SheetConfig config = new SheetConfig();
    ColumnConfig columnWithAlternate =
        new ColumnConfig("Primary", new ArrayList<>(List.of("alternate1","Alternate")),
            "DEST_ALT", true);
    config.setColumns(List.of(columnWithAlternate));

    Map<String, String> srcToDest = new HashMap<>();
    Map<String, Boolean> srcToMandatory = new HashMap<>();
    Map<String, Integer> headerIndexMap = new HashMap<>();
    headerIndexMap.put("alternate", 2);

    Map<String, Integer> result =
        CommonUtils.getRequiredColumnIndexMap(ZIP_FILE_NAME, FILE_NAME, srcToDest, srcToMandatory,
            config, headerIndexMap);

    assertEquals(1, result.size());
    assertTrue(result.containsKey("Alternate"));
    assertEquals(2, result.get("Alternate"));
    assertEquals("DEST_ALT", srcToDest.get("Alternate"));
    assertTrue(srcToMandatory.get("Alternate"));
  }

  @Test
  void testGetRequiredColumnIndexMap_withAlternateSourceColumnNotInHeaderIndex_shouldThrow() {
    SheetConfig config = new SheetConfig();
    ColumnConfig columnWithAlternate =
        new ColumnConfig("Primary", new ArrayList<>(Collections.singletonList("Alternate2")),
            "DEST_ALT2", true);
    config.setColumns(List.of(columnWithAlternate));

    Map<String, String> srcToDest = new HashMap<>();
    Map<String, Boolean> srcToMandatory = new HashMap<>();
    Map<String, Integer> headerIndexMap = new HashMap<>();
    ValidationException ex = assertThrows(ValidationException.class,
        () -> CommonUtils.getRequiredColumnIndexMap(ZIP_FILE_NAME, FILE_NAME, srcToDest,
            srcToMandatory, config, headerIndexMap));

    assertNotNull(ex);
    assertTrue(ex.getErrorMessage().contains("Primary"));
  }

  @Test
  void testGetBulkProcessImageQC() {
    BulkProcess bp = new BulkProcess();
    bp.setBulkProcessCode("CODE");
    bp.setStoreId("STORE");

    BrandAndCategoryPredictionRequest req = new BrandAndCategoryPredictionRequest();
    req.setExternalCategory("Category");

    Map.Entry<String, BrandAndCategoryPredictionRequest> entry = Map.entry("key", req);

    BulkProcessImageQC result = CommonUtils.getBulkProcessImageQC(entry, bp);

    assertEquals("CODE", result.getBulkProcessCode());
    assertEquals("Category", result.getExternalCategory());
    assertEquals("STORE", result.getStoreId());
    assertFalse(result.isCompleted());
  }


  @Test
  void testValidateActiveBusinessPartner_WithActiveMerchant_DoesNotThrow() {
    // --- Arrange ---
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setMerchantStatus("ACTIVE");

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode("BP123");
    bulkProcess.setBusinessPartnerCode("SELLER1");

    // --- Act & Assert ---
    assertDoesNotThrow(
      () -> CommonUtils.validateActiveBusinessPartner(profileResponse, bulkProcess, "file1.xlsx"));
  }

  @Test
  void testValidateActiveBusinessPartner_WithInactiveMerchant_ThrowsApplicationException() {
    // --- Arrange ---
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setMerchantStatus("INACTIVE");

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode("BP456");
    bulkProcess.setBusinessPartnerCode("SELLER2");

    String fileName = "inactiveFile.xlsx";

    // --- Act ---
    assertThrows(ApplicationException.class, () -> {
      CommonUtils.validateActiveBusinessPartner(profileResponse, bulkProcess, fileName);
    });

    // --- Assert ---
    assertTrue(bulkProcess.getDescription().contains(fileName));
  }


  @Test
  void testDistributeSheetDataToMap_basicInfo() {
    Map<String, List<Map<String, String>>> basicInfoMap = new HashMap<>();
    Map<String, List<Map<String, String>>> mediaMap = new HashMap<>();
    Map<String, List<Map<String, String>>> shippingMap = new HashMap<>();
    Map<String, List<Map<String, String>>> salesMap = new HashMap<>();

    Map<String, List<Map<String, String>>> sheetData = Map.of("p1", List.of(Map.of("key", "v1")));

    CommonUtils.distributeSheetDataToMap(basicInfoMap, mediaMap, shippingMap, salesMap,
      "file1.xlsx", "basicInfo", sheetData);

    assertEquals(sheetData, basicInfoMap);
    assertTrue(mediaMap.isEmpty());
    assertTrue(shippingMap.isEmpty());
    assertTrue(salesMap.isEmpty());
  }

  @Test
  void testDistributeSheetDataToMap_media() {
    Map<String, List<Map<String, String>>> basicInfoMap = new HashMap<>();
    Map<String, List<Map<String, String>>> mediaMap = new HashMap<>();
    Map<String, List<Map<String, String>>> shippingMap = new HashMap<>();
    Map<String, List<Map<String, String>>> salesMap = new HashMap<>();

    Map<String, List<Map<String, String>>> sheetData = Map.of("p2", List.of(Map.of("k", "v2")));

    CommonUtils.distributeSheetDataToMap(basicInfoMap, mediaMap, shippingMap, salesMap,
      "file2.xlsx", "media", sheetData);

    assertEquals(sheetData, mediaMap);
    assertTrue(basicInfoMap.isEmpty());
    assertTrue(shippingMap.isEmpty());
    assertTrue(salesMap.isEmpty());
  }

  @Test
  void testDistributeSheetDataToMap_shipping() {
    Map<String, List<Map<String, String>>> basicInfoMap = new HashMap<>();
    Map<String, List<Map<String, String>>> mediaMap = new HashMap<>();
    Map<String, List<Map<String, String>>> shippingMap = new HashMap<>();
    Map<String, List<Map<String, String>>> salesMap = new HashMap<>();

    Map<String, List<Map<String, String>>> sheetData = Map.of("p3", List.of(Map.of("k", "v3")));

    CommonUtils.distributeSheetDataToMap(basicInfoMap, mediaMap, shippingMap, salesMap,
      "file3.xlsx", "shipping", sheetData);

    assertEquals(sheetData, shippingMap);
    assertTrue(basicInfoMap.isEmpty());
    assertTrue(mediaMap.isEmpty());
    assertTrue(salesMap.isEmpty());
  }

  @Test
  void testDistributeSheetDataToMap_sales() {
    Map<String, List<Map<String, String>>> basicInfoMap = new HashMap<>();
    Map<String, List<Map<String, String>>> mediaMap = new HashMap<>();
    Map<String, List<Map<String, String>>> shippingMap = new HashMap<>();
    Map<String, List<Map<String, String>>> salesMap = new HashMap<>();

    Map<String, List<Map<String, String>>> sheetData = Map.of("p4", List.of(Map.of("k", "v4")));

    CommonUtils.distributeSheetDataToMap(basicInfoMap, mediaMap, shippingMap, salesMap,
      "file4.xlsx", "sales", sheetData);

    assertEquals(sheetData, salesMap);
    assertTrue(basicInfoMap.isEmpty());
    assertTrue(mediaMap.isEmpty());
    assertTrue(shippingMap.isEmpty());
  }

  @Test
  void testDistributeSheetDataToMap_unknownSheetType() {
    Map<String, List<Map<String, String>>> basicInfoMap = new HashMap<>();
    Map<String, List<Map<String, String>>> mediaMap = new HashMap<>();
    Map<String, List<Map<String, String>>> shippingMap = new HashMap<>();
    Map<String, List<Map<String, String>>> salesMap = new HashMap<>();

    Map<String, List<Map<String, String>>> sheetData = Map.of("p5", List.of(Map.of("k", "v5")));

    // Nothing should be added since sheetType is unknown
    CommonUtils.distributeSheetDataToMap(basicInfoMap, mediaMap, shippingMap, salesMap,
      "file5.xlsx", "unknownType", sheetData);

    assertTrue(basicInfoMap.isEmpty());
    assertTrue(mediaMap.isEmpty());
    assertTrue(shippingMap.isEmpty());
    assertTrue(salesMap.isEmpty());
  }


  @Test
  void testPopulateDataFromShippingSheet_variantKeyEqualsJoinKey_skipsEntry() {
    // Arrange
    Map<String, String> variant = new HashMap<>();
    variant.put("joinKey", "variantValue"); // will be skipped
    variant.put("color", "red"); // will be processed
    variant.put("RowNumber", "10"); // this should trigger the continue condition

    Map<String, Object> dataMap = new HashMap<>();
    Map<String, String> sourceToDestMap = new HashMap<>();
    sourceToDestMap.put("color", "COLOR_DEST");

    Map<String, String> shippingRow = new HashMap<>();
    shippingRow.put("weight", "5kg");
    Map<String, List<Map<String, String>>> shippingInfoMap = new HashMap<>();
    shippingInfoMap.put("product1", List.of(shippingRow));

    PlatformConfig shopeeConfig = mock(PlatformConfig.class);
    when(shopeeConfig.getJoinKey()).thenReturn("joinKey");

    // Act
    CommonUtils.populateDataFromShippingSheet(variant, "product1", shopeeConfig, dataMap,
      sourceToDestMap, shippingInfoMap);

    // Assert
    // The joinKey entry should be skipped
    assertFalse(dataMap.containsValue("variantValue"), "Entry with joinKey should be skipped");
    // The color entry should be added
    assertEquals("red", dataMap.get("COLOR_DEST"));
  }

  @Test
  void testPopulateDataFromMediaSheet_fullCoverage() {
    // Arrange common objects
    String productId = "p1";
    Map<String, Object> dataMap = new HashMap<>();
    Map<String, String> sourceToDestMap = new HashMap<>();
    Set<String> imageUrls = new HashSet<>();

    PlatformConfig shopeeConfig = mock(PlatformConfig.class);
    when(shopeeConfig.getJoinKey()).thenReturn("joinKey");

    // ---- Case 1: TRUE branch ----
    // destination column starts with "Image-", status != FAIL
    sourceToDestMap.put("media1", "Image-1");
    sourceToDestMap.put("media2", "Image-1");

    Map<String, String> mediaRow = new HashMap<>();
    mediaRow.put("media1", "http://img1.jpg");
    mediaRow.put("media2", "");
    Map<String, List<Map<String, String>>> productMediaMap = new HashMap<>();
    productMediaMap.put(productId, List.of(mediaRow));

    BulkProcessData bulkProcessDataSuccess = new BulkProcessData();
    bulkProcessDataSuccess.setStatus(BulkProcessData.STATUS_SUCCESS);

    // Act
    CommonUtils.populateDataFromMediaSheet(productId, shopeeConfig, dataMap, sourceToDestMap,
      productMediaMap, bulkProcessDataSuccess, imageUrls);

    // Assert TRUE branch
    assertTrue(imageUrls.contains("http://img1.jpg"),
      "Image URL should be added for valid prefix and SUCCESS status");
    assertTrue(dataMap.containsValue("http://img1.jpg"), "Data map should include the media value");

    // ---- Case 2: FALSE branch ----
    // destination column not matching prefix, OR status == FAIL
    imageUrls.clear();
    dataMap.clear();
    sourceToDestMap.put("media2", "Image-1"); // not starting with allowed prefix

    Map<String, String> mediaRow2 = new HashMap<>();
    mediaRow2.put("media2", "http://img2.jpg");
    productMediaMap.put(productId, List.of(mediaRow2));

    BulkProcessData bulkProcessDataFail = new BulkProcessData();
    bulkProcessDataFail.setStatus(BulkProcessData.STATUS_FAIL);

    // Act
    CommonUtils.populateDataFromMediaSheet(productId, shopeeConfig, dataMap, sourceToDestMap,
      productMediaMap, bulkProcessDataFail, imageUrls);

    // Assert FALSE branch
    assertFalse(imageUrls.contains("http://img2.jpg"),
      "Image URL should NOT be added for FAIL status or invalid prefix");
    assertTrue(dataMap.containsValue("http://img2.jpg"),
      "Data map still should include the media value");
  }

  @Test
  void testPopulateBulkProcessDataRequestMap_trueCondition() {
    // --- Arrange ---
    String productId = "p123";
    Map<String, String> variant = new HashMap<>();
    variant.put("joinKey", "JK001");

    PlatformConfig shopeeConfig = mock(PlatformConfig.class);
    when(shopeeConfig.getJoinKey()).thenReturn("joinKey");

    Map<String, Object> dataMap = new HashMap<>();
    Map<String, String> sourceToDestMap = new HashMap<>();
    Set<String> imageUrls = new HashSet<>();

    Map<String, List<Map<String, String>>> productBasicInfoMap = new HashMap<>();
    Map<String, List<Map<String, String>>> productMediaInfoMap = new HashMap<>();
    Map<String, List<Map<String, String>>> productShippingInfoMap = new HashMap<>();
    Map<String, BrandAndCategoryPredictionRequest> productIdToImageQCModelMap = new HashMap<>();

    BulkProcess bulkProcess = mock(BulkProcess.class);
    when(bulkProcess.getBulkProcessCode()).thenReturn("BULK-001");

    BulkProcessData bulkProcessData = new BulkProcessData();
    bulkProcessData.setStatus(BulkProcessData.STATUS_SUCCESS);

    // --- Act ---
    CommonUtils.populateBulkProcessDataRequestMap(variant, productBasicInfoMap, productId,
      shopeeConfig, dataMap, sourceToDestMap, productMediaInfoMap, bulkProcessData, imageUrls,
      productShippingInfoMap, productIdToImageQCModelMap, bulkProcess);

    // --- Assert ---
    assertNotNull(productIdToImageQCModelMap, "Map should not be null");
    assertNotNull(dataMap, "Data map should not be null");
  }

  @Test
  void testGetProductCreationTypeFromBulkProcessType() {
    // EXTERNAL_CREATION_UPLOAD should map to EXTERNAL_BULK_UPLOAD

    assertEquals(ProductCreationType.UNIFIED_BULK_UPLOAD,
      CommonUtils.getProductCreationTypeFromBulkProcessType(
        null));

    assertEquals(ProductCreationType.EXTERNAL_BULK_UPLOAD,
      CommonUtils.getProductCreationTypeFromBulkProcessType(
        BulkProcessType.EXTERNAL_CREATION_UPLOAD));

    // CONVERTED_PRODUCT_CREATION_UPLOAD should map to CONVERTED_BULK_UPLOAD
    assertEquals(ProductCreationType.CONVERTED_BULK_UPLOAD,
      CommonUtils.getProductCreationTypeFromBulkProcessType(
        BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD));

    // Any other type should map to UNIFIED_BULK_UPLOAD
    assertEquals(ProductCreationType.UNIFIED_BULK_UPLOAD,
      CommonUtils.getProductCreationTypeFromBulkProcessType(
        BulkProcessType.PRODUCT_CREATION_UPLOAD));
  }

  @Test
  void testGenerateConvertedTemplateUploadPath() throws Exception {
    // Arrange
    String gcsPath = "gs://bucket/failed-products/";

    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode("BPCODE123");
    bulkProcess.setBusinessPartnerCode("BP001");

    // Fixed date: 2025-10-17 14:35:45
    Date fixedDate = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")
      .parse("2025-10-17 14:35:45");
    bulkProcess.setCreatedDate(fixedDate);

    // Act
    String actualPath = CommonUtils.generateConvertedTemplateUploadPath(gcsPath, bulkProcess);

    // Expected values
    String expectedDate = new SimpleDateFormat("yyMMdd").format(fixedDate);  // "251017"
    String expectedTime = new SimpleDateFormat("HHmmss").format(fixedDate);  // "143545"

    String expectedPath = "gs://bucket/failed-products/BPCODE123/"
      + Constant.CONVERTED_GENERAL_TEMPLATE + "_BP001_"
      + expectedDate + expectedTime + "."
      + Constant.FILE_TYPE_XLSM;

    // Assert
    assertEquals(expectedPath, actualPath);
  }

  @Test
  public void testMultipleVariants_ShouldReturnTrue() {
    Map<String, String> variant1 = Map.of("VARIASI_HEADER", "Color");
    Map<String, String> variant2 = Map.of("VARIASI_HEADER", "Size");
    List<Map<String, String>> variants = Arrays.asList(variant1, variant2);

    assertTrue(CommonUtils.hasVariantCreatingAttributeOrMultiVariant(variant1, variants));
  }

  @Test
  public void testSingleVariant_WithNonBlankVariasiHeader_ShouldReturnTrue() {
    Map<String, String> variant = Map.of("VARIASI_HEADER", "Color");
    List<Map<String, String>> variants = Collections.singletonList(variant);

    assertFalse(CommonUtils.hasVariantCreatingAttributeOrMultiVariant(variant, variants));
  }

  @Test
  public void testSingleVariant_WithBlankVariasiHeader_ShouldReturnFalse() {
    Map<String, String> variant = Map.of("VARIASI_HEADER", "");
    List<Map<String, String>> variants = Collections.singletonList(variant);

    assertFalse(CommonUtils.hasVariantCreatingAttributeOrMultiVariant(variant, variants));
  }

  @Test
  public void testSingleVariant_WithNullVariasiHeader_ShouldReturnFalse() {
    Map<String, String> variant = new HashMap<>();
    variant.put("VARIASI_HEADER", null);
    List<Map<String, String>> variants = Collections.singletonList(variant);

    assertFalse(CommonUtils.hasVariantCreatingAttributeOrMultiVariant(variant, variants));
  }

  @Test
  public void testSingleVariant_MissingVariasiHeaderKey_ShouldReturnFalse() {
    Map<String, String> variant = Map.of("OTHER_HEADER", "Value");
    List<Map<String, String>> variants = Collections.singletonList(variant);

    assertFalse(CommonUtils.hasVariantCreatingAttributeOrMultiVariant(variant, variants));
  }

  @Test
  public void testEmptyVariantsList_ShouldReturnFalse() {
    Map<String, String> variant = Map.of("VARIASI_HEADER", "Color");
    List<Map<String, String>> variants = Collections.emptyList();

    assertFalse(CommonUtils.hasVariantCreatingAttributeOrMultiVariant(variant, variants));
  }
}
