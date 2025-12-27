package com.gdn.mta.bulk.util;

import java.text.ParseException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.UUID;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductLevel3PriceResponse;
import com.gda.mta.product.dto.ProductLevel3ViewConfigResponse;
import com.gda.mta.product.dto.response.ItemPickupPointListingL3Response;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.dto.*;
import com.gdn.mta.bulk.dto.inventory.InventoryDetailInfoRequestDTO;
import com.gdn.mta.bulk.dto.product.FbbL5CreateDTO;
import com.gdn.mta.bulk.entity.BulkDownloadEntity;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.constants.BulkProcessConstant;
import com.gdn.mta.bulk.models.BrandAuthAddRequestData;
import com.gdn.mta.bulk.models.BrandAuthDeleteRequestData;
import com.gdn.mta.bulk.models.BulkPriceUpdateRequestData;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.RestrictedKeywordRequestData;
import com.gdn.mta.bulk.models.SimpleListAssemblyDisassemblyRequest;
import com.gdn.mta.bulk.models.TransferRequest;
import com.gdn.mta.bulk.models.WorkOrderDataModel;
import com.gdn.mta.bulk.models.download.BrandAuthDownloadRequest;
import com.gdn.mta.bulk.models.download.BrandAuthFilterRequest;
import com.gdn.mta.bulk.models.download.UpdateRemoveProductTaggingRequest;
import com.gdn.mta.bulk.models.download.VendorAutoAssignmentFilterRequest;
import com.gdn.mta.bulk.models.download.VendorAutoAssignmentRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkAddReviewIPRProductsRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkAssignAutoApprovedProductsRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkMasterSkuReviewRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkApprovalRejectionRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkPriceProductTypeTaggingRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkPriceRebateRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkRebateUpdateRequest;
import com.gdn.mta.bulk.service.FileStorageService;
import com.gdn.partners.bulk.util.*;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemDetailResponse;
import com.gdn.partners.pbp.dto.offlineitem.DeleteOfflineItemRequest;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.mta.bulk.models.IprActionRequest;
import com.gdn.x.mta.distributiontask.model.enums.ProductStateIPR;
import com.gdn.x.mta.distributiontask.rest.model.request.BoostedProductFilterRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.RejectProductVendorRequest;
import com.gdn.x.mta.distributiontask.rest.model.request.VendorQuickApprovalRequest;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.BundleRecipeV2Response;
import com.gdn.x.product.rest.web.model.response.ItemBasicDetailV2Response;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointListingResponse;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.core.JsonProcessingException;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthDeleteRequest;

import static com.gdn.mta.bulk.BulkInternalProcessType.FBB_L5_CREATE;
import static com.gdn.partners.bulk.util.BulkIPRProductsParameter.BRAND_REPORT;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.mockito.Mockito.when;

public class RequestHelperTest {

  private static final String STORE_COPY = "STORE_COPY";
  private static final String SALES_CATEGORY_UPDATE = "SALES_CATEGORY_UPDATE";
  private static final String DUMMY_PROCESS_TYPE = "DUMMY_PROCESS_TYPE";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String ITEM_SKU = "itemSku";
  private static final String MASTER_SKU = "masterSku";
  private static final String PICKUP_POINT_CODE = "ppCode";
  private static final String L5_CODE = "itemSku-ppCode";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_NAME = "productName";
  private static final String PRODUCT_SKU = "productSku";
  private static final String ACTION_VALUE = "IN_REVIEW";
  private static final String SOURCE = "SOURCE";
  private static final String CREATED_BY = "CREATED_BY";
  private static final String VIOLATION_TYPE_VALUE = "TRADEMARK";
  private static final String INVALID = "INVALID";
  private static final String REASON_VALUE = "FAKE";
  private static final String SELLER_NOTES = "SELLER_NOTES";
  private static final String REVIEWER_NOTES = "REVIEWER_NOTES";
  private static final String REPORT_DATE = "45940";
  private static final String REPORT_DATE_WRONG = "test";
  private static final String REPORT_DATE_NUMERIC_WRONG = "13/13/20254";
  private static final String REPORTER = "REPORTER";
  private static final String REPORTER_NAME = "REPORTER_NAME";
  private static final String REPORTER_EMAIL = "REPORTER_EMAIL";
  private static final String REPORTER_PHONE_NUMBER = "REPORTER_PHONE_NUMBER";
  private static final String REPORTER_ADDRESS = "REPORTER_ADDRESS";
  private static final String REPORTER_REASON = "REPORTER_REASON";
  private static final String STORE_NAME = "storeName";
  private static final String CATEGORY_NAME = "category";
  private static final String ASSIGNEE = "assignee";
  private static final String ASSIGNEE2 = "assignee2";
  private static final String STORE_ID = "10001";
  private static final String INTERNAL_PROCESS_CODE = "internal-process-code";
  private static final String KEYWORD = "Keyword";
  private static final String OTHERS = "Others";
  private static final String STRAIGHT_REJECTION = "Straight rejection";
  private static final String CATEGORY_CHANGE = "Change category to";
  private static final String NO_ACTION = "No action";
  private static final String SELLER_CODE="seller code";
  private static final String DEFAULT_USERNAME = "developer";
  private static final String DEFAULT_BULK_PROCESS_CODE = UUID.randomUUID().toString();
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BLI-00106";
  private static final String BRAND_NAME = "brandName";
  private static final String BRAND_CODE = "brandCode";
  private static final String STATUS = "status";
  public static final String ITEM_SKU1 = "SKU456";
  public static final String PICKUP_POINT_CODE1 = "PP003";
  public static final String EXTRA_HEADER_1 = "extra_header1";
  public static final String EXTRA_HEADER_2 = "extra_header2";
  public static final String INCORRECT_HEADER_1 = "incorrect_header1";
  public static final String INCORRECT_HEADER_2 = "incorrect_header2";
  private static final String DEFAULT_DESCRIPTION = "Test Description";

  private ItemPickupPointListingResponse itemPickupPointListingResponse =
    new ItemPickupPointListingResponse();
  private BulkInternalProcessUploadRequest uploadRequest = new BulkInternalProcessUploadRequest();
  private FbbL5CreateDTO fbbL5CreateDTO;
  Map<RestrictedKeywordRequestData, String> requestResponseMap;
  Map<Map<String, String>, String> internalSheetUpsertDataRequestResponseMap;
  Map<Map<String, String>, String> internalSheetDeleteDataRequestResponseMap;
  Map<String, String> internalSheetData;
  RestrictedKeywordRequestData restrictedKeywordRequestData;
  BulkInternalProcessDataGenerationDTO bulkInternalProcessDataGenerationDTO;

  RequestHelper requestHelper = new RequestHelper();
  BulkApprovalRejectionRequestData bulkVendorActionsModel = new BulkApprovalRejectionRequestData();

  Set<String> cncPickupPointCodes = new HashSet<>();
  @Mock
  private FileStorageService fileStorageService;

  @BeforeEach
  public void initialize() {
    MockitoAnnotations.initMocks(this);
    internalSheetData = new HashMap<>();
    internalSheetData.put(ExcelHeaderNames.APPLICABLE_FOR_ALL, KEYWORD);
    requestResponseMap = new HashMap<>();
    requestResponseMap.put(RestrictedKeywordRequestData.builder().build(), ProductUpdateErrorMessages.KEYWORD_INVALID);
    requestResponseMap.put(RestrictedKeywordRequestData.builder().keyword(KEYWORD).build(),
        ProductUpdateErrorMessages.KEYWORD_TYPE_INVALID);
    requestResponseMap.put(RestrictedKeywordRequestData.builder().keyword(KEYWORD).keywordType(KEYWORD).build(),
        ProductUpdateErrorMessages.KEYWORD_TYPE_INVALID);
    requestResponseMap.put(RestrictedKeywordRequestData.builder().keyword(KEYWORD).keywordType(OTHERS).build(),
        ProductUpdateErrorMessages.KEYWORD_ACTION_INVALID);
    requestResponseMap.put(
        RestrictedKeywordRequestData.builder().keyword(KEYWORD).keywordType(OTHERS).keywordAction(KEYWORD).build(),
        ProductUpdateErrorMessages.KEYWORD_ACTION_INVALID);
    requestResponseMap.put(
        RestrictedKeywordRequestData.builder().keyword(KEYWORD).keywordType(OTHERS).keywordAction(STRAIGHT_REJECTION)
            .build(), ProductUpdateErrorMessages.REASON_INVALID);
    requestResponseMap.put(
        RestrictedKeywordRequestData.builder().keyword(KEYWORD).keywordType(OTHERS).keywordAction(STRAIGHT_REJECTION)
            .message(KEYWORD).build(), ProductUpdateErrorMessages.APPLICABLE_FOR_ALL_INVALID);
    requestResponseMap.put(
        RestrictedKeywordRequestData.builder().keyword(KEYWORD).keywordType(OTHERS).keywordAction(CATEGORY_CHANGE)
            .build(), ProductUpdateErrorMessages.DESTINATION_CATEGORY_EMPTY);
    requestResponseMap.put(
        RestrictedKeywordRequestData.builder().keyword(KEYWORD).keywordType(OTHERS).keywordAction(NO_ACTION).build(),
        ProductUpdateErrorMessages.APPLICABLE_FOR_ALL_INVALID);
    requestResponseMap.put(
        RestrictedKeywordRequestData.builder().keyword(KEYWORD).keywordType(OTHERS).keywordAction(CATEGORY_CHANGE)
            .destinationCategory(KEYWORD).build(), ProductUpdateErrorMessages.APPLICABLE_FOR_ALL_INVALID);
    restrictedKeywordRequestData =
        RestrictedKeywordRequestData.builder().keyword(KEYWORD).keywordType(OTHERS).keywordAction(CATEGORY_CHANGE)
            .destinationCategory(KEYWORD).build();
    internalSheetUpsertDataRequestResponseMap = new HashMap<>();
    internalSheetUpsertDataRequestResponseMap.put(new HashMap<>(internalSheetData),
        ProductUpdateErrorMessages.APPLICABLE_FOR_ALL_INVALID);
    internalSheetData.put(ExcelHeaderNames.APPLICABLE_FOR_ALL, ExcelHeaderNames.NO);
    internalSheetUpsertDataRequestResponseMap.put(new HashMap<>(internalSheetData),
        ProductUpdateErrorMessages.APPLICABLE_FOR_THESE_INVALID);
    internalSheetData.put(ExcelHeaderNames.APPLICABLE_FOR_ALL, ExcelHeaderNames.NO);
    internalSheetData.put(ExcelHeaderNames.APPLICABLE_FOR_THESE, KEYWORD);
    internalSheetUpsertDataRequestResponseMap.put(new HashMap<>(internalSheetData), null);
    internalSheetData.put(ExcelHeaderNames.APPLICABLE_FOR_ALL, ExcelHeaderNames.YES);
    internalSheetUpsertDataRequestResponseMap.put(new HashMap<>(internalSheetData), null);

    internalSheetDeleteDataRequestResponseMap = new HashMap<>();
    internalSheetData.put(ExcelHeaderNames.DELETE_KEYWORD_FOR_ALL_CATEGORY, KEYWORD);
    internalSheetDeleteDataRequestResponseMap.put(new HashMap<>(internalSheetData),
        ProductUpdateErrorMessages.DELETE_FOR_ALL_INVALID);
    internalSheetData.put(ExcelHeaderNames.DELETE_KEYWORD_FOR_ALL_CATEGORY, StringUtils.EMPTY);
    internalSheetDeleteDataRequestResponseMap.put(new HashMap<>(internalSheetData),
        ProductUpdateErrorMessages.DELETE_FOR_ALL_INVALID);
    internalSheetData.put(ExcelHeaderNames.DELETE_KEYWORD_FOR_ALL_CATEGORY, ExcelHeaderNames.NO);
    internalSheetDeleteDataRequestResponseMap.put(new HashMap<>(internalSheetData),
        ProductUpdateErrorMessages.DELETE_FOR_THESE_INVALID);
    internalSheetData.put(ExcelHeaderNames.DELETE_KEYWORD_FOR_ALL_CATEGORY, ExcelHeaderNames.NO);
    internalSheetData.put(ExcelHeaderNames.DELETE_KEYWORD_ACROSS_THESE_CATEGORY, KEYWORD);
    internalSheetDeleteDataRequestResponseMap.put(new HashMap<>(internalSheetData), null);
    internalSheetData.put(ExcelHeaderNames.DELETE_KEYWORD_FOR_ALL_CATEGORY, ExcelHeaderNames.YES);
    internalSheetDeleteDataRequestResponseMap.put(new HashMap<>(internalSheetData), null);
    RequestHelper.setFileStorageService(fileStorageService);

    bulkVendorActionsModel.setProductCode(DEFAULT_BULK_PROCESS_CODE);
    bulkVendorActionsModel.setComment(KEYWORD);
    bulkInternalProcessDataGenerationDTO = new BulkInternalProcessDataGenerationDTO();
    bulkInternalProcessDataGenerationDTO.setUserName(Constant.USER_NAME);
    bulkInternalProcessDataGenerationDTO.setStoreId(Constant.STORE_ID);
    bulkInternalProcessDataGenerationDTO.setBrandAuthEndYear(1);
    bulkInternalProcessDataGenerationDTO.setBulkPriceUpdateMaxRows(2);
  }

  @Test
  public void isExcelHeaderValidTest_TEMPLATE_HEADER_sizeEqualForSTORE_COPY() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<String, String>();
    for (int i = 0; i < 15; i++) {
      map.put(String.valueOf(i), StoreCopyConstants.BLIBLI_PRODUCT_SKU);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(STORE_COPY);
    RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidTestVendorBulkAssignment() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (int i = 0; i < 15; i++) {
      map.put(String.valueOf(i), StoreCopyConstants.BLIBLI_PRODUCT_SKU);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
    RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(!RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidRestrictedKeywordUpsertSizeTrueTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (int i = 0; i < 9; i++) {
      map.put(String.valueOf(i), StoreCopyConstants.BLIBLI_PRODUCT_SKU);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name());
    RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidForBulkApproval() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (int i = 0; i < 3; i++) {
      map.put(String.valueOf(i), StoreCopyConstants.BLIBLI_PRODUCT_SKU);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_APPROVAL.name());
    RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidForBulkPriceUpdate() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (int i = 0; i < 15; i++) {
      map.put(String.valueOf(i), StoreCopyConstants.BLIBLI_PRODUCT_SKU);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_UPDATE.name());
    RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidForBulkRejection() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (int i = 0; i < 4; i++) {
      map.put(String.valueOf(i), StoreCopyConstants.BLIBLI_PRODUCT_SKU);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_REJECTION.name());
    RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidRestrictedKeywordUpsertSizeTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (int i = 0; i < 7; i++) {
      map.put(String.valueOf(i), StoreCopyConstants.BLIBLI_PRODUCT_SKU);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name());
    RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidRestrictedKeywordDeleteSizeTrueTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (int i = 0; i < 5; i++) {
      map.put(String.valueOf(i), StoreCopyConstants.BLIBLI_PRODUCT_SKU);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name());
    RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidRestrictedKeywordDeleteFalseTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (int i = 0; i < 3; i++) {
      map.put(String.valueOf(i), StoreCopyConstants.BLIBLI_PRODUCT_SKU);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name());
    RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidBrandAuthSizeTrueTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (int i = 0; i < 9; i++) {
      map.put(String.valueOf(i), StoreCopyConstants.BLIBLI_PRODUCT_SKU);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_ADD.name());
    RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidBrandAuthAddSizeTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (int i = 0; i < 7; i++) {
      map.put(String.valueOf(i), StoreCopyConstants.BLIBLI_PRODUCT_SKU);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_ADD.name());
    RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidBrandAuthDeleteSizeTrueTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (int i = 0; i < 5; i++) {
      map.put(String.valueOf(i), StoreCopyConstants.BLIBLI_PRODUCT_SKU);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_DELETE.name());
    RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidAssigneeMasterSkuReviewSizeTrueTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (int i = 0; i < 6; i++) {
      map.put(String.valueOf(i), StoreCopyConstants.BLIBLI_PRODUCT_SKU);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name());
    RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidAssigneeMasterSkuReviewSizeFalseTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (int i = 0; i < 3; i++) {
      map.put(String.valueOf(i), StoreCopyConstants.BLIBLI_PRODUCT_SKU);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name());
    RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidBulkMasterSkuReviewTrueTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (int i = 0; i < 6; i++) {
      map.put(String.valueOf(i), StoreCopyConstants.BLIBLI_PRODUCT_SKU);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name());
    RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidBulkMasterSkuReviewSizeFalseTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (int i = 0; i < 3; i++) {
      map.put(String.valueOf(i), StoreCopyConstants.BLIBLI_PRODUCT_SKU);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name());
    RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidBulkAssignAutoApprovedProducts_validHeaders() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    int ind = 0;
    for (String header :
        BulkAssignAutoApprovedProductsParameters.BULK_ASSIGN_AUTO_APPROVED_PRODUCTS_HEADERS) {
      map.put(header, Integer.toString(ind++));
    }
    map.put("rowNumber", Integer.toString(ind++));
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());
    Assertions.assertFalse(
        RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidBulkAssignAutoApprovedProducts_validOptionalHeaders() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    int ind = 0;
    for (String header :
 BulkAssignAutoApprovedProductsParameters.BULK_ASSIGN_AUTO_APPROVED_PRODUCTS_OPTIONAL_HEADERS) {
      map.put(header,Integer.toString(ind++));
    }
    map.put("rowNumber", Integer.toString(ind));
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());
    Assertions.assertFalse(
        RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidBulkAssignAutoApprovedProducts_invalidHeaders() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    int ind = 0;
    for (String header :
        BulkAssignAutoApprovedProductsParameters.BULK_ASSIGN_AUTO_APPROVED_PRODUCTS_OPTIONAL_HEADERS) {
      map.put(header + " invalid",Integer.toString(ind++));
    }
    map.put("rowNumber", Integer.toString(ind));
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());
    Assertions.assertTrue(
        RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidBulkAssignAutoApprovedProducts_validHeaderSize() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    int ind = 0;
    for (String header :
        BulkAssignAutoApprovedProductsParameters.BULK_ASSIGN_AUTO_APPROVED_PRODUCTS_HEADERS) {
      map.put(header, Integer.toString(ind++));
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());
    Assertions.assertTrue(
        RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }


  @Test
  public void isExcelHeaderValidBrandAuthDeleteFalseTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (int i = 0; i < 3; i++) {
      map.put(String.valueOf(i), StoreCopyConstants.BLIBLI_PRODUCT_SKU);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_DELETE.name());
    RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidSuspendFailTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    // Add incorrect headers that don't match with required SUSPENSION_HEADERS
    map.put("0", INCORRECT_HEADER_1);
    map.put("1", INCORRECT_HEADER_2);
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.SUSPEND.name());
    boolean result = RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(result);
  }

  @Test
  public void isExcelHeaderValidSuspendTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    // Add all required headers for SUSPENSION
    map.put("0", BulkProductSuspensionParameters.SELLER_CODE);
    map.put("1", BulkProductSuspensionParameters.SELLER_NAME);
    map.put("2", BulkProductSuspensionParameters.PRODUCT_CODE);
    map.put("3", BulkProductSuspensionParameters.PRODUCT_NAME);
    map.put("4", BulkProductSuspensionParameters.REASON);
    map.put("5", BulkProductSuspensionParameters.SELLER_REASON_DESCRIPTION);
    map.put("6", BulkProductSuspensionParameters.FAILURE_REASON);
    map.put("7", "RowNumber"); // The +1 column that's expected
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.SUSPEND.name());
    boolean result = RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    // Test should pass with all required headers
    Assertions.assertTrue(result);
  }


  @Test
  public void isExcelHeaderValidDeleteBulkAuthHeaderSizeFailTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (int i = 0; i < 5; i++) {
      map.put(String.valueOf(i), BrandAuthorisationConstant.BRAND_CODE);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name());
    Assertions.assertTrue(RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidTestDeleteBulkAuthHeaderFieldsFailTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (int i = 0; i < 4; i++) {
      map.put(String.valueOf(i), BrandAuthorisationConstant.BRAND_CODE);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.DELETE_BRAND_AUTHORISATION.name());
    Assertions.assertTrue(RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidTest_TEMPLATE_HEADER_sizeEqualForDUMMY_PROCESS_TYPE() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<String, String>();
    for (int i = 0; i < 15; i++) {
      map.put(String.valueOf(i), StoreCopyConstants.BLIBLI_PRODUCT_SKU);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(DUMMY_PROCESS_TYPE);
    RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidTest_TEMPLATE_HEADER_sizeEqualForSALES_CATEGORY_UPDATE() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<String, String>();
    for (int i = 0; i < 15; i++) {
      map.put(String.valueOf(i), StoreCopyConstants.BLIBLI_PRODUCT_SKU);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(SALES_CATEGORY_UPDATE);
    RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeTest() throws Exception {
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(StoreCopyConstants.BLIBLI_PRODUCT_SKU);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeRestrictedKeywordUpsertTest() throws Exception {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (String string : ExcelHeaderNames.RESTRICTED_KEYWORD_UPSERT_TEMPLATE_HEADER) {
      map.put("", string);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name());
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(), bulkInternalProcess.getStatus());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeRestrictedKeywordDeleteTest() throws Exception {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (String string : ExcelHeaderNames.RESTRICTED_KEYWORD_DELETE_TEMPLATE_HEADER) {
      map.put("", string);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.RESTRICTED_KEYWORD_DELETE.name());
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(), bulkInternalProcess.getStatus());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeBrandAuthAddTest() throws Exception {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (String string : ExcelHeaderNames.BRAND_AUTH_ADD_TEMPLATE_HEADER) {
      map.put(string, "11-11-11");
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_ADD.name());
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(), bulkInternalProcess.getStatus());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(ProductUpdateErrorMessages.INVALID_DATE_FORMAT,
        bulkInternalProcessDataList.get(0).getErrorMessage());
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeBrandAuthAddTest_duplicateEntry() throws Exception {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (String string : ExcelHeaderNames.BRAND_AUTH_ADD_TEMPLATE_HEADER) {
      map.put(string, "45120");
    }
    map.put(ExcelHeaderNames.BRAND_AUTH_SELLER_CODE, SELLER_CODE);
    map.put(ExcelHeaderNames.BRAND_AUTH_BRAND_CODE, BRAND_CODE);
    internalProcessDataFromExcel.add(map);
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_ADD.name());
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(2, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(), bulkInternalProcess.getStatus());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), bulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(1).getStatus());
    Assertions.assertEquals(ProductUpdateErrorMessages.DUPLICATE_BRAND_CODE_AND_SELLER_CODE,
        bulkInternalProcessDataList.get(1).getErrorMessage());
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeBrandAuthAddStartDateEmptyTest() throws Exception {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (String string : ExcelHeaderNames.BRAND_AUTH_ADD_TEMPLATE_HEADER) {
      map.put(string, "");
    }
    map.put(ExcelHeaderNames.AUTH_START_DATE, "45120");
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_ADD.name());
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setBrandAuthEndYear(5);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(), bulkInternalProcess.getStatus());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeBrandAuthAddDateNonEmptyTest() throws Exception {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (String string : ExcelHeaderNames.BRAND_AUTH_ADD_TEMPLATE_HEADER) {
      map.put(string, "");
    }
    map.put(ExcelHeaderNames.AUTH_START_DATE, "45120");
    map.put(ExcelHeaderNames.AUTH_END_DATE, "45120");
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_ADD.name());
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setBrandAuthEndYear(5);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(), bulkInternalProcess.getStatus());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
  }
  @Test
  public void generateBulkInternalProcessDataByProcessTypeBrandAuthAddBrandCodeEmptyTest() throws Exception {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (String string : ExcelHeaderNames.BRAND_AUTH_ADD_TEMPLATE_HEADER) {
      map.put(string, "");
    }
    map.put(ExcelHeaderNames.BRAND_AUTH_SELLER_CODE, MERCHANT_CODE);
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_ADD.name());
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(), bulkInternalProcess.getStatus());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeBulkPriceUpdateTest() throws Exception {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (String string : ExcelHeaderNames.BRAND_AUTH_ADD_TEMPLATE_HEADER) {
      map.put(string, "");
    }
    map.put(ExcelHeaderNames.BRAND_AUTH_SELLER_CODE, MERCHANT_CODE);
    map.put(BulkParameters.ROW_NUMBER, "1");
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_UPDATE.name());
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(), bulkInternalProcess.getStatus());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
  }
  @Test
  public void generateBulkInternalProcessDataByProcessTypeBrandAuthAddBrandCodeNotEmptyTest() throws Exception {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (String string : ExcelHeaderNames.BRAND_AUTH_ADD_TEMPLATE_HEADER) {
      map.put(string, "");
    }
    map.put(ExcelHeaderNames.BRAND_AUTH_SELLER_CODE, MERCHANT_CODE);
    map.put(ExcelHeaderNames.BRAND_AUTH_BRAND_CODE, MERCHANT_CODE);
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_ADD.name());
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(), bulkInternalProcess.getStatus());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeBulkApprovalCodeNotEmptyTest() throws Exception {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (String string : BulkApprovalRejectionParameters.BULK_APPROVAL) {
      map.put(string, "");
    }
    map.put(BulkApprovalRejectionParameters.PRODUCT_CODE, MERCHANT_CODE);
    map.put(BulkApprovalRejectionParameters.APPROVAL_REASON, MERCHANT_CODE);
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_APPROVAL.name());
    bulkInternalProcess.setCreatedBy(SELLER_CODE);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
            RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), bulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(SELLER_CODE,bulkInternalProcessDataList.get(0).getSellerCode());
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeBulkApprovalFailed() throws Exception {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (String string : BulkApprovalRejectionParameters.BULK_APPROVAL) {
      map.put(string, "");
    }
    map.put(BulkApprovalRejectionParameters.PRODUCT_CODE, StringUtils.EMPTY);
    map.put(BulkApprovalRejectionParameters.APPROVAL_REASON, MERCHANT_CODE);
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_APPROVAL.name());
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
            RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeBulkApprovalForRepeatedProducts() throws Exception {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    Map<String, String> map1 = new HashMap<>();
    Map<String, String> map2 = new HashMap<>();
    for (String string : BulkApprovalRejectionParameters.BULK_APPROVAL) {
      map.put(string, "");
    }
    for (String string : BulkApprovalRejectionParameters.BULK_APPROVAL) {
      map1.put(string, "");
    }
    for (String string : BulkApprovalRejectionParameters.BULK_APPROVAL) {
      map2.put(string, "");
    }
    map.put(BulkApprovalRejectionParameters.PRODUCT_CODE, "DAMAGE");
    map.put(BulkApprovalRejectionParameters.APPROVAL_REASON, MERCHANT_CODE);
    map1.put(BulkApprovalRejectionParameters.PRODUCT_CODE, "DAMAGE");
    map2.put(BulkApprovalRejectionParameters.PRODUCT_CODE, "DAMAGE1");
    internalProcessDataFromExcel.add(map);
    internalProcessDataFromExcel.add(map1);
    internalProcessDataFromExcel.add(map2);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_APPROVAL.name());
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(2, bulkInternalProcessDataList.size());
  }

  @Test
  public void generateBulkInternalProcessDataForBulkAssigneeForRepeatedAnchorMapping() throws Exception {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    Map<String, String> map1 = new HashMap<>();
    Map<String, String> map2 = new HashMap<>();
    Map<String, String> map3 = new HashMap<>();
    for (String string : BulkMasterSkuUploadParameters.MASTER_SKU_BULK_ASSIGNEE) {
      map.put(string, "");
    }
    for (String string : BulkMasterSkuUploadParameters.MASTER_SKU_BULK_ASSIGNEE) {
      map1.put(string, "");
    }
    for (String string : BulkMasterSkuUploadParameters.MASTER_SKU_BULK_ASSIGNEE) {
      map2.put(string, "");
    }
    for (String string : BulkMasterSkuUploadParameters.MASTER_SKU_BULK_ASSIGNEE) {
      map3.put(string, "");
    }
    map.put(BulkMasterSkuUploadParameters.FIRST_MASTER_SKU, "FIRST");
    map.put(BulkMasterSkuUploadParameters.SECOND_MASTER_SKU, "SECOND");
    map1.put(BulkMasterSkuUploadParameters.FIRST_MASTER_SKU, "DAMAGE");
    map2.put(BulkMasterSkuUploadParameters.SECOND_MASTER_SKU, "DAMAGE1");
    map3.put(BulkMasterSkuUploadParameters.FIRST_MASTER_SKU, "FIRST");
    map3.put(BulkMasterSkuUploadParameters.SECOND_MASTER_SKU, "SECOND");
    internalProcessDataFromExcel.add(map);
    internalProcessDataFromExcel.add(map1);
    internalProcessDataFromExcel.add(map2);
    internalProcessDataFromExcel.add(map3);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.MASTER_SKU_BULK_ASSIGNEE.name());
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
      RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(4, bulkInternalProcessDataList.size());
  }

  @Test
  public void getBulkInternalProcessDataForBulkMasterSkuReviewTest() throws Exception {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    //Empty action
    internalProcessDataFromExcel.add(getRowForMasterSkuReview(
      BulkMasterSkuReviewRequestData.builder().build()));
    //Invalid action type
    internalProcessDataFromExcel.add(
        getRowForMasterSkuReview(BulkMasterSkuReviewRequestData.builder().action(NO_ACTION).build()));
    //First master sku Empty
    internalProcessDataFromExcel.add(getRowForMasterSkuReview(
        BulkMasterSkuReviewRequestData.builder().action(BulkMasterSkuUploadParameters.DISMANTLE_CLUSTER).build()));
    //Second master sku Empty
    internalProcessDataFromExcel.add(getRowForMasterSkuReview(
        BulkMasterSkuReviewRequestData.builder().action(BulkMasterSkuUploadParameters.ADD_TO_CLUSTER)
            .firstMasterSku(ITEM_SKU).build()));
    //Valid case
    internalProcessDataFromExcel.add(getRowForMasterSkuReview(
        BulkMasterSkuReviewRequestData.builder().action(BulkMasterSkuUploadParameters.DISMANTLE_CLUSTER)
            .firstMasterSku(ITEM_SKU).build()));
    //Duplicate entry
    internalProcessDataFromExcel.add(getRowForMasterSkuReview(
        BulkMasterSkuReviewRequestData.builder().action(BulkMasterSkuUploadParameters.DISMANTLE_CLUSTER)
            .firstMasterSku(ITEM_SKU).build()));
    internalProcessDataFromExcel.add(getRowForMasterSkuReview(
        BulkMasterSkuReviewRequestData.builder().action(BulkMasterSkuUploadParameters.ADD_TO_CLUSTER)
            .firstMasterSku(ITEM_SKU).secondMasterSku(ITEM_SKU).build()));
    internalProcessDataFromExcel.add(getRowForMasterSkuReview(
        BulkMasterSkuReviewRequestData.builder().action(BulkMasterSkuUploadParameters.ADD_TO_CLUSTER)
            .firstMasterSku(MASTER_SKU).secondMasterSku(ITEM_SKU).build()));
    internalProcessDataFromExcel.add(getRowForMasterSkuReview(
        BulkMasterSkuReviewRequestData.builder().action(BulkMasterSkuUploadParameters.DISMANTLE_CLUSTER)
            .firstMasterSku(MASTER_SKU).secondMasterSku(MASTER_SKU).build()));
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name());
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(9, bulkInternalProcessDataList.size());
  }

  private Map<String, String> getRowForMasterSkuReview(
    BulkMasterSkuReviewRequestData bulKMasterSkuReviewRequestData) {
    Map<String, String> row = new HashMap<>();
    row.put(BulkMasterSkuUploadParameters.ANCHOR_SKU, bulKMasterSkuReviewRequestData.getFirstMasterSku());
    row.put(BulkMasterSkuUploadParameters.ANCHOR_SKU_NAME, bulKMasterSkuReviewRequestData.getFirstMasterSkuName());
    row.put(BulkMasterSkuUploadParameters.SECOND_MASTER_SKU, bulKMasterSkuReviewRequestData.getSecondMasterSku());
    row.put(BulkMasterSkuUploadParameters.SECOND_MASTER_SKU_NAME,
        bulKMasterSkuReviewRequestData.getSecondMasterSkuName());
    row.put(BulkMasterSkuUploadParameters.REVIEW_ACTION, bulKMasterSkuReviewRequestData.getAction());
    return row;
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkAssignAutoApprovedProducts()
      throws Exception {
    BulkAssignAutoApprovedProductsRequestData bulkAssignAutoApprovedProductsRequestData =
        BulkAssignAutoApprovedProductsRequestData.builder().productCode(PRODUCT_CODE)
            .productName(PRODUCT_NAME).storeName(STORE_NAME).categoryName(CATEGORY_NAME)
            .assignee(ASSIGNEE).excelRowNumber(1).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.PRODUCT_CODE,
        bulkAssignAutoApprovedProductsRequestData.getProductCode());
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.PRODUCT_NAME,
        bulkAssignAutoApprovedProductsRequestData.getProductName());
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.CATEGORY_NAME,
        bulkAssignAutoApprovedProductsRequestData.getCategoryName());
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.STORE_NAME,
        bulkAssignAutoApprovedProductsRequestData.getStoreName());
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.ASSIGNEE,
        bulkAssignAutoApprovedProductsRequestData.getAssignee());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkAssignAutoApprovedProductsParameters.REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(bulkAssignAutoApprovedProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
        BulkAssignAutoApprovedProductsRequestData.class));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkAssignAutoApprovedProducts_emptyAssignee()
      throws Exception {
    BulkAssignAutoApprovedProductsRequestData bulkAssignAutoApprovedProductsRequestData =
        BulkAssignAutoApprovedProductsRequestData.builder().productCode(PRODUCT_CODE)
            .productName(PRODUCT_NAME).storeName(STORE_NAME).categoryName(CATEGORY_NAME)
            .assignee(StringUtils.EMPTY).excelRowNumber(1).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.PRODUCT_CODE,
        bulkAssignAutoApprovedProductsRequestData.getProductCode());
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.PRODUCT_NAME,
        bulkAssignAutoApprovedProductsRequestData.getProductName());
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.CATEGORY_NAME,
        bulkAssignAutoApprovedProductsRequestData.getCategoryName());
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.STORE_NAME,
        bulkAssignAutoApprovedProductsRequestData.getStoreName());
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.ASSIGNEE,
        bulkAssignAutoApprovedProductsRequestData.getAssignee());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkAssignAutoApprovedProductsParameters.REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(bulkAssignAutoApprovedProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
            BulkAssignAutoApprovedProductsRequestData.class));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkAssignAutoApprovedProductsOptionalHeaders()
      throws Exception {
    BulkAssignAutoApprovedProductsRequestData bulkAssignAutoApprovedProductsRequestData =
        BulkAssignAutoApprovedProductsRequestData.builder().productCode(PRODUCT_CODE)
            .productName(PRODUCT_NAME).storeName(STORE_NAME).categoryName(CATEGORY_NAME)
            .assignee(ASSIGNEE).excelRowNumber(1).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.PRODUCT_CODE,
        bulkAssignAutoApprovedProductsRequestData.getProductCode());
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.PRODUCT_NAME_OPTIONAL,
        bulkAssignAutoApprovedProductsRequestData.getProductName());
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.CATEGORY_NAME_OPTIONAL,
        bulkAssignAutoApprovedProductsRequestData.getCategoryName());
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.STORE_NAME_OPTIONAL,
        bulkAssignAutoApprovedProductsRequestData.getStoreName());
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.ASSIGNEE,
        bulkAssignAutoApprovedProductsRequestData.getAssignee());
    internalProcessDataFromExcel.add(keyValuePairMap);
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkAssignAutoApprovedProductsParameters.REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(bulkAssignAutoApprovedProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
            BulkAssignAutoApprovedProductsRequestData.class));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkAssignAutoApprovedProducts_emptyProductCode()
      throws Exception {
    BulkAssignAutoApprovedProductsRequestData bulkAssignAutoApprovedProductsRequestData =
        BulkAssignAutoApprovedProductsRequestData.builder().productCode(StringUtils.EMPTY).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.PRODUCT_CODE,
        bulkAssignAutoApprovedProductsRequestData.getProductCode());
    internalProcessDataFromExcel.add(keyValuePairMap);
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkAssignAutoApprovedProductsParameters.REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(),actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.PRODUCT_CODE_EMPTY,
        actualBulkInternalProcessDataList.get(0).getErrorMessage());
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkAssignAutoApprovedProducts_duplicateProductCode()
      throws Exception {
    BulkAssignAutoApprovedProductsRequestData bulkAssignAutoApprovedProductsRequestData =
        BulkAssignAutoApprovedProductsRequestData.builder().productCode(PRODUCT_CODE)
            .assignee(ASSIGNEE).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.PRODUCT_CODE,
        bulkAssignAutoApprovedProductsRequestData.getProductCode());
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.ASSIGNEE,
        bulkAssignAutoApprovedProductsRequestData.getAssignee());
    internalProcessDataFromExcel.add(new HashMap<>(keyValuePairMap));
    keyValuePairMap.remove(BulkAssignAutoApprovedProductsParameters.ASSIGNEE);
    internalProcessDataFromExcel.add(new HashMap<>(keyValuePairMap));
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkAssignAutoApprovedProductsParameters.REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(2, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.PENDING.name(),actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(ProcessStatus.FAILED.name(),actualBulkInternalProcessDataList.get(1).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.DUPLICATE_PRODUCT_CODE,
        actualBulkInternalProcessDataList.get(1).getErrorMessage());
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkAssignAutoApprovedProducts_invalidReviewer()
      throws Exception {
    BulkAssignAutoApprovedProductsRequestData bulkAssignAutoApprovedProductsRequestData =
        BulkAssignAutoApprovedProductsRequestData.builder().productCode(PRODUCT_CODE)
            .productName(PRODUCT_NAME).storeName(STORE_NAME).categoryName(CATEGORY_NAME)
            .assignee(ASSIGNEE).excelRowNumber(1).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.PRODUCT_CODE,
        bulkAssignAutoApprovedProductsRequestData.getProductCode());
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.PRODUCT_NAME,
        bulkAssignAutoApprovedProductsRequestData.getProductName());
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.CATEGORY_NAME,
        bulkAssignAutoApprovedProductsRequestData.getCategoryName());
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.STORE_NAME,
        bulkAssignAutoApprovedProductsRequestData.getStoreName());
    keyValuePairMap.put(BulkAssignAutoApprovedProductsParameters.ASSIGNEE,
        bulkAssignAutoApprovedProductsRequestData.getAssignee());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkAssignAutoApprovedProductsParameters.REVIEWERS, Arrays.asList(ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.INVALID_REVIEWER,
        actualBulkInternalProcessDataList.get(0).getErrorMessage());
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeBulkRejectionCodeNotEmptyTest() throws Exception {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (String string : BulkApprovalRejectionParameters.BULK_REJECTION) {
      map.put(string, "");
    }
    map.put(BulkApprovalRejectionParameters.PRODUCT_CODE,MERCHANT_CODE);
    map.put(BulkApprovalRejectionParameters.REJECTION_REASON, MERCHANT_CODE);
    map.put(BulkApprovalRejectionParameters.COMMENTS_TO_SELLERS,MERCHANT_CODE);
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_REJECTION.name());
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
            RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeBulkRejectionFailed() throws Exception {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (String string : BulkApprovalRejectionParameters.BULK_REJECTION) {
      map.put(string, "");
    }
    map.put(BulkApprovalRejectionParameters.PRODUCT_CODE,MERCHANT_CODE);
    map.put(BulkApprovalRejectionParameters.REJECTION_REASON,StringUtils.EMPTY);
    map.put(BulkApprovalRejectionParameters.COMMENTS_TO_SELLERS,MERCHANT_CODE);
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_REJECTION.name());
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
            RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
  }
  @Test
  public void generateBulkInternalProcessDataByProcessTypeBulkRejectionFailedCommentEmpty() throws Exception {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (String string : BulkApprovalRejectionParameters.BULK_REJECTION) {
      map.put(string, "");
    }
    map.put(BulkApprovalRejectionParameters.PRODUCT_CODE, MERCHANT_CODE);
    map.put(BulkApprovalRejectionParameters.REJECTION_REASON, MERCHANT_CODE);
    map.put(BulkApprovalRejectionParameters.COMMENTS_TO_SELLERS, StringUtils.EMPTY);
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_REJECTION.name());
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
            RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
  }
  @Test
  public void generateBulkInternalProcessDataByProcessTypeBrandAuthDeleteTest() throws Exception {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (String string : ExcelHeaderNames.BRAND_AUTH_DELETE_TEMPLATE_HEADER) {
      map.put(string, "");
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_DELETE.name());
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(), bulkInternalProcess.getStatus());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
  }
  @Test
  public void generateBulkInternalProcessDataByProcessTypeBrandAuthDeleteBrandCodeEmptyTest() throws Exception {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (String string : ExcelHeaderNames.BRAND_AUTH_DELETE_TEMPLATE_HEADER) {
      map.put(string, "");
    }
    map.put(ExcelHeaderNames.BRAND_AUTH_SELLER_CODE, MERCHANT_CODE);
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_DELETE.name());
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(), bulkInternalProcess.getStatus());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeBrandAuthDeleteBrandCodeNotEmptyTest() throws Exception {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (String string : ExcelHeaderNames.BRAND_AUTH_DELETE_TEMPLATE_HEADER) {
      map.put(string, MERCHANT_CODE);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_DELETE.name());
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(), bulkInternalProcess.getStatus());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void validateRowAndFormRequestForVendorBulkAssignmentTest() throws JsonProcessingException {
    Map<String, String> row = new HashMap<>();
    row.put(MasterDataBulkParameters.PRODUCT_CODE, StringUtils.EMPTY);
    row.put(VendorProductDataBulkParameters.ROW_NUMBER, String.valueOf(1));
    RequestHelper.validateRowAndFormRequestForVendorBulkAssignment(row, new HashMap<>(), new BulkInternalProcess(),
        Constant.USER_NAME);
  }

  @Test
  public void validateRowAndFormRequestForVendorBulkAssignmentValidTest() throws JsonProcessingException {
    Map<String, String> row = new HashMap<>();
    row.put(MasterDataBulkParameters.PRODUCT_CODE, MasterDataBulkParameters.PRODUCT_CODE);
    row.put(VendorProductDataBulkParameters.ROW_NUMBER, String.valueOf(1));
    RequestHelper.validateRowAndFormRequestForVendorBulkAssignment(row, new HashMap<>(), new BulkInternalProcess(),
        Constant.USER_NAME);
  }

  @Test
  public void validateHeaderByProcessTypeTest() {
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(StoreCopyConstants.BLIBLI_PRODUCT_SKU);
    RequestHelper.validateHeaderByProcessType(new ArrayList<>(), bulkInternalProcess);
  }

  @Test
  public void validateHeaderByProcessTypeStoreCopyTest() {
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.STORE_COPY.name());
    Map<String, String> data = new TreeMap<>();
    data.put(BulkInternalProcessType.STORE_COPY.name(), BulkInternalProcessType.STORE_COPY.name());
    ArrayList hashMapList = new ArrayList<>();
    hashMapList.add(data);
    RequestHelper.validateHeaderByProcessType(hashMapList, bulkInternalProcess);
  }

  @Test
  public void validateHeaderByProcessTypeBulkRebateTest() {
    Map<String, String> data = new TreeMap<>();
    data.put(BulkInternalProcessType.BULK_PRICE_REBATE.name(), BulkInternalProcessType.BULK_PRICE_REBATE.name());
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_REBATE.name());
    RequestHelper.validateHeaderByProcessType(Collections.singletonList(data), bulkInternalProcess);
  }

  @Test
  public void validateHeaderByProcessTypeBulkRebateTest2() {
    Map<String, String> data = new TreeMap<>();
    data.put(BulkParameters.BRAND, "Brand");
    data.put(BulkParameters.MAIN_CATEGORY_CODE, "mainCategoryCode");
    data.put(BulkParameters.CATEGORY_CODE, "categoryCode");
    data.put(BulkParameters.PROJECTED_REBATE, "200");
    data.put(BulkParameters.STORE_ID, "storeId");
    data.put(BulkParameters.MONTH, "month");
    data.put(BulkParameters.YEAR, "year");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_REBATE.name());
    RequestHelper.validateHeaderByProcessType(Collections.singletonList(data), bulkInternalProcess);
  }

  @Test
  public void inventoryDetailInfoRequestDTOTest() {
    itemPickupPointListingResponse.setPickUpPointCode(PICKUP_POINT_CODE);
    itemPickupPointListingResponse.setItemSku(ITEM_SKU);
    itemPickupPointListingResponse.setMerchantCode(MERCHANT_CODE);
    InventoryDetailInfoRequestDTO inventoryDetailInfoRequestDTO =
      RequestHelper.inventoryDetailInfoRequestDTO(itemPickupPointListingResponse);
    Assertions.assertEquals(ITEM_SKU, inventoryDetailInfoRequestDTO.getWebItemSku());
    Assertions.assertEquals(PICKUP_POINT_CODE, inventoryDetailInfoRequestDTO.getPickupPointCode());
    Assertions.assertEquals(MERCHANT_CODE, inventoryDetailInfoRequestDTO.getWebMerchantCode());
  }

  @Test
  public void toL5IdTest() {
    String itemPickupPointKey = RequestHelper.toL5Id(ITEM_SKU, PICKUP_POINT_CODE);
    Assertions.assertEquals(L5_CODE, itemPickupPointKey);
  }

  @Test
  public void getFbbBulkInternalProcessTest() {
    uploadRequest.setInternalProcessRequestCode("RANDOM");
    uploadRequest.setSellerCode(MERCHANT_CODE);
    uploadRequest.setProcessType(FBB_L5_CREATE.name());
    uploadRequest.setNotes("CONSIGNMENT_ID");
    BulkInternalProcess bulkInternalProcess =
      RequestHelper.getFbbBulkInternalProcess(STORE_ID, uploadRequest);
    Assertions.assertEquals(bulkInternalProcess.getSellerCode(), MERCHANT_CODE);
  }

  @Test
  public void getFbbL5InternalDataProcessTest() throws JsonProcessingException {
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    fbbL5CreateDTO =
      FbbL5CreateDTO.builder().itemSku(ITEM_SKU).buyable(true).discoverable(true)
        .businessPartnerCode(MERCHANT_CODE).build();
    BulkInternalProcessData internalProcessData =
      RequestHelper.getFbbL5InternalDataProcess(STORE_ID,
        bulkInternalProcess, fbbL5CreateDTO);
    Assertions.assertEquals(internalProcessData.getSellerCode(), MERCHANT_CODE);
  }

  @Test
  public void toBulkInternalProcessForAutoAssignmentTest() throws JsonProcessingException {
    VendorAutoAssignmentRequest vendorAutoAssignmentRequest = new VendorAutoAssignmentRequest();
    List<String> assigneeList = Arrays.asList(MERCHANT_CODE, ITEM_SKU);
    VendorAutoAssignmentFilterRequest vendorAutoAssignmentFilterRequest = new VendorAutoAssignmentFilterRequest();
    vendorAutoAssignmentFilterRequest.setBrandPending(true);
    vendorAutoAssignmentFilterRequest.setBusinessPartnerCode(MERCHANT_CODE);
    vendorAutoAssignmentRequest.setAssigneeList(assigneeList);
    vendorAutoAssignmentRequest.setInternalProcessRequestCode(MERCHANT_CODE);
    BulkInternalProcess internalProcess =
        RequestHelper.toBulkInternalProcessForAutoAssignment(vendorAutoAssignmentRequest);
    Assertions.assertEquals(internalProcess.getInternalProcessRequestCode(), MERCHANT_CODE);
    Assertions.assertEquals(internalProcess.getNotes(), new ObjectMapper().writeValueAsString(assigneeList));
    Assertions.assertEquals(internalProcess.getFileName(),
        new ObjectMapper().writeValueAsString(vendorAutoAssignmentRequest.getVendorAutoAssignmentFilterRequest()));
  }

  @Test
  public void totoBoostedProductFilterRequest(){
    VendorAutoAssignmentFilterRequest vendorAutoAssignmentFilterRequest =
      VendorAutoAssignmentFilterRequest.builder().edited(true).revised(true).postLive(true).build();
    BoostedProductFilterRequest boostedProductFilterRequest =
      RequestHelper.toBoostedProductFilterRequest(vendorAutoAssignmentFilterRequest);
    Assertions.assertEquals(vendorAutoAssignmentFilterRequest.getEdited(),
      boostedProductFilterRequest.getEdited());
  }

  @Test
  public void totoBoostedProductFilterRequestNullTest(){
    VendorAutoAssignmentFilterRequest vendorAutoAssignmentFilterRequest =
      null;
    BoostedProductFilterRequest boostedProductFilterRequest =
      RequestHelper.toBoostedProductFilterRequest(vendorAutoAssignmentFilterRequest);
  }

 @Test
 public void updateInternalProcessForVendorAutoAssignmentTest(){
   BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
   List<BulkInternalProcessData> bulkInternalProcessData = new ArrayList<>();
     RequestHelper.updateInternalProcessForVendorAutoAssignment(bulkInternalProcess,
       bulkInternalProcessData, STORE_COPY,0);
     Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(),bulkInternalProcess.getStatus());

  }

  @Test
  public void getBulkInternalProcessFromBulkRestrictedKeywordUploadRequestTest() {
    BulkRestrictedKeywordUploadModel bulkRestrictedKeywordUploadModel =
        BulkRestrictedKeywordUploadModel.builder().bulkProcessCode(INTERNAL_PROCESS_CODE)
            .bulkProcessType(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name()).filePath(STORE_COPY)
            .actionType(SALES_CATEGORY_UPDATE).build();
    BulkInternalProcess bulkInternalProcess =
        RequestHelper.getBulkInternalProcessFromBulkRestrictedKeywordUploadRequest(STORE_ID,
            bulkRestrictedKeywordUploadModel);
    Assertions.assertEquals(ProcessStatus.PENDING.name(), bulkInternalProcess.getStatus());
    Assertions.assertEquals(INTERNAL_PROCESS_CODE, bulkInternalProcess.getInternalProcessRequestCode());
    Assertions.assertEquals(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name(), bulkInternalProcess.getProcessType());
    Assertions.assertEquals(STORE_COPY, bulkInternalProcess.getFileName());
    Assertions.assertEquals(SALES_CATEGORY_UPDATE, bulkInternalProcess.getNotes());
  }

  @Test
  public void validateDataForRestrictedKeywordUpsertRowValidateTest() {
    BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
    for (Map.Entry<Map<String, String>, String> request : internalSheetUpsertDataRequestResponseMap.entrySet()) {
      RequestHelper.validateDataForRestrictedKeywordUpsert(request.getKey(), restrictedKeywordRequestData,
          bulkInternalProcessData);
      Assertions.assertEquals(request.getValue(), bulkInternalProcessData.getErrorMessage());
      bulkInternalProcessData.setErrorMessage(null);
    }
  }

  @Test
  public void validateDataForRestrictedKeywordDeleteTest() {
    BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
    for (Map.Entry<Map<String, String>, String> request : internalSheetDeleteDataRequestResponseMap.entrySet()) {
      RequestHelper.validateDataForRestrictedKeywordDelete(request.getKey(), restrictedKeywordRequestData,
          bulkInternalProcessData);
      Assertions.assertEquals(request.getValue(), bulkInternalProcessData.getErrorMessage());
      bulkInternalProcessData.setErrorMessage(null);
    }
  }

  @Test
  public void validateDataForRestrictedKeywordUpsertTest() {
    BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
    for (Map.Entry<RestrictedKeywordRequestData, String> request : requestResponseMap.entrySet()) {
      RequestHelper.validateDataForRestrictedKeywordUpsert(new HashMap<>(), request.getKey(), bulkInternalProcessData);
      Assertions.assertEquals(request.getValue(), bulkInternalProcessData.getErrorMessage());
      Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessData.getStatus());
    }
  }

  @Test
  public void getBulkInternalProcessFromBulkBrandAuthUploadRequestTest() {
    BulkBrandAuthUploadModel bulkBrandAuthUploadModel =
        BulkBrandAuthUploadModel.builder().bulkProcessCode(INTERNAL_PROCESS_CODE)
            .bulkProcessType(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name()).filePath(STORE_COPY).build();
    BulkInternalProcess bulkInternalProcess =
        RequestHelper.getBulkInternalProcessFromBulkBrandAuthUploadRequest(STORE_ID, bulkBrandAuthUploadModel);
    Assertions.assertEquals(ProcessStatus.PENDING.name(), bulkInternalProcess.getStatus());
    Assertions.assertEquals(INTERNAL_PROCESS_CODE, bulkInternalProcess.getInternalProcessRequestCode());
    Assertions.assertEquals(BulkInternalProcessType.RESTRICTED_KEYWORD_UPSERT.name(), bulkInternalProcess.getProcessType());
    Assertions.assertEquals(STORE_COPY, bulkInternalProcess.getFileName());
  }

  @Test
  public void testToBulkProcessStatusListingResponse()
    throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue());
    bulkProcess.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
    bulkProcess.setTotalCount(100);
    bulkProcess.setSuccessCount(99);
    bulkProcess.setErrorCount(1);
    bulkProcess.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkProcess.setCreatedDate(new Date());
    bulkProcess.setCreatedBy(DEFAULT_USERNAME);
    bulkProcess.setUploadedFile("File1");
    RequestHelper.setFileStorageService(fileStorageService);
    List<BulkProcess> bulkProcessList = Collections.singletonList(bulkProcess);
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcessList, PageRequest.of(0, 10), 1L);
    when(fileStorageService.getErrorFileLinkForListing(bulkProcess)).thenReturn("ErrorFileLink");
    List<BulkProcessStatusListingResponse> result = RequestHelper.toBulkProcessStatusListingResponse(bulkProcessPage,
        new HashSet<>());
    Mockito.verify(fileStorageService).getErrorFileLinkForListing(bulkProcess);
    Assertions.assertEquals(bulkProcessList.size(), result.size());
    BulkProcessStatusListingResponse listingResponse = result.get(0);
    Assertions.assertEquals(bulkProcess.getBulkProcessCode(), listingResponse.getBulkProcessCode());
    Assertions.assertEquals(bulkProcess.getBulkProcessType(), listingResponse.getBulkProcessType());
    Assertions.assertEquals(BulkActivityStatus.PARTIAL_SUCCESS, listingResponse.getBulkActivityStatus());
    Assertions.assertEquals(99.0, listingResponse.getProcessCompletionPercentage(),0);
    Assertions.assertTrue(listingResponse.isProcessCompleted());
    Assertions.assertEquals("ErrorFileLink", listingResponse.getErrorFileLink());
    Assertions.assertEquals(100, listingResponse.getTotalRowCountRequested());
    Assertions.assertEquals(1, listingResponse.getErrorRowCount());
    Assertions.assertEquals(bulkProcess.getBusinessPartnerCode(), listingResponse.getBusinessPartnerCode());
    Assertions.assertNull(listingResponse.getEstimatedCompletionTime());
    Assertions.assertEquals(bulkProcess.getCreatedDate(), listingResponse.getUploadDate());
    Assertions.assertEquals(bulkProcess.getCreatedBy(), listingResponse.getUser());
    Assertions.assertEquals(bulkProcess.getUploadedFile(), listingResponse.getUploadedFileName());
  }

  @Test
  public void testToBulkProcessStatusListingResponseWithoutErrorLink()
      throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue());
    bulkProcess.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
    bulkProcess.setTotalCount(100);
    bulkProcess.setSuccessCount(99);
    bulkProcess.setErrorCount(0);
    bulkProcess.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkProcess.setCreatedDate(new Date());
    bulkProcess.setCreatedBy(DEFAULT_USERNAME);
    bulkProcess.setUploadedFile("File1");
    RequestHelper.setFileStorageService(fileStorageService);
    List<BulkProcess> bulkProcessList = Collections.singletonList(bulkProcess);
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcessList, PageRequest.of(0, 10), 1L);
    List<BulkProcessStatusListingResponse> result = RequestHelper.toBulkProcessStatusListingResponse(bulkProcessPage,
        new HashSet<>());
    Assertions.assertEquals(bulkProcessList.size(), result.size());
    BulkProcessStatusListingResponse listingResponse = result.get(0);
    Assertions.assertEquals(bulkProcess.getBulkProcessCode(), listingResponse.getBulkProcessCode());
    Assertions.assertEquals(bulkProcess.getBulkProcessType(), listingResponse.getBulkProcessType());
    Assertions.assertEquals(BulkActivityStatus.PARTIAL_SUCCESS, listingResponse.getBulkActivityStatus());
    Assertions.assertEquals(99.0, listingResponse.getProcessCompletionPercentage(),0);
    Assertions.assertTrue(listingResponse.isProcessCompleted());
    Assertions.assertEquals("", listingResponse.getErrorFileLink());
    Assertions.assertEquals(100, listingResponse.getTotalRowCountRequested());
    Assertions.assertEquals(0, listingResponse.getErrorRowCount());
    Assertions.assertEquals(bulkProcess.getBusinessPartnerCode(), listingResponse.getBusinessPartnerCode());
    Assertions.assertNull(listingResponse.getEstimatedCompletionTime());
    Assertions.assertEquals(bulkProcess.getCreatedDate(), listingResponse.getUploadDate());
    Assertions.assertEquals(bulkProcess.getCreatedBy(), listingResponse.getUser());
    Assertions.assertEquals(bulkProcess.getUploadedFile(), listingResponse.getUploadedFileName());
  }

  @Test
  public void testToBulkProcessStatusListingResponseWithoutErrorLink2()
    throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue());
    bulkProcess.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
    bulkProcess.setTotalCount(100);
    bulkProcess.setSuccessCount(0);
    bulkProcess.setErrorCount(10);
    bulkProcess.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkProcess.setCreatedDate(new Date());
    bulkProcess.setCreatedBy(DEFAULT_USERNAME);
    bulkProcess.setUploadedFile("File1");
    RequestHelper.setFileStorageService(fileStorageService);
    List<BulkProcess> bulkProcessList = Collections.singletonList(bulkProcess);
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcessList, PageRequest.of(0, 10), 1L);
    List<BulkProcessStatusListingResponse> result = RequestHelper.toBulkProcessStatusListingResponse(bulkProcessPage,
        new HashSet<>());
    Assertions.assertEquals(bulkProcessList.size(), result.size());
    BulkProcessStatusListingResponse listingResponse = result.get(0);
    Assertions.assertEquals(bulkProcess.getBulkProcessCode(), listingResponse.getBulkProcessCode());
    Assertions.assertEquals(bulkProcess.getBulkProcessType(), listingResponse.getBulkProcessType());
    Assertions.assertEquals(BulkActivityStatus.PARTIAL_SUCCESS, listingResponse.getBulkActivityStatus());
    Assertions.assertTrue(listingResponse.isProcessCompleted());
    Assertions.assertEquals(100, listingResponse.getTotalRowCountRequested());
    Assertions.assertEquals(bulkProcess.getBusinessPartnerCode(), listingResponse.getBusinessPartnerCode());
    Assertions.assertNull(listingResponse.getEstimatedCompletionTime());
    Assertions.assertEquals(bulkProcess.getCreatedDate(), listingResponse.getUploadDate());
    Assertions.assertEquals(bulkProcess.getCreatedBy(), listingResponse.getUser());
    Assertions.assertEquals(bulkProcess.getUploadedFile(), listingResponse.getUploadedFileName());
  }

  @Test
  public void testToBulkProcessStatusListingResponseWithoutErrorLinkForSystemErrorCount()
    throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue());
    bulkProcess.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
    bulkProcess.setTotalCount(100);
    bulkProcess.setSuccessCount(99);
    bulkProcess.setErrorCount(0);
    bulkProcess.setSystemErrorCount(10);
    bulkProcess.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkProcess.setCreatedDate(new Date());
    bulkProcess.setCreatedBy(DEFAULT_USERNAME);
    bulkProcess.setUploadedFile("File1");
    RequestHelper.setFileStorageService(fileStorageService);
    List<BulkProcess> bulkProcessList = Collections.singletonList(bulkProcess);
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcessList, PageRequest.of(0, 10), 1L);
    List<BulkProcessStatusListingResponse> result = RequestHelper.toBulkProcessStatusListingResponse(bulkProcessPage,
        new HashSet<>());
    Assertions.assertEquals(bulkProcessList.size(), result.size());
    BulkProcessStatusListingResponse listingResponse = result.get(0);
    Assertions.assertEquals(bulkProcess.getBulkProcessCode(), listingResponse.getBulkProcessCode());
    Assertions.assertEquals(bulkProcess.getBulkProcessType(), listingResponse.getBulkProcessType());
    Assertions.assertEquals(BulkActivityStatus.PARTIAL_SUCCESS, listingResponse.getBulkActivityStatus());
    Assertions.assertEquals(99.0, listingResponse.getProcessCompletionPercentage(),0);
    Assertions.assertTrue(listingResponse.isProcessCompleted());
    Assertions.assertEquals(100, listingResponse.getTotalRowCountRequested());
    Assertions.assertEquals(bulkProcess.getBusinessPartnerCode(), listingResponse.getBusinessPartnerCode());
    Assertions.assertNull(listingResponse.getEstimatedCompletionTime());
    Assertions.assertEquals(bulkProcess.getCreatedDate(), listingResponse.getUploadDate());
    Assertions.assertEquals(bulkProcess.getCreatedBy(), listingResponse.getUser());
    Assertions.assertEquals(bulkProcess.getUploadedFile(), listingResponse.getUploadedFileName());
  }

  @Test
  public void testToBulkProcessStatusListingResponseWithoutErrorLinkForInputErrorCount()
    throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue());
    bulkProcess.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
    bulkProcess.setTotalCount(100);
    bulkProcess.setSuccessCount(99);
    bulkProcess.setErrorCount(0);
    bulkProcess.setSystemErrorCount(0);
    bulkProcess.setInputErrorCount(1110);
    bulkProcess.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkProcess.setCreatedDate(new Date());
    bulkProcess.setCreatedBy(DEFAULT_USERNAME);
    bulkProcess.setUploadedFile("File1");
    RequestHelper.setFileStorageService(fileStorageService);
    List<BulkProcess> bulkProcessList = Collections.singletonList(bulkProcess);
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcessList, PageRequest.of(0, 10), 1L);
    List<BulkProcessStatusListingResponse> result = RequestHelper.toBulkProcessStatusListingResponse(bulkProcessPage,
        new HashSet<>());
    Assertions.assertEquals(bulkProcessList.size(), result.size());
    BulkProcessStatusListingResponse listingResponse = result.get(0);
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkIPRProductAddReviewUnknownSourceTest()
    throws Exception {
    BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
      BulkAddReviewIPRProductsRequestData.builder().productSku(PRODUCT_SKU)
        .productName(PRODUCT_NAME).reasons(REASON_VALUE).violationType(VIOLATION_TYPE_VALUE)
        .source(SOURCE).action(ACTION_VALUE).sellerNotes(SELLER_NOTES)
        .reviewerNotes(REVIEWER_NOTES).excelRowNumber(1).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
      BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_SKU,
      bulkAddReviewIPRProductsRequestData.getProductSku());
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_NAME,
      bulkAddReviewIPRProductsRequestData.getProductName());
    keyValuePairMap.put(BulkIPRProductsParameter.VIOLATION_TYPE,
      bulkAddReviewIPRProductsRequestData.getViolationType());
    keyValuePairMap.put(BulkIPRProductsParameter.ACTION,
      bulkAddReviewIPRProductsRequestData.getAction());
    keyValuePairMap.put(BulkIPRProductsParameter.SELLER_NOTES,
      bulkAddReviewIPRProductsRequestData.getSellerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REVIEWER_NOTES,
      bulkAddReviewIPRProductsRequestData.getReviewerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REASON,
      bulkAddReviewIPRProductsRequestData.getReasons());
    keyValuePairMap.put(BulkIPRProductsParameter.SOURCE,
      bulkAddReviewIPRProductsRequestData.getSource());
    internalProcessDataFromExcel.add(
      keyValuePairMap);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setIprValidActions(Collections.singleton(ACTION_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprSource(Collections.singleton(STATUS));
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkIPRProductsParameter.IPR_REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
      RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.UNKNOWN_SOURCE, actualBulkInternalProcessDataList.get(0).getErrorMessage());
    Assertions.assertEquals(bulkAddReviewIPRProductsRequestData,
      new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
        BulkAddReviewIPRProductsRequestData.class));
  }

  @Test
  public void getBulkInternalProcessDataForNewBulkPriceUpdateTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.BULK_PRICE_UPDATE_SKU_ID, "TEB-24219-00001-00001");
    excelData.put(BulkParameters.BULK_PRICE_UPDATE_PICKUP_POINT_CODE, "PP-123456");
    excelData.put(BulkParameters.BULK_PRICE_UPDATE_NEW_NORMAL_PRICE, "1000");
    excelData.put(BulkParameters.BULK_PRICE_UPDATE_NEW_SELLING_PRICE, "1000");
    excelData.put(BulkParameters.BULK_PRICE_UPDATE_NEW_CAMPAIGN_PRICE, "1000");
    excelData.put(BulkParameters.BULK_PRICE_UPDATE_CAMPAIGN_ID, "CAMP-123456");
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Collections.singletonList(excelData));
    bulkInternalProcessDataGenerationDTO.setBulkPriceUpdateNewMaxRows(2);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
      RequestHelper.getBulkInternalProcessDataForNewBulkPriceUpdate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void getBulkInternalProcessDataForNewBulkPriceUpdate_exceedMaxLimitTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.BULK_PRICE_UPDATE_SKU_ID, "TEB-24219-00001-00001");
    excelData.put(BulkParameters.BULK_PRICE_UPDATE_PICKUP_POINT_CODE, "PP-123456");
    excelData.put(BulkParameters.BULK_PRICE_UPDATE_NEW_NORMAL_PRICE, "1000");
    excelData.put(BulkParameters.BULK_PRICE_UPDATE_NEW_SELLING_PRICE, "1000");
    excelData.put(BulkParameters.BULK_PRICE_UPDATE_NEW_CAMPAIGN_PRICE, "1000");
    excelData.put(BulkParameters.BULK_PRICE_UPDATE_CAMPAIGN_ID, "CAMP-123456");
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Collections.singletonList(excelData));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
      RequestHelper.getBulkInternalProcessDataForNewBulkPriceUpdate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(0, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(), bulkInternalProcess.getStatus());
  }

  @Test
  public void getBulkInternalProcessDataForNewBulkPriceUpdate_invalidDataTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.BULK_PRICE_UPDATE_SKU_ID, "TEB-24219-00001");
    excelData.put(BulkParameters.BULK_PRICE_UPDATE_NEW_NORMAL_PRICE, "");
    excelData.put(BulkParameters.BULK_PRICE_UPDATE_NEW_SELLING_PRICE, "1000");
    excelData.put(BulkParameters.BULK_PRICE_UPDATE_NEW_CAMPAIGN_PRICE, "1000");
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    Map<String, String> excelData2 = new HashMap<>();
    excelData2.put(BulkParameters.BULK_PRICE_UPDATE_SKU_ID, "TEB-24219-00001-00001");
    excelData2.put(BulkParameters.BULK_PRICE_UPDATE_PICKUP_POINT_CODE, "");
    excelData2.put(BulkParameters.BULK_PRICE_UPDATE_NEW_NORMAL_PRICE, "");
    excelData2.put(BulkParameters.BULK_PRICE_UPDATE_NEW_SELLING_PRICE, "");
    excelData2.put(BulkParameters.BULK_PRICE_UPDATE_NEW_CAMPAIGN_PRICE, "1000");
    excelData2.put(BulkParameters.ROW_NUMBER, "2");
    Map<String, String> excelData3 = new HashMap<>();
    excelData3.put(BulkParameters.BULK_PRICE_UPDATE_SKU_ID, "TEB-24219-00001-00001");
    excelData3.put(BulkParameters.BULK_PRICE_UPDATE_PICKUP_POINT_CODE, "PP-123456");
    excelData3.put(BulkParameters.BULK_PRICE_UPDATE_NEW_NORMAL_PRICE, "1000");
    excelData3.put(BulkParameters.BULK_PRICE_UPDATE_NEW_SELLING_PRICE, "1000");
    excelData3.put(BulkParameters.BULK_PRICE_UPDATE_NEW_CAMPAIGN_PRICE, "1000");
    excelData3.put(BulkParameters.ROW_NUMBER, "3");
    Map<String, String> excelData4 = new HashMap<>();
    excelData4.put(BulkParameters.BULK_PRICE_UPDATE_SKU_ID, "TEB-24219-00001-00001");
    excelData4.put(BulkParameters.BULK_PRICE_UPDATE_PICKUP_POINT_CODE, "PP-123456");
    excelData4.put(BulkParameters.BULK_PRICE_UPDATE_NEW_NORMAL_PRICE, "1000");
    excelData4.put(BulkParameters.BULK_PRICE_UPDATE_NEW_SELLING_PRICE, "1000");
    excelData4.put(BulkParameters.BULK_PRICE_UPDATE_NEW_CAMPAIGN_PRICE, "1000");
    excelData4.put(BulkParameters.BULK_PRICE_UPDATE_CAMPAIGN_ID, "CAMP-1234");
    excelData4.put(BulkParameters.ROW_NUMBER, "4");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(List.of(excelData, excelData2, excelData3, excelData4));
    bulkInternalProcessDataGenerationDTO.setBulkPriceUpdateNewMaxRows(5);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
      RequestHelper.getBulkInternalProcessDataForNewBulkPriceUpdate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(4, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(1).getStatus());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(2).getStatus());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(3).getStatus());
  }

  @Test
  public void getBulkInternalProcessDataForNewBulkPriceUpdate_duplicateRowsTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.BULK_PRICE_UPDATE_SKU_ID, "TEB-24219-00001-00001");
    excelData.put(BulkParameters.BULK_PRICE_UPDATE_PICKUP_POINT_CODE, "PP-123456");
    excelData.put(BulkParameters.BULK_PRICE_UPDATE_NEW_NORMAL_PRICE, "1000");
    excelData.put(BulkParameters.BULK_PRICE_UPDATE_NEW_SELLING_PRICE, "1000");
    excelData.put(BulkParameters.BULK_PRICE_UPDATE_NEW_CAMPAIGN_PRICE, "");
    excelData.put(BulkParameters.BULK_PRICE_UPDATE_CAMPAIGN_ID, "CAMP-123456");
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    Map<String, String> excelData2 = new HashMap<>();
    excelData2.put(BulkParameters.BULK_PRICE_UPDATE_SKU_ID, "TEB-24219-00001-00001");
    excelData2.put(BulkParameters.BULK_PRICE_UPDATE_PICKUP_POINT_CODE, "PP-123456");
    excelData2.put(BulkParameters.BULK_PRICE_UPDATE_NEW_NORMAL_PRICE, "1000");
    excelData2.put(BulkParameters.BULK_PRICE_UPDATE_NEW_SELLING_PRICE, "1000");
    excelData2.put(BulkParameters.BULK_PRICE_UPDATE_NEW_CAMPAIGN_PRICE, "");
    excelData2.put(BulkParameters.BULK_PRICE_UPDATE_CAMPAIGN_ID, "CAMP-123456");
    excelData2.put(BulkParameters.ROW_NUMBER, "2");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(List.of(excelData, excelData2));
    bulkInternalProcessDataGenerationDTO.setBulkPriceUpdateNewMaxRows(3);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
      RequestHelper.getBulkInternalProcessDataForNewBulkPriceUpdate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(2, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(1).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.DUPLICATE_L5_ROW_ERROR,
      bulkInternalProcessDataList.get(0).getErrorMessage());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.DUPLICATE_L5_ROW_ERROR,
      bulkInternalProcessDataList.get(1).getErrorMessage());
  }

  @Test
  public void getBulkInternalProcessDataForNewBulkPriceUpdate_noRowsDetectedForPriceUpdateTest() throws JsonProcessingException {
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Collections.emptyList());
    bulkInternalProcessDataGenerationDTO.setBulkPriceUpdateNewMaxRows(3);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
      RequestHelper.getBulkInternalProcessDataForNewBulkPriceUpdate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(0, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(), bulkInternalProcess.getStatus());
    Assertions.assertTrue(bulkInternalProcessDataGenerationDTO.isNoRowsDetectedForPriceUpdate());
  }


  @Test
  public void testToBulkProcessStatusListingResponseWithErrorLinkForInputErrorZero()
    throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue());
    bulkProcess.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
    bulkProcess.setTotalCount(100);
    bulkProcess.setSuccessCount(99);
    bulkProcess.setErrorCount(10);
    bulkProcess.setSystemErrorCount(0);
    bulkProcess.setInputErrorCount(0);
    bulkProcess.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkProcess.setCreatedDate(new Date());
    bulkProcess.setCreatedBy(DEFAULT_USERNAME);
    bulkProcess.setUploadedFile("File1");
    RequestHelper.setFileStorageService(fileStorageService);
    List<BulkProcess> bulkProcessList = Collections.singletonList(bulkProcess);
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcessList, PageRequest.of(0, 10), 1L);
    when(fileStorageService.getErrorFileLinkForListing(bulkProcess)).thenReturn("ErrorFileLink");
    List<BulkProcessStatusListingResponse> result = RequestHelper.toBulkProcessStatusListingResponse(bulkProcessPage,
        new HashSet<>());
    Mockito.verify(fileStorageService).getErrorFileLinkForListing(bulkProcess);
    Assertions.assertEquals(bulkProcessList.size(), result.size());
    BulkProcessStatusListingResponse listingResponse = result.get(0);
    Assertions.assertEquals(bulkProcess.getBulkProcessCode(), listingResponse.getBulkProcessCode());
    Assertions.assertEquals(bulkProcess.getBulkProcessType(), listingResponse.getBulkProcessType());
    Assertions.assertEquals(BulkActivityStatus.PARTIAL_SUCCESS, listingResponse.getBulkActivityStatus());
    Assertions.assertEquals(99.0, listingResponse.getProcessCompletionPercentage(),0);
    Assertions.assertTrue(listingResponse.isProcessCompleted());
    assertNotEquals(StringUtils.EMPTY, listingResponse.getErrorFileLink());
    Assertions.assertEquals(100, listingResponse.getTotalRowCountRequested());
    Assertions.assertEquals(bulkProcess.getBusinessPartnerCode(), listingResponse.getBusinessPartnerCode());
    Assertions.assertNull(listingResponse.getEstimatedCompletionTime());
    Assertions.assertEquals(bulkProcess.getCreatedDate(), listingResponse.getUploadDate());
    Assertions.assertEquals(bulkProcess.getCreatedBy(), listingResponse.getUser());
    Assertions.assertEquals(bulkProcess.getUploadedFile(), listingResponse.getUploadedFileName());
  }

  @Test
  public void testToBulkProcessStatusListingEmptyErrorFile()
    throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue());
    bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    bulkProcess.setTotalCount(100);
    bulkProcess.setSuccessCount(99);
    bulkProcess.setErrorCount(1);
    bulkProcess.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkProcess.setCreatedDate(new Date());
    bulkProcess.setCreatedBy(DEFAULT_USERNAME);
    bulkProcess.setUploadedFile("File1");
    RequestHelper.setFileStorageService(fileStorageService);
    List<BulkProcess> bulkProcessList = Collections.singletonList(bulkProcess);
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcessList, PageRequest.of(0, 10), 1L);
    List<BulkProcessStatusListingResponse> result = RequestHelper.toBulkProcessStatusListingResponse(bulkProcessPage,
        new HashSet<>());
    Assertions.assertEquals(bulkProcessList.size(), result.size());
    BulkProcessStatusListingResponse listingResponse = result.get(0);
    Assertions.assertEquals(bulkProcess.getBulkProcessCode(), listingResponse.getBulkProcessCode());
    Assertions.assertEquals(bulkProcess.getBulkProcessType(), listingResponse.getBulkProcessType());
    Assertions.assertEquals(BulkActivityStatus.SUCCESS, listingResponse.getBulkActivityStatus());
    Assertions.assertEquals(99.0, listingResponse.getProcessCompletionPercentage(),0);
    Assertions.assertTrue(listingResponse.isProcessCompleted());
    Assertions.assertEquals("", listingResponse.getErrorFileLink());
    Assertions.assertEquals(100, listingResponse.getTotalRowCountRequested());
    Assertions.assertEquals(1, listingResponse.getErrorRowCount());
    Assertions.assertEquals(bulkProcess.getBusinessPartnerCode(), listingResponse.getBusinessPartnerCode());
    Assertions.assertNull(listingResponse.getEstimatedCompletionTime());
    Assertions.assertEquals(bulkProcess.getCreatedDate(), listingResponse.getUploadDate());
    Assertions.assertEquals(bulkProcess.getCreatedBy(), listingResponse.getUser());
    Assertions.assertEquals(bulkProcess.getUploadedFile(), listingResponse.getUploadedFileName());
  }


  @Test
  public void testToBulkProcessStatusListingResponse_pending()
    throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue());
    bulkProcess.setStatus(BulkProcess.STATUS_PENDING);
    bulkProcess.setTotalCount(100);
    bulkProcess.setSuccessCount(0);
    bulkProcess.setErrorCount(0);
    bulkProcess.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkProcess.setCreatedDate(new Date());
    bulkProcess.setCreatedBy(DEFAULT_USERNAME);
    bulkProcess.setUploadedFile("File1");
    RequestHelper.setFileStorageService(fileStorageService);
    List<BulkProcess> bulkProcessList = Collections.singletonList(bulkProcess);
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcessList, PageRequest.of(0, 10), 1L);
    when(fileStorageService.getErrorFileLinkForListing(bulkProcess)).thenReturn("ErrorFileLink");
    List<BulkProcessStatusListingResponse> result = RequestHelper.toBulkProcessStatusListingResponse(bulkProcessPage,
        new HashSet<>());
    Assertions.assertEquals(bulkProcessList.size(), result.size());
    BulkProcessStatusListingResponse listingResponse = result.get(0);
    Assertions.assertEquals(bulkProcess.getBulkProcessCode(), listingResponse.getBulkProcessCode());
    Assertions.assertEquals(bulkProcess.getBulkProcessType(), listingResponse.getBulkProcessType());
    Assertions.assertEquals(BulkActivityStatus.PENDING, listingResponse.getBulkActivityStatus());
    Assertions.assertEquals(0.0, listingResponse.getProcessCompletionPercentage(),0);
    Assertions.assertFalse(listingResponse.isProcessCompleted());
    Assertions.assertEquals("", listingResponse.getErrorFileLink());
    Assertions.assertEquals(100, listingResponse.getTotalRowCountRequested());
    Assertions.assertEquals(0, listingResponse.getErrorRowCount());
    Assertions.assertEquals(bulkProcess.getBusinessPartnerCode(), listingResponse.getBusinessPartnerCode());
    Assertions.assertNull(listingResponse.getEstimatedCompletionTime());
    Assertions.assertEquals(bulkProcess.getCreatedDate(), listingResponse.getUploadDate());
    Assertions.assertEquals(bulkProcess.getCreatedBy(), listingResponse.getUser());
    Assertions.assertEquals(bulkProcess.getUploadedFile(), listingResponse.getUploadedFileName());
  }

  @Test
  public void testToBulkProcessStatusListingResponseForSuccess()
    throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue());
    bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    bulkProcess.setTotalCount(100);
    bulkProcess.setSuccessCount(100);
    bulkProcess.setErrorCount(0);
    bulkProcess.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkProcess.setCreatedDate(new Date());
    bulkProcess.setCreatedBy(DEFAULT_USERNAME);
    bulkProcess.setUploadedFile("File2");
    RequestHelper.setFileStorageService(fileStorageService);
    List<BulkProcess> bulkProcessList = Collections.singletonList(bulkProcess);
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcessList, PageRequest.of(0, 10), 1L);
    when(fileStorageService.getErrorFileLinkForListing(bulkProcess)).thenReturn("ErrorFileLink");
    List<BulkProcessStatusListingResponse> result = RequestHelper.toBulkProcessStatusListingResponse(bulkProcessPage,
        new HashSet<>());
    Assertions.assertEquals(bulkProcessList.size(), result.size());
    BulkProcessStatusListingResponse listingResponse = result.get(0);
    Assertions.assertEquals(bulkProcess.getBulkProcessCode(), listingResponse.getBulkProcessCode());
    Assertions.assertEquals(bulkProcess.getBulkProcessType(), listingResponse.getBulkProcessType());
    Assertions.assertEquals(BulkActivityStatus.SUCCESS, listingResponse.getBulkActivityStatus());
    Assertions.assertEquals(100.0, listingResponse.getProcessCompletionPercentage(),0);
    Assertions.assertTrue(listingResponse.isProcessCompleted());
    Assertions.assertEquals("", listingResponse.getErrorFileLink());
    Assertions.assertEquals(100, listingResponse.getTotalRowCountRequested());
    Assertions.assertEquals(0, listingResponse.getErrorRowCount());
    Assertions.assertEquals(bulkProcess.getBusinessPartnerCode(), listingResponse.getBusinessPartnerCode());
    Assertions.assertNull(listingResponse.getEstimatedCompletionTime());
    Assertions.assertEquals(bulkProcess.getCreatedDate(), listingResponse.getUploadDate());
    Assertions.assertEquals(bulkProcess.getCreatedBy(), listingResponse.getUser());
    Assertions.assertEquals(bulkProcess.getUploadedFile(), listingResponse.getUploadedFileName());
  }

  @Test
  public void testToBulkProcessStatusListingResponseForUnfinished()
    throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue());
    bulkProcess.setStatus(BulkProcess.STATUS_ABORTED);
    bulkProcess.setTotalCount(100);
    bulkProcess.setSuccessCount(99);
    bulkProcess.setErrorCount(1);
    bulkProcess.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkProcess.setCreatedDate(new Date());
    bulkProcess.setCreatedBy(DEFAULT_USERNAME);
    bulkProcess.setUploadedFile("File2");
    RequestHelper.setFileStorageService(fileStorageService);
    List<BulkProcess> bulkProcessList = Collections.singletonList(bulkProcess);
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcessList, PageRequest.of(0, 10), 1L);
    when(fileStorageService.getErrorFileLinkForListing(bulkProcess)).thenReturn("ErrorFileLink");
    List<BulkProcessStatusListingResponse> result = RequestHelper.toBulkProcessStatusListingResponse(bulkProcessPage,
        new HashSet<>());
    Assertions.assertEquals(bulkProcessList.size(), result.size());
    BulkProcessStatusListingResponse listingResponse = result.get(0);
    Assertions.assertEquals(bulkProcess.getBulkProcessCode(), listingResponse.getBulkProcessCode());
    Assertions.assertEquals(bulkProcess.getBulkProcessType(), listingResponse.getBulkProcessType());
    Assertions.assertEquals(BulkActivityStatus.FAILED, listingResponse.getBulkActivityStatus());
    Assertions.assertEquals(99.0, listingResponse.getProcessCompletionPercentage(),0);
    Assertions.assertTrue(listingResponse.isProcessCompleted());
    Assertions.assertEquals("ErrorFileLink", listingResponse.getErrorFileLink());
    Assertions.assertEquals(100, listingResponse.getTotalRowCountRequested());
    Assertions.assertEquals(1, listingResponse.getErrorRowCount());
    Assertions.assertEquals(bulkProcess.getBusinessPartnerCode(), listingResponse.getBusinessPartnerCode());
    Assertions.assertNull(listingResponse.getEstimatedCompletionTime());
    Assertions.assertEquals(bulkProcess.getCreatedDate(), listingResponse.getUploadDate());
    Assertions.assertEquals(bulkProcess.getCreatedBy(), listingResponse.getUser());
    Assertions.assertEquals(bulkProcess.getUploadedFile(), listingResponse.getUploadedFileName());
  }

  @Test
  public void testToBulkProcessStatusListingResponseForPartialDone()
    throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue());
    bulkProcess.setStatus(BulkProcess.STATUS_PARTIALLY_DONE);
    bulkProcess.setTotalCount(100);
    bulkProcess.setSuccessCount(100);
    bulkProcess.setErrorCount(0);
    bulkProcess.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkProcess.setCreatedDate(new Date());
    bulkProcess.setCreatedBy(DEFAULT_USERNAME);
    bulkProcess.setUploadedFile("File2");
    RequestHelper.setFileStorageService(fileStorageService);
    List<BulkProcess> bulkProcessList = Collections.singletonList(bulkProcess);
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcessList, PageRequest.of(0, 10), 1L);
    when(fileStorageService.getErrorFileLinkForListing(bulkProcess)).thenReturn("ErrorFileLink");
    List<BulkProcessStatusListingResponse> result = RequestHelper.toBulkProcessStatusListingResponse(bulkProcessPage,
        new HashSet<>());
    Assertions.assertEquals(bulkProcessList.size(), result.size());
    BulkProcessStatusListingResponse listingResponse = result.get(0);
    Assertions.assertEquals(bulkProcess.getBulkProcessCode(), listingResponse.getBulkProcessCode());
    Assertions.assertEquals(bulkProcess.getBulkProcessType(), listingResponse.getBulkProcessType());
    Assertions.assertEquals(BulkActivityStatus.PARTIAL_SUCCESS, listingResponse.getBulkActivityStatus());
    Assertions.assertEquals(100.0, listingResponse.getProcessCompletionPercentage(),0);
    Assertions.assertTrue(listingResponse.isProcessCompleted());
    Assertions.assertEquals("", listingResponse.getErrorFileLink());
    Assertions.assertEquals(100, listingResponse.getTotalRowCountRequested());
    Assertions.assertEquals(0, listingResponse.getErrorRowCount());
    Assertions.assertEquals(bulkProcess.getBusinessPartnerCode(), listingResponse.getBusinessPartnerCode());
    Assertions.assertNull(listingResponse.getEstimatedCompletionTime());
    Assertions.assertEquals(bulkProcess.getCreatedDate(), listingResponse.getUploadDate());
    Assertions.assertEquals(bulkProcess.getCreatedBy(), listingResponse.getUser());
    Assertions.assertEquals(bulkProcess.getUploadedFile(), listingResponse.getUploadedFileName());
  }

  @Test
  public void testToBulkProcessStatusListingResponse_emptyResponse() throws JsonProcessingException,
    ParseException {
    List<BulkProcess> bulkProcessList = Collections.emptyList();
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcessList, PageRequest.of(0, 1), 1L);
    List<BulkProcessStatusListingResponse> bulkProcessStatusListingResponses =
      RequestHelper.toBulkProcessStatusListingResponse(bulkProcessPage, new HashSet<>());
    Assertions.assertEquals(0, bulkProcessStatusListingResponses.size());
  }

  @Test
  public void testToBulkProcessStatusListingResponse_NullResponse() throws JsonProcessingException,
    ParseException {
    List<BulkProcess> bulkProcessList = Collections.emptyList();
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcessList, PageRequest.of(0, 1), 1L);
    List<BulkProcessStatusListingResponse> bulkProcessStatusListingResponses =
      RequestHelper.toBulkProcessStatusListingResponse(null, new HashSet<>());
    Assertions.assertEquals(0, bulkProcessStatusListingResponses.size());
  }

  @Test
  public void testToBulkProcessStatusListingResponse_WithDescription()
    throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue());
    bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    bulkProcess.setTotalCount(100);
    bulkProcess.setSuccessCount(100);
    bulkProcess.setErrorCount(0);
    bulkProcess.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkProcess.setCreatedDate(new Date());
    bulkProcess.setCreatedBy(DEFAULT_USERNAME);
    bulkProcess.setUploadedFile("File1");
    bulkProcess.setDescription(DEFAULT_DESCRIPTION);
    RequestHelper.setFileStorageService(fileStorageService);
    List<BulkProcess> bulkProcessList = Collections.singletonList(bulkProcess);
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcessList, PageRequest.of(0, 10), 1L);
    List<BulkProcessStatusListingResponse> result = RequestHelper.toBulkProcessStatusListingResponse(bulkProcessPage,
        new HashSet<>());
    Assertions.assertEquals(bulkProcessList.size(), result.size());
    BulkProcessStatusListingResponse listingResponse = result.get(0);
    Assertions.assertEquals(DEFAULT_DESCRIPTION, listingResponse.getDescription());
  }

  @Test
  public void testToBulkProcessStatusListingResponse_Exception() throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    String ERROR_MESSAGE = "System/data are in invalid state :INVALID_STATE";
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT.getValue());
    bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    bulkProcess.setTotalCount(100);
    bulkProcess.setSuccessCount(120);
    List<BulkProcess> bulkProcessList = Collections.singletonList(bulkProcess);
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcessList, PageRequest.of(0, 10), 1L);
    Exception exception = Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
      RequestHelper.toBulkProcessStatusListingResponse(bulkProcessPage, new HashSet<>());
    });
    Assertions.assertEquals(ERROR_MESSAGE, exception.getMessage());
  }

  @Test
  public void testToBulkProcessStatusListingResponse_OnlyTotalCountZero() throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT.getValue());
    bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    bulkProcess.setTotalCount(0);
    bulkProcess.setErrorCount(0);
    List<BulkProcess> bulkProcessList = Collections.singletonList(bulkProcess);
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcessList, PageRequest.of(0, 10), 1L);
    List<BulkProcessStatusListingResponse> bulkProcessStatusListingResponses =
      RequestHelper.toBulkProcessStatusListingResponse(bulkProcessPage, new HashSet<>());
    Assertions.assertEquals(0, bulkProcessStatusListingResponses.get(0).getProcessCompletionPercentage(), 0);
  }

  @Test
  public void testToBulkProcessStatusListingResponse_OnlySuccessCountZero() throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT.getValue());
    bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    bulkProcess.setTotalCount(100);
    bulkProcess.setSuccessCount(0);
    bulkProcess.setErrorCount(100);
    List<BulkProcess> bulkProcessList = Collections.singletonList(bulkProcess);
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcessList, PageRequest.of(0, 10), 1L);
    List<BulkProcessStatusListingResponse> bulkProcessStatusListingResponses =
      RequestHelper.toBulkProcessStatusListingResponse(bulkProcessPage, new HashSet<>());
    Assertions.assertEquals(0, bulkProcessStatusListingResponses.get(0).getProcessCompletionPercentage(), 0);
  }

  @Test
  public void testToBulkProcessStatusListingResponse_BothZeroCounts() throws JsonProcessingException, ParseException {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    bulkProcess.setBulkProcessType(BulkProcessType.INSTANT_PICKUP_PRODUCT.getValue());
    bulkProcess.setStatus(BulkProcess.STATUS_FINISHED);
    bulkProcess.setTotalCount(0);
    bulkProcess.setSuccessCount(0);
    bulkProcess.setErrorCount(100);
    List<BulkProcess> bulkProcessList = Collections.singletonList(bulkProcess);
    Page<BulkProcess> bulkProcessPage = new PageImpl<>(bulkProcessList, PageRequest.of(0, 10), 1L);
    List<BulkProcessStatusListingResponse> bulkProcessStatusListingResponses =
      RequestHelper.toBulkProcessStatusListingResponse(bulkProcessPage, new HashSet<>());
    Assertions.assertEquals(0, bulkProcessStatusListingResponses.get(0).getProcessCompletionPercentage(), 0);
  }

  @Test
  public void downloadEntityToBulkProcessStatusListingResponseTest() throws Exception {
    BulkDownloadEntity bulkDownloadEntity = new BulkDownloadEntity();
    bulkDownloadEntity.setEntityType(BulkProcessEntity.CAMPAIGN_PRODUCT.name());
    bulkDownloadEntity.setPrimaryIdentifier("PRIMARY_IDENTIFIER");
    bulkDownloadEntity.setStatus("SUCCESS");
    bulkDownloadEntity.setRequestBody(null);
    bulkDownloadEntity.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkDownloadEntity.setRecordsDownload(1);
    bulkDownloadEntity.setMarkForDelete(false);
    List<BulkDownloadEntity> bulkDownloadEntityList = Collections.singletonList(bulkDownloadEntity);
    Page<BulkDownloadEntity> bulkDownloadEntityPage =
        new PageImpl<BulkDownloadEntity>(bulkDownloadEntityList, PageRequest.of(0, 10), 1L);
    List<BulkProcessStatusListingResponse> result =
        RequestHelper.downloadEntityToBulkProcessStatusListingResponse(bulkDownloadEntityPage);
    Assertions.assertEquals(bulkDownloadEntityList.size(), result.size());
  }

  @Test
  public void downloadEntityToBulkProcessStatusListingResponseWithEntityTypeAbortedTest() throws Exception {
    BulkDownloadEntity bulkDownloadEntity = new BulkDownloadEntity();
    bulkDownloadEntity.setEntityType(BulkProcessEntity.CAMPAIGN_PRODUCT.name());
    bulkDownloadEntity.setPrimaryIdentifier("PRIMARY_IDENTIFIER");
    bulkDownloadEntity.setStatus(BulkProcess.STATUS_ABORTED);
    bulkDownloadEntity.setRequestBody(null);
    bulkDownloadEntity.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkDownloadEntity.setRecordsDownload(1);
    bulkDownloadEntity.setMarkForDelete(false);
    List<BulkDownloadEntity> bulkDownloadEntityList = Collections.singletonList(bulkDownloadEntity);
    Page<BulkDownloadEntity> bulkDownloadEntityPage =
        new PageImpl<BulkDownloadEntity>(bulkDownloadEntityList, PageRequest.of(0, 10), 1L);
    List<BulkProcessStatusListingResponse> result =
        RequestHelper.downloadEntityToBulkProcessStatusListingResponse(bulkDownloadEntityPage);
    Assertions.assertEquals(bulkDownloadEntityList.size(), result.size());
  }

  @Test
  public void downloadEntityToBulkProcessStatusListingResponseWithEntityTypeInprogressTest() throws Exception {
    BulkDownloadEntity bulkDownloadEntity = new BulkDownloadEntity();
    bulkDownloadEntity.setEntityType(BulkProcessEntity.CAMPAIGN_PRODUCT.name());
    bulkDownloadEntity.setPrimaryIdentifier("PRIMARY_IDENTIFIER");
    bulkDownloadEntity.setStatus(BulkProcess.STATUS_IN_PROGRESS);
    bulkDownloadEntity.setRequestBody(null);
    bulkDownloadEntity.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkDownloadEntity.setRecordsDownload(1);
    bulkDownloadEntity.setMarkForDelete(false);
    List<BulkDownloadEntity> bulkDownloadEntityList = Collections.singletonList(bulkDownloadEntity);
    Page<BulkDownloadEntity> bulkDownloadEntityPage =
        new PageImpl<BulkDownloadEntity>(bulkDownloadEntityList, PageRequest.of(0, 10), 1L);
    List<BulkProcessStatusListingResponse> result =
        RequestHelper.downloadEntityToBulkProcessStatusListingResponse(bulkDownloadEntityPage);
    Assertions.assertEquals(bulkDownloadEntityList.size(), result.size());
  }

  @Test
  public void downloadEntityToBulkProcessStatusListingResponseWithEntityTypeDownloadFailed() throws Exception {
    BulkDownloadEntity bulkDownloadEntity = new BulkDownloadEntity();
    bulkDownloadEntity.setEntityType(BulkProcessEntity.CAMPAIGN_PRODUCT.name());
    bulkDownloadEntity.setPrimaryIdentifier("PRIMARY_IDENTIFIER");
    bulkDownloadEntity.setStatus(BulkProcessConstant.DOWNLOAD_FAILED);
    bulkDownloadEntity.setRequestBody(null);
    bulkDownloadEntity.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    bulkDownloadEntity.setRecordsDownload(1);
    bulkDownloadEntity.setMarkForDelete(false);
    List<BulkDownloadEntity> bulkDownloadEntityList = Collections.singletonList(bulkDownloadEntity);
    Page<BulkDownloadEntity> bulkDownloadEntityPage =
        new PageImpl<BulkDownloadEntity>(bulkDownloadEntityList, PageRequest.of(0, 10), 1L);
    List<BulkProcessStatusListingResponse> result =
        RequestHelper.downloadEntityToBulkProcessStatusListingResponse(bulkDownloadEntityPage);
    Assertions.assertEquals(bulkDownloadEntityList.size(), result.size());
  }

  @Test
  public void downloadEntityToBulkProcessStatusListingResponsewithNullPageTest() throws Exception {
    List<BulkDownloadEntity> bulkDownloadEntityList = new ArrayList<>();
    Page<BulkDownloadEntity> bulkDownloadEntityPage =
        new PageImpl<BulkDownloadEntity>(bulkDownloadEntityList, PageRequest.of(0, 10), 1L);
    List<BulkProcessStatusListingResponse> result =
        RequestHelper.downloadEntityToBulkProcessStatusListingResponse(bulkDownloadEntityPage);
    Assertions.assertEquals(bulkDownloadEntityList.size(), result.size());
  }

  @Test
  public void downloadEntityToBulkProcessStatusListingResponsewithEmptyPageTest() throws Exception {
    List<BulkDownloadEntity> bulkDownloadEntityList = new ArrayList<>();
    Page<BulkDownloadEntity> bulkDownloadEntityPage = null;
    List<BulkProcessStatusListingResponse> result =
        RequestHelper.downloadEntityToBulkProcessStatusListingResponse(bulkDownloadEntityPage);
    Assertions.assertEquals(bulkDownloadEntityList.size(), result.size());
  }


  @Test
  public void toBrandAuthCreateRequestTest() {
    BrandAuthAddRequestData brandAuthAddRequestData =
        BrandAuthAddRequestData.builder().brandCode(INTERNAL_PROCESS_CODE).sellerCode(INTERNAL_PROCESS_CODE).build();
    BrandAuthCreateRequest brandAuthCreateRequest = RequestHelper.toBrandAuthCreateRequest(brandAuthAddRequestData);
    Assertions.assertEquals(INTERNAL_PROCESS_CODE, brandAuthCreateRequest.getBrandCode());
    Assertions.assertEquals(INTERNAL_PROCESS_CODE, brandAuthAddRequestData.getSellerCode());
    Assertions.assertTrue(brandAuthCreateRequest.isBulkAction());
  }

  @Test
  public void toBrandAuthDeleteRequestTest() {
    BrandAuthDeleteRequestData brandAuthDeleteRequestData =
        BrandAuthDeleteRequestData.builder().brandCode(INTERNAL_PROCESS_CODE).sellerCode(INTERNAL_PROCESS_CODE).build();
    BrandAuthDeleteRequest brandAuthDeleteRequest = RequestHelper.toBrandAuthDeleteRequest(brandAuthDeleteRequestData);
    Assertions.assertEquals(INTERNAL_PROCESS_CODE, brandAuthDeleteRequest.getBrandCode());
    Assertions.assertEquals(INTERNAL_PROCESS_CODE, brandAuthDeleteRequest.getSellerCode());
  }

  @Test
  public void toBrandAuthFilterRequestTest() {
    BrandAuthDownloadRequest brandAuthDownloadRequest = new BrandAuthDownloadRequest(MERCHANT_CODE, BRAND_NAME, STATUS);
    BrandAuthFilterRequest brandAuthFilterRequest = RequestHelper.toBrandAuthFilterRequest(brandAuthDownloadRequest);
    Assertions.assertEquals(MERCHANT_CODE, brandAuthFilterRequest.getSellerCode());
    Assertions.assertEquals(BRAND_NAME, brandAuthFilterRequest.getBrandName());
    Assertions.assertEquals(STATUS, brandAuthFilterRequest.getStatus());
  }

  @Test
  public void getBulkInternalProcessFromBulkReviewUploadRequestTest() {
    BulkReviewUploadModel bulkReviewUploadModel =
            BulkReviewUploadModel.builder().bulkProcessCode(INTERNAL_PROCESS_CODE)
                    .bulkProcessType(BulkInternalProcessType.BULK_APPROVAL.name()).filePath(STORE_COPY).build();
    BulkInternalProcess bulkInternalProcess =
            RequestHelper.getBulkInternalProcessFromBulkReviewUploadRequest(STORE_ID, bulkReviewUploadModel);
    Assertions.assertEquals(ProcessStatus.PENDING.name(), bulkInternalProcess.getStatus());
    Assertions.assertEquals(INTERNAL_PROCESS_CODE, bulkInternalProcess.getInternalProcessRequestCode());
    Assertions.assertEquals(BulkInternalProcessType.BULK_APPROVAL.name(), bulkInternalProcess.getProcessType());
    Assertions.assertEquals(STORE_COPY, bulkInternalProcess.getFileName());
  }

  @Test
  public void toVendorRejectRequestProductTest() {
    String REASON = Constant.PRODUK.concat(":").concat("Upload dua kali");
    bulkVendorActionsModel.setReason(REASON);
    RejectProductVendorRequest request = RequestHelper.toVendorRejectRequest(bulkVendorActionsModel);
    Assertions.assertEquals(request.getRejectReasonRequest().getProduct().get(0), "Upload dua kali");
  }

  @Test
  public void toVendorRejectRequestContentTest() {
    String REASON = Constant.KONTEN.concat(":").concat("Upload dua kali");
    bulkVendorActionsModel.setReason(REASON);
    RejectProductVendorRequest request = RequestHelper.toVendorRejectRequest(bulkVendorActionsModel);
    Assertions.assertEquals(request.getRejectReasonRequest().getContent().get(0), "Upload dua kali");
  }

  @Test
  public void toVendorRejectRequestPhotoTest() {
    String REASON = Constant.FOTO.concat(":").concat("Upload dua kali");
    bulkVendorActionsModel.setReason(REASON);
    RejectProductVendorRequest request = RequestHelper.toVendorRejectRequest(bulkVendorActionsModel);
    Assertions.assertEquals(request.getRejectReasonRequest().getImage().get(0), "Upload dua kali");
  }

  @Test
  public void toVendorQuickApprovalRequestTest() {
    BulkApprovalRejectionRequestData bulkVendorActionsModel = new BulkApprovalRejectionRequestData();
    bulkVendorActionsModel.setProductCode(PICKUP_POINT_CODE);
    VendorQuickApprovalRequest vendorQuickApprovalRequest = RequestHelper.toVendorQuickApprovalRequest(bulkVendorActionsModel
            , "username");
    Assertions.assertEquals(vendorQuickApprovalRequest.getProductCode(),PICKUP_POINT_CODE);
  }

  @Test
  public void toVendorQuickApprovalRequestWithCommentTest() {
    BulkApprovalRejectionRequestData bulkVendorActionsModel = new BulkApprovalRejectionRequestData();
    bulkVendorActionsModel.setProductCode(PICKUP_POINT_CODE);
    bulkVendorActionsModel.setReason(STRAIGHT_REJECTION);
    VendorQuickApprovalRequest vendorQuickApprovalRequest = RequestHelper.toVendorQuickApprovalRequest(bulkVendorActionsModel
            , "username");
    Assertions.assertEquals(PICKUP_POINT_CODE, vendorQuickApprovalRequest.getProductCode());
    Assertions.assertEquals(STRAIGHT_REJECTION, vendorQuickApprovalRequest.getNotes());
  }

  @Test
  public void getDefaultChannelItemViewConfigTest() {
    ItemPickupPointListingL3Response response = new ItemPickupPointListingL3Response();
    ProductLevel3ViewConfigResponse b2CviewConfigResponse = new ProductLevel3ViewConfigResponse();
    b2CviewConfigResponse.setChannelId(Constant.DEFAULT);
    b2CviewConfigResponse.setDisplay(Boolean.TRUE);
    b2CviewConfigResponse.setBuyable(Boolean.TRUE);

    ProductLevel3ViewConfigResponse b2BviewConfigResponse = new ProductLevel3ViewConfigResponse();
    b2BviewConfigResponse.setChannelId(Constant.B2B_CHANNEL);
    b2BviewConfigResponse.setDisplay(Boolean.FALSE);
    b2BviewConfigResponse.setBuyable(Boolean.FALSE);

    response.setViewConfigs(Arrays.asList(b2BviewConfigResponse, b2CviewConfigResponse));
    ProductLevel3ViewConfigResponse result =
      RequestHelper.getDefaultChannelItemViewConfig(response);
    Assertions.assertEquals(Constant.DEFAULT, result.getChannelId());
    assertNotEquals(Constant.B2B_CHANNEL, result.getChannelId());
  }


  @Test
  public void getCncChannelItemViewConfigTest() {
    ItemPickupPointListingL3Response response = new ItemPickupPointListingL3Response();
    ProductLevel3ViewConfigResponse b2CviewConfigResponse = new ProductLevel3ViewConfigResponse();
    b2CviewConfigResponse.setChannelId(Constant.DEFAULT);
    b2CviewConfigResponse.setDisplay(Boolean.FALSE);
    b2CviewConfigResponse.setBuyable(Boolean.FALSE);

    ProductLevel3ViewConfigResponse cncViewConfigResponse = new ProductLevel3ViewConfigResponse();
    cncViewConfigResponse.setChannelId(Constant.CNC);
    cncViewConfigResponse.setDisplay(Boolean.TRUE);
    cncViewConfigResponse.setBuyable(Boolean.TRUE);

    response.setViewConfigs(Arrays.asList(cncViewConfigResponse, b2CviewConfigResponse));
    ProductLevel3ViewConfigResponse result =
        RequestHelper.getCncChannelItemViewConfig(response);
    Assertions.assertNotEquals(Constant.DEFAULT, result.getChannelId());
    Assertions.assertEquals(Constant.CNC, result.getChannelId());
  }

  @Test
  public void getB2BChannelItemViewConfigTest() {
    ItemPickupPointListingL3Response response = new ItemPickupPointListingL3Response();
    ProductLevel3ViewConfigResponse b2CviewConfigResponse = new ProductLevel3ViewConfigResponse();
    b2CviewConfigResponse.setChannelId(Constant.DEFAULT);
    b2CviewConfigResponse.setDisplay(Boolean.TRUE);
    b2CviewConfigResponse.setBuyable(Boolean.TRUE);

    ProductLevel3ViewConfigResponse b2BviewConfigResponse = new ProductLevel3ViewConfigResponse();
    b2BviewConfigResponse.setChannelId(Constant.B2B_CHANNEL);
    b2BviewConfigResponse.setDisplay(Boolean.FALSE);
    b2BviewConfigResponse.setBuyable(Boolean.FALSE);

    response.setViewConfigs(Arrays.asList(b2BviewConfigResponse, b2CviewConfigResponse));
    ProductLevel3ViewConfigResponse result =
      RequestHelper.getBfbChannelItemViewConfig(response);
    Assertions.assertEquals(Constant.B2B_CHANNEL, result.getChannelId());
  }

  @Test
  public void getSimpleListAssemblyDisassemblyRequestTest() {
    ItemBasicDetailV2Response itemBasicDetailV2Response = new ItemBasicDetailV2Response();
    itemBasicDetailV2Response.setBundleRecipeList(Collections.singletonList(new BundleRecipeV2Response()));
    SimpleListAssemblyDisassemblyRequest response =
        RequestHelper.getSimpleListAssemblyDisassemblyRequest(PICKUP_POINT_CODE, 0, PICKUP_POINT_CODE, MERCHANT_CODE,
            MERCHANT_CODE, ITEM_SKU, itemBasicDetailV2Response, new HashMap<>());
    Assertions.assertNotNull(response);
    itemBasicDetailV2Response.setBundleRecipeList(new ArrayList<>());
    response =
        RequestHelper.getSimpleListAssemblyDisassemblyRequest(PICKUP_POINT_CODE, 0, PICKUP_POINT_CODE, MERCHANT_CODE,
            MERCHANT_CODE, ITEM_SKU, itemBasicDetailV2Response, new HashMap<>());
    Assertions.assertNotNull(response);
  }

  @Test
  public void getTransferRequestTest() {
    WorkOrderDataModel workOrderDataModel = new WorkOrderDataModel();
    workOrderDataModel.setStock("1");
    TransferRequest response =
        RequestHelper.getTransferRequest(PICKUP_POINT_CODE, PICKUP_POINT_CODE, new ItemBasicDetailV2Response(),
            new ItemBasicDetailV2Response(), MERCHANT_CODE, ITEM_SKU, PICKUP_POINT_CODE, workOrderDataModel);
    Assertions.assertNotNull(response);
  }

  @Test
  public void getBulkInternalProcessDataForBulkPriceUpdateMaxRowsTest() throws JsonProcessingException {
    bulkInternalProcessDataGenerationDTO.setBulkPriceUpdateMaxRows(-1);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkPriceUpdate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(0, bulkInternalProcessDataList.size());
  }

  @Test
  public void getBulkInternalProcessDataForBulkPriceUpdateValidTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.BLIBLI_PRODUCT_SKU, PRODUCT_CODE);
    excelData.put(BulkParameters.BLIBLI_SKU, PRODUCT_CODE);
    excelData.put(BulkParameters.PICKUP_POINT_HEADER, PRODUCT_CODE);
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Arrays.asList(excelData));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkPriceUpdate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
  }

  @Test
  public void getBulkInternalProcessDataForBulkPriceUpdateInValidTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.PRODUCT_SKU, PRODUCT_CODE);
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Arrays.asList(excelData));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkPriceUpdate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
  }

  @Test
  public void getBulkInternalProcessDataForBulkPriceUpdateStockInValidTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.STOCK, PRODUCT_CODE);
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Arrays.asList(excelData));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkPriceUpdate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
  }

  @Test
  public void getBulkInternalProcessDataForBulkPriceUpdateInstoreTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.SELLER_SKU, PRODUCT_CODE);
    excelData.put(BulkParameters.IN_STORE_HEADER, "0");
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Arrays.asList(excelData));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkPriceUpdate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
  }

  @Test
  public void getBulkInternalProcessDataForBulkPriceUpdateInvalidSellerSkuTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.SELLER_SKU, "9092");
    excelData.put(BulkParameters.AMPHI_SKU_STATUS, "0");
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    excelData.put(BulkParameters.SELLER_SKU, new String(new char[300]).replace('\0', '1'));
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Arrays.asList(excelData));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkPriceUpdate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
  }

  @Test
  public void getBulkInternalProcessDataForBulkPriceUpdateValidStatusTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.AMPHI_SKU_STATUS, "1");
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    excelData.put(BulkParameters.WAREHOUSE_STOCK_HEADER, Constant.HYPHEN);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Arrays.asList(excelData));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkPriceUpdate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
  }

  @Test
  public void getBulkInternalProcessDataForBulkPriceUpdateValidWarehouseStockTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.AMPHI_SKU_STATUS, PRODUCT_CODE);
    excelData.put(BulkParameters.WAREHOUSE_STOCK_HEADER, STORE_ID);
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Arrays.asList(excelData));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkPriceUpdate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
  }

  @Test
  public void getBulkInternalProcessDataForBulkPriceUpdateAllValidTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.AMPHI_SKU_STATUS, "1");
    excelData.put(BulkParameters.WAREHOUSE_STOCK_HEADER, STORE_ID);
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Arrays.asList(excelData));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkPriceUpdate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
  }

  @Test
  public void getBulkInternalProcessDataForBulkPriceUpdateWarehouseInvalidTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.WAREHOUSE_STOCK_HEADER, PRODUCT_CODE);
    excelData.put(BulkParameters.SELLING_PRICE_HEADER, STORE_ID);
    excelData.put(BulkParameters.PRICE_HEADER, STORE_ID);
    excelData.put(BulkParameters.CNC_STATUS_HEADER, "1");
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Arrays.asList(excelData));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkPriceUpdate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
  }

  @Test
  public void getBulkInternalProcessDataForBulkPriceUpdateNullTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.SALE_PRICE, null);
    excelData.put(BulkParameters.PRICE_HEADER, null);
    excelData.put(BulkParameters.STOCK_HEADER, "");
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Arrays.asList(excelData));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkPriceUpdate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
  }

  @Test
  public void getBulkInternalProcessDataForBulkPriceUpdateCNCTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.SELLING_PRICE_HEADER, STORE_ID);
    excelData.put(BulkParameters.PRICE_HEADER, STORE_ID);
    excelData.put(BulkParameters.CNC_STATUS_HEADER, "1");
    excelData.put(BulkParameters.STOCK_HEADER, "1");
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Arrays.asList(excelData));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkPriceUpdate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
  }

  @Test
  public void getBulkInternalProcessDataForBulkPriceUpdateCNCInvalidTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.SELLING_PRICE_HEADER, STORE_ID);
    excelData.put(BulkParameters.PRICE_HEADER, STORE_ID);
    excelData.put(BulkParameters.CNC_STATUS_HEADER, PRODUCT_CODE);
    excelData.put(BulkParameters.STOCK_HEADER, "1");
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Arrays.asList(excelData));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkPriceUpdate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
  }

  @Test
  public void getBulkInternalProcessDataForBulkPriceUpdateEmptyCNCTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.SELLING_PRICE_HEADER, STORE_ID);
    excelData.put(BulkParameters.PRICE_HEADER, STORE_ID);
    excelData.put(BulkParameters.CNC_STATUS_HEADER, StringUtils.EMPTY);
    excelData.put(BulkParameters.STOCK_HEADER, "1");
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Arrays.asList(excelData));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkPriceUpdate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
  }

  @Test
  public void getBulkInternalProcessDataForBulkPriceUpdatePoQuotaTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.SELLING_PRICE_HEADER, STORE_ID);
    excelData.put(BulkParameters.PRICE_HEADER, STORE_ID);
    excelData.put(BulkParameters.CNC_STATUS_HEADER, StringUtils.EMPTY);
    excelData.put(BulkParameters.STOCK_HEADER, "1");
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    excelData.put(BulkParameters.PO_QUOTA, "12");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Arrays.asList(excelData));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkPriceUpdate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
  }

  @Test
  public void getBulkInternalProcessDataForBulkPriceRebateTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.MONTH, "April");
    excelData.put(BulkParameters.YEAR, "2024");
    excelData.put(BulkParameters.MAIN_CATEGORY_CODE, CATEGORY_CHANGE);
    excelData.put(BulkParameters.CATEGORY_CODE, CATEGORY_NAME);
    excelData.put(BulkParameters.PROJECTED_REBATE, "1000");
    excelData.put(BulkParameters.STORE_ID, STORE_ID);
    excelData.put(BulkParameters.BRAND, BRAND_NAME);
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Collections.singletonList(excelData));
    bulkInternalProcessDataGenerationDTO.setBulkRebateMaxRows(2);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkPriceRebate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void getBulkInternalProcessDataForBulkPriceRebateExceedMaxLimitTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.MONTH, "April");
    excelData.put(BulkParameters.YEAR, "2024");
    excelData.put(BulkParameters.MAIN_CATEGORY_CODE, CATEGORY_CHANGE);
    excelData.put(BulkParameters.CATEGORY_CODE, CATEGORY_NAME);
    excelData.put(BulkParameters.PROJECTED_REBATE, "1000");
    excelData.put(BulkParameters.STORE_ID, STORE_ID);
    excelData.put(BulkParameters.BRAND, BRAND_NAME);
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Collections.singletonList(excelData));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkPriceRebate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(0, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(), bulkInternalProcess.getStatus());
  }

  @Test
  public void getBulkInternalProcessDataForBulkPriceRebateInvalidDataTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.MONTH, "April2");
    excelData.put(BulkParameters.YEAR, "year");
    excelData.put(BulkParameters.MAIN_CATEGORY_CODE, CATEGORY_CHANGE);
    excelData.put(BulkParameters.CATEGORY_CODE, CATEGORY_NAME);
    excelData.put(BulkParameters.PROJECTED_REBATE, "twok");
    excelData.put(BulkParameters.STORE_ID, STORE_ID);
    excelData.put(BulkParameters.BRAND, BRAND_NAME);
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Collections.singletonList(excelData));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setBulkRebateMaxRows(2);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkPriceRebate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertNotNull(bulkInternalProcessDataList.get(0).getNotes());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void getBulkInternalProcessDataForBulkPriceRebateDuplicateDataTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.MONTH, "April2");
    excelData.put(BulkParameters.YEAR, "year");
    excelData.put(BulkParameters.MAIN_CATEGORY_CODE, CATEGORY_CHANGE);
    excelData.put(BulkParameters.CATEGORY_CODE, CATEGORY_NAME);
    excelData.put(BulkParameters.PROJECTED_REBATE, "twok");
    excelData.put(BulkParameters.STORE_ID, STORE_ID);
    excelData.put(BulkParameters.BRAND, BRAND_NAME);
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    Map<String, String> excelData2 = new HashMap<>();
    excelData2.put(BulkParameters.MONTH, "April2");
    excelData2.put(BulkParameters.YEAR, "year");
    excelData2.put(BulkParameters.MAIN_CATEGORY_CODE, CATEGORY_CHANGE);
    excelData2.put(BulkParameters.CATEGORY_CODE, CATEGORY_NAME);
    excelData2.put(BulkParameters.PROJECTED_REBATE, "twok");
    excelData2.put(BulkParameters.STORE_ID, STORE_ID);
    excelData2.put(BulkParameters.BRAND, BRAND_NAME);
    excelData2.put(BulkParameters.ROW_NUMBER, "2");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Arrays.asList(excelData, excelData2));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setBulkRebateMaxRows(2);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkPriceRebate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(2, bulkInternalProcessDataList.size());
    Assertions.assertNotNull(bulkInternalProcessDataList.get(0).getNotes());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(Constant.DUPLICATE_ROW_REBATE_ERROR_MESSAGE,
        bulkInternalProcessDataList.get(0).getErrorMessage());
  }

  @Test
  public void getBulkInternalProcessDataForBulkPriceRebateInvalidDataTest2() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.MONTH, "");
    excelData.put(BulkParameters.YEAR, "");
    excelData.put(BulkParameters.MAIN_CATEGORY_CODE, "");
    excelData.put(BulkParameters.CATEGORY_CODE, "");
    excelData.put(BulkParameters.PROJECTED_REBATE, "twok");
    excelData.put(BulkParameters.STORE_ID, STORE_ID);
    excelData.put(BulkParameters.BRAND, "");
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Collections.singletonList(excelData));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setBulkRebateMaxRows(2);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkPriceRebate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertNotNull(bulkInternalProcessDataList.get(0).getNotes());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void getBulkInternalProcessDataForBulkSkuLevelRebateTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.SKU_LEVEL_REBATE_SKU_ID, "TEB-24219-00001-00001");
    excelData.put(BulkParameters.SKU_LEVEL_REBATE_PICKUP_POINT_CODE, "PP-123456");
    excelData.put(BulkParameters.SKU_LEVEL_REBATE_NEW_REBATE_VALUE, "1000");
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Collections.singletonList(excelData));
    bulkInternalProcessDataGenerationDTO.setBulkSkuLevelRebateMaxRows(2);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkSkuLevelRebate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void getBulkInternalProcessDataForBulkSkuLevelRebateExceedMaxLimitTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.SKU_LEVEL_REBATE_SKU_ID, "TEB-24219-00001-00001");
    excelData.put(BulkParameters.SKU_LEVEL_REBATE_PICKUP_POINT_CODE, "PP-123456");
    excelData.put(BulkParameters.SKU_LEVEL_REBATE_NEW_REBATE_VALUE, "1000");
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Collections.singletonList(excelData));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkSkuLevelRebate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(0, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(), bulkInternalProcess.getStatus());
  }

  @Test
  public void getBulkInternalProcessDataForBulkSkuLevelRebateInvalidDataTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.SKU_LEVEL_REBATE_SKU_ID, ITEM_SKU);
    excelData.put(BulkParameters.SKU_LEVEL_REBATE_PICKUP_POINT_CODE, "PP-123456");
    excelData.put(BulkParameters.SKU_LEVEL_REBATE_NEW_REBATE_VALUE, "1000");
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    Map<String, String> excelData2 = new HashMap<>();
    excelData2.put(BulkParameters.SKU_LEVEL_REBATE_SKU_ID, "TEB-24219-00001-00001");
    excelData2.put(BulkParameters.SKU_LEVEL_REBATE_PICKUP_POINT_CODE, "");
    excelData2.put(BulkParameters.SKU_LEVEL_REBATE_NEW_REBATE_VALUE, "1000");
    excelData2.put(BulkParameters.ROW_NUMBER, "2");
    Map<String, String> excelData3 = new HashMap<>();
    excelData3.put(BulkParameters.SKU_LEVEL_REBATE_SKU_ID, "TEB-24219-00001-00001");
    excelData3.put(BulkParameters.SKU_LEVEL_REBATE_PICKUP_POINT_CODE, "PP-123456");
    excelData3.put(BulkParameters.SKU_LEVEL_REBATE_NEW_REBATE_VALUE, "");
    excelData3.put(BulkParameters.ROW_NUMBER, "3");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Arrays.asList(excelData, excelData2, excelData3));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setBulkSkuLevelRebateMaxRows(5);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkSkuLevelRebate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(3, bulkInternalProcessDataList.size());
    Assertions.assertNotNull(bulkInternalProcessDataList.get(0).getNotes());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void getBulkInternalProcessDataForBulkSkuLevelRebateDuplicateDataTest() throws JsonProcessingException {
    Map<String, String> excelData = new HashMap<>();
    excelData.put(BulkParameters.SKU_LEVEL_REBATE_SKU_ID, "TEB-24219-00001-00001");
    excelData.put(BulkParameters.SKU_LEVEL_REBATE_PICKUP_POINT_CODE, "PP-123456");
    excelData.put(BulkParameters.SKU_LEVEL_REBATE_NEW_REBATE_VALUE, "1000");
    excelData.put(BulkParameters.ROW_NUMBER, "1");
    Map<String, String> excelData2 = new HashMap<>();
    excelData2.put(BulkParameters.SKU_LEVEL_REBATE_SKU_ID, "TEB-24219-00001-00001");
    excelData2.put(BulkParameters.SKU_LEVEL_REBATE_PICKUP_POINT_CODE, "PP-123456");
    excelData2.put(BulkParameters.SKU_LEVEL_REBATE_NEW_REBATE_VALUE, "5000");
    excelData2.put(BulkParameters.ROW_NUMBER, "2");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Arrays.asList(excelData, excelData2));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setBulkSkuLevelRebateMaxRows(2);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
        RequestHelper.getBulkInternalProcessDataForBulkSkuLevelRebate(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(2, bulkInternalProcessDataList.size());
    Assertions.assertNotNull(bulkInternalProcessDataList.get(0).getNotes());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
//    Assertions.assertEquals(BulkProcessValidationErrorMessages.SKU_LEVEL_REBATE_DUPLICATE_ROW_ERROR,
//        bulkInternalProcessDataList.get(0).getErrorMessage());
  }

  @Test
  public void toBulkRebateUpdateRequestTest() {
    BulkRebateUpdateRequest bulkRebateUpdateRequest = RequestHelper.toBulkRebateUpdateRequest(
        new BulkPriceRebateRequestData("April", "2024", "storeId", "brandName", "mainCategoryCode", "categoryCode",
            "200", 2));
    Assertions.assertEquals(200.0, bulkRebateUpdateRequest.getRebate(), 0);
  }

  @Test
  public void toIprActionRequestTest() {
    IprActionRequest iprActionRequest = RequestHelper.toIprActionRequest(
        new BulkAddReviewIPRProductsRequestData(PRODUCT_SKU, PRODUCT_NAME,
            BulkIPRProductsParameter.REVIEW, SELLER_NOTES, REVIEWER_NOTES, ASSIGNEE, REASON_VALUE, VIOLATION_TYPE_VALUE,
            BulkIPRProductsParameter.CUSTOMER_REPORT, 1, REPORT_DATE, REPORTER, REPORTER_NAME,
            REPORTER_PHONE_NUMBER, REPORTER_EMAIL, REPORTER_ADDRESS, REPORTER_REASON),
        DEFAULT_USERNAME);
    Assertions.assertEquals(ProductStateIPR.IN_REVIEW.name(), iprActionRequest.getAction());
  }

  @Test
  public void toIprActionRequestWhiteListTest() {
    IprActionRequest iprActionRequest = RequestHelper.toIprActionRequest(
        new BulkAddReviewIPRProductsRequestData(PRODUCT_SKU, PRODUCT_NAME,
            BulkIPRProductsParameter.WHITELIST, SELLER_NOTES, REVIEWER_NOTES, ASSIGNEE, REASON_VALUE, VIOLATION_TYPE_VALUE,
            BRAND_REPORT, 1, REPORT_DATE, REPORTER, REPORTER_NAME,
            REPORTER_PHONE_NUMBER, REPORTER_EMAIL, REPORTER_ADDRESS, REPORTER_REASON),
        DEFAULT_USERNAME);
    Assertions.assertEquals(ProductStateIPR.WHITELISTED.name(), iprActionRequest.getAction());
  }

  @Test
  public void toIprActionRequestSuspendTest() {
    IprActionRequest iprActionRequest = RequestHelper.toIprActionRequest(
        new BulkAddReviewIPRProductsRequestData(PRODUCT_SKU, PRODUCT_NAME,
            BulkIPRProductsParameter.SUSPEND, SELLER_NOTES, REVIEWER_NOTES, ASSIGNEE, REASON_VALUE, VIOLATION_TYPE_VALUE,
            BulkIPRProductsParameter.RANDOM_SAMPLE, 1, REPORT_DATE, REPORTER, REPORTER_NAME,
            REPORTER_PHONE_NUMBER, REPORTER_EMAIL, REPORTER_ADDRESS, REPORTER_REASON),
        DEFAULT_USERNAME);
    Assertions.assertEquals(ProductStateIPR.SUSPENDED.name(), iprActionRequest.getAction());
  }

  @Test
  public void toIprActionRequestReleaseTest() {
    IprActionRequest iprActionRequest = RequestHelper.toIprActionRequest(
        new BulkAddReviewIPRProductsRequestData(PRODUCT_SKU, PRODUCT_NAME,
            BulkIPRProductsParameter.RELEASE, SELLER_NOTES, REVIEWER_NOTES, ASSIGNEE, REASON_VALUE, VIOLATION_TYPE_VALUE,
            BulkIPRProductsParameter.RANDOM_SAMPLE, 1, REPORT_DATE, REPORTER, REPORTER_NAME,
            REPORTER_PHONE_NUMBER, REPORTER_EMAIL, REPORTER_ADDRESS, REPORTER_REASON),
        DEFAULT_USERNAME);
    Assertions.assertEquals(ProductStateIPR.RELEASED.name(), iprActionRequest.getAction());
  }

  @Test
  public void toIprActionRequestEvidenceRequestedTest() {
    IprActionRequest iprActionRequest = RequestHelper.toIprActionRequest(
        new BulkAddReviewIPRProductsRequestData(PRODUCT_SKU, PRODUCT_NAME,
            BulkIPRProductsParameter.REQUEST_EVIDENCE, SELLER_NOTES, REVIEWER_NOTES, ASSIGNEE, REASON_VALUE,
            VIOLATION_TYPE_VALUE, BulkIPRProductsParameter.RANDOM_SAMPLE, 1, REPORT_DATE, REPORTER,
            REPORTER_NAME, REPORTER_PHONE_NUMBER, REPORTER_EMAIL, REPORTER_ADDRESS,
            REPORTER_REASON), DEFAULT_USERNAME);
    Assertions.assertEquals(ProductStateIPR.EVIDENCE_REQUESTED.name(), iprActionRequest.getAction());
  }

  @Test
  public void convertFromDataToExcelHeaderMapTest() {
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    ProductLevel3PriceResponse productLevel3PriceResponse = new ProductLevel3PriceResponse();
    productLevel3PriceResponse.setChannelId(DEFAULT_USERNAME);
    RequestHelper.convertFromDataToExcelHeaderMap(new BulkPriceUpdateRequestData(), new BasicProductResponse(),
        null, null, cncPickupPointCodes);
    itemPickupPointListingL3Response.setViewConfigs(null);
    RequestHelper.convertFromDataToExcelHeaderMap(new BulkPriceUpdateRequestData(), new BasicProductResponse(),
        null, null, cncPickupPointCodes);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    ProductLevel3ViewConfigResponse productLevel3ViewConfigResponseDefault = new ProductLevel3ViewConfigResponse();
    productLevel3ViewConfigResponseDefault.setDisplay(true);
    productLevel3ViewConfigResponseDefault.setBuyable(true);
    productLevel3ViewConfigResponseDefault.setChannelId("DEFAULT");
    ProductLevel3ViewConfigResponse productLevel3ViewConfigResponseCnc = new ProductLevel3ViewConfigResponse();
    productLevel3ViewConfigResponseCnc.setDisplay(true);
    productLevel3ViewConfigResponseCnc.setBuyable(true);
    productLevel3ViewConfigResponseCnc.setChannelId("CNC");
    itemPickupPointListingL3Response.getViewConfigs().add(productLevel3ViewConfigResponseDefault);
    itemPickupPointListingL3Response.getViewConfigs().add(productLevel3ViewConfigResponseCnc);
    RequestHelper.convertFromDataToExcelHeaderMap(new BulkPriceUpdateRequestData(), new BasicProductResponse(),
        itemPickupPointListingL3Response, null, cncPickupPointCodes);

    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    productLevel3ViewConfigResponseDefault.setDisplay(false);
    productLevel3ViewConfigResponseDefault.setBuyable(false);
    productLevel3ViewConfigResponseDefault.setChannelId("DEFAULT");
    productLevel3ViewConfigResponseCnc.setDisplay(true);
    productLevel3ViewConfigResponseCnc.setBuyable(true);
    productLevel3ViewConfigResponseCnc.setChannelId("CNC");
    itemPickupPointListingL3Response.getViewConfigs().add(productLevel3ViewConfigResponseDefault);
    itemPickupPointListingL3Response.getViewConfigs().add(productLevel3ViewConfigResponseCnc);
    RequestHelper.convertFromDataToExcelHeaderMap(new BulkPriceUpdateRequestData(), new BasicProductResponse(),
        itemPickupPointListingL3Response, null, cncPickupPointCodes);

    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    productLevel3ViewConfigResponseDefault.setDisplay(false);
    productLevel3ViewConfigResponseDefault.setBuyable(true);
    productLevel3ViewConfigResponseDefault.setChannelId("DEFAULT");
    productLevel3ViewConfigResponseCnc.setDisplay(false);
    productLevel3ViewConfigResponseCnc.setBuyable(true);
    productLevel3ViewConfigResponseCnc.setChannelId("CNC");
    itemPickupPointListingL3Response.getViewConfigs().add(productLevel3ViewConfigResponseDefault);
    itemPickupPointListingL3Response.getViewConfigs().add(productLevel3ViewConfigResponseCnc);
    RequestHelper.convertFromDataToExcelHeaderMap(new BulkPriceUpdateRequestData(), new BasicProductResponse(),
        itemPickupPointListingL3Response, null, cncPickupPointCodes);

    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    productLevel3ViewConfigResponseDefault.setDisplay(true);
    productLevel3ViewConfigResponseDefault.setBuyable(false);
    productLevel3ViewConfigResponseDefault.setChannelId("DEFAULT");
    productLevel3ViewConfigResponseCnc.setDisplay(true);
    productLevel3ViewConfigResponseCnc.setBuyable(false);
    productLevel3ViewConfigResponseCnc.setChannelId("CNC");
    itemPickupPointListingL3Response.getViewConfigs().add(productLevel3ViewConfigResponseDefault);
    itemPickupPointListingL3Response.getViewConfigs().add(productLevel3ViewConfigResponseCnc);
    RequestHelper.convertFromDataToExcelHeaderMap(new BulkPriceUpdateRequestData(), new BasicProductResponse(),
        itemPickupPointListingL3Response, null, cncPickupPointCodes);

    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    productLevel3ViewConfigResponseDefault.setDisplay(false);
    productLevel3ViewConfigResponseDefault.setBuyable(false);
    productLevel3ViewConfigResponseDefault.setChannelId("DEFAULT");
    productLevel3ViewConfigResponseCnc.setDisplay(false);
    productLevel3ViewConfigResponseCnc.setBuyable(true);
    productLevel3ViewConfigResponseCnc.setChannelId("CNC");
    itemPickupPointListingL3Response.getViewConfigs().add(productLevel3ViewConfigResponseDefault);
    itemPickupPointListingL3Response.getViewConfigs().add(productLevel3ViewConfigResponseCnc);
    RequestHelper.convertFromDataToExcelHeaderMap(new BulkPriceUpdateRequestData(), new BasicProductResponse(),
        itemPickupPointListingL3Response, null, cncPickupPointCodes);

    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    productLevel3ViewConfigResponseDefault.setDisplay(true);
    productLevel3ViewConfigResponseDefault.setBuyable(false);
    productLevel3ViewConfigResponseDefault.setChannelId("DEFAULT");
    productLevel3ViewConfigResponseCnc.setDisplay(true);
    productLevel3ViewConfigResponseCnc.setBuyable(true);
    productLevel3ViewConfigResponseCnc.setChannelId("CNC");
    itemPickupPointListingL3Response.getViewConfigs().add(productLevel3ViewConfigResponseDefault);
    itemPickupPointListingL3Response.getViewConfigs().add(productLevel3ViewConfigResponseCnc);
    RequestHelper.convertFromDataToExcelHeaderMap(new BulkPriceUpdateRequestData(), new BasicProductResponse(),
        itemPickupPointListingL3Response, null, cncPickupPointCodes);

    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    productLevel3ViewConfigResponseDefault.setDisplay(false);
    productLevel3ViewConfigResponseDefault.setBuyable(true);
    productLevel3ViewConfigResponseDefault.setChannelId("DEFAULT");
    productLevel3ViewConfigResponseCnc.setDisplay(false);
    productLevel3ViewConfigResponseCnc.setBuyable(true);
    productLevel3ViewConfigResponseCnc.setChannelId("CNC");
    itemPickupPointListingL3Response.getViewConfigs().add(productLevel3ViewConfigResponseDefault);
    itemPickupPointListingL3Response.getViewConfigs().add(productLevel3ViewConfigResponseCnc);
    RequestHelper.convertFromDataToExcelHeaderMap(new BulkPriceUpdateRequestData(), new BasicProductResponse(),
        itemPickupPointListingL3Response, null, cncPickupPointCodes);

    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    productLevel3ViewConfigResponseDefault.setDisplay(false);
    productLevel3ViewConfigResponseDefault.setBuyable(false);
    productLevel3ViewConfigResponseDefault.setChannelId("DEFAULT");
    productLevel3ViewConfigResponseCnc.setDisplay(true);
    productLevel3ViewConfigResponseCnc.setBuyable(false);
    productLevel3ViewConfigResponseCnc.setChannelId("CNC");
    itemPickupPointListingL3Response.getViewConfigs().add(productLevel3ViewConfigResponseDefault);
    itemPickupPointListingL3Response.getViewConfigs().add(productLevel3ViewConfigResponseCnc);
    RequestHelper.convertFromDataToExcelHeaderMap(new BulkPriceUpdateRequestData(), new BasicProductResponse(),
        itemPickupPointListingL3Response, null, cncPickupPointCodes);

    itemPickupPointListingL3Response.setPrices(Collections.singletonList(productLevel3PriceResponse));
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    itemPickupPointListingL3Response.setCncActive(true);
    itemPickupPointListingL3Response.setOff2OnActiveFlag(true);
    RequestHelper.convertFromDataToExcelHeaderMap(new BulkPriceUpdateRequestData(), new BasicProductResponse(),
        itemPickupPointListingL3Response, profileResponse, cncPickupPointCodes);
    ProductLevel3ViewConfigResponse productLevel3ViewConfigResponse = new ProductLevel3ViewConfigResponse();
    productLevel3ViewConfigResponse.setBuyable(true);
    productLevel3ViewConfigResponse.setDisplay(true);
    productLevel3ViewConfigResponse.setChannelId("DEFAULT");
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    itemPickupPointListingL3Response.getViewConfigs().add(productLevel3ViewConfigResponse);
    productLevel3ViewConfigResponseCnc.setBuyable(false);
    productLevel3ViewConfigResponseCnc.setDisplay(true);
    itemPickupPointListingL3Response.getViewConfigs().add(productLevel3ViewConfigResponseCnc);
    RequestHelper.convertFromDataToExcelHeaderMap(new BulkPriceUpdateRequestData(), new BasicProductResponse(),
        itemPickupPointListingL3Response, profileResponse, cncPickupPointCodes);
    productLevel3ViewConfigResponse.setBuyable(true);
    productLevel3ViewConfigResponse.setDisplay(false);
    productLevel3ViewConfigResponse.setChannelId("DEFAULT");
    itemPickupPointListingL3Response.setViewConfigs(Collections.singletonList(productLevel3ViewConfigResponse));
    RequestHelper.convertFromDataToExcelHeaderMap(new BulkPriceUpdateRequestData(), new BasicProductResponse(),
        itemPickupPointListingL3Response, profileResponse, cncPickupPointCodes);
    productLevel3ViewConfigResponse.setBuyable(false);
    productLevel3ViewConfigResponse.setDisplay(true);
    productLevel3ViewConfigResponse.setChannelId("DEFAULT");
    itemPickupPointListingL3Response.setViewConfigs(Collections.singletonList(productLevel3ViewConfigResponse));
    RequestHelper.convertFromDataToExcelHeaderMap(new BulkPriceUpdateRequestData(), new BasicProductResponse(),
        itemPickupPointListingL3Response, profileResponse, cncPickupPointCodes);
    productLevel3ViewConfigResponse.setBuyable(false);
    productLevel3ViewConfigResponse.setDisplay(false);
    productLevel3ViewConfigResponse.setChannelId("DEFAULT");
    itemPickupPointListingL3Response.setViewConfigs(Collections.singletonList(productLevel3ViewConfigResponse));
    RequestHelper.convertFromDataToExcelHeaderMap(new BulkPriceUpdateRequestData(), new BasicProductResponse(),
        itemPickupPointListingL3Response, profileResponse, cncPickupPointCodes);
    RequestHelper.convertFromDataToExcelHeaderMap(new BulkPriceUpdateRequestData(), new BasicProductResponse(),
        itemPickupPointListingL3Response, profileResponse, cncPickupPointCodes);
    BulkPriceUpdateRequestData bulkPriceUpdateRequestData = new BulkPriceUpdateRequestData();
    bulkPriceUpdateRequestData.setSellerCode(SELLER_CODE);
    bulkPriceUpdateRequestData.setProductSku(PRODUCT_CODE);
    bulkPriceUpdateRequestData.setItemSku(ITEM_SKU);
    bulkPriceUpdateRequestData.setPickupPointCode(PICKUP_POINT_CODE);
    bulkPriceUpdateRequestData.setCnc(0);
    bulkPriceUpdateRequestData.setDelivery(0);
    bulkPriceUpdateRequestData.setInstore(1);
    bulkPriceUpdateRequestData.setWarehouseStock(1);
    bulkPriceUpdateRequestData.setStock(1);
    bulkPriceUpdateRequestData.setListPrice(String.valueOf(100.0));
    bulkPriceUpdateRequestData.setSalesPrice(String.valueOf(100.0));
    bulkPriceUpdateRequestData.setProductName(PRODUCT_NAME);
    bulkPriceUpdateRequestData.setParentProductName(PRODUCT_NAME);
    bulkPriceUpdateRequestData.setStatus(0);
    RequestHelper.convertFromDataToExcelHeaderMap(bulkPriceUpdateRequestData, new BasicProductResponse(),
        itemPickupPointListingL3Response, profileResponse, cncPickupPointCodes);
    RequestHelper.convertFromDataToExcelHeaderMap(bulkPriceUpdateRequestData, new BasicProductResponse(),
        null, profileResponse, cncPickupPointCodes);
    itemPickupPointListingL3Response.setWebSyncStock(true);
    bulkPriceUpdateRequestData.setStock(null);
    bulkPriceUpdateRequestData.setSellerSku(SELLER_CODE);
    profileResponse.getCompany().setCncActivated(true);
    RequestHelper.convertFromDataToExcelHeaderMap(bulkPriceUpdateRequestData, new BasicProductResponse(),
        itemPickupPointListingL3Response, profileResponse, cncPickupPointCodes);
    bulkPriceUpdateRequestData.setCnc(null);
    RequestHelper.convertFromDataToExcelHeaderMap(bulkPriceUpdateRequestData, new BasicProductResponse(),
        itemPickupPointListingL3Response, profileResponse, cncPickupPointCodes);
    RequestHelper.convertFromDataToExcelHeaderMap(bulkPriceUpdateRequestData, new BasicProductResponse(),
        itemPickupPointListingL3Response, profileResponse, cncPickupPointCodes);
    cncPickupPointCodes.add(PICKUP_POINT_CODE);
    bulkPriceUpdateRequestData.setCnc(1);
    RequestHelper.convertFromDataToExcelHeaderMap(bulkPriceUpdateRequestData, new BasicProductResponse(),
        itemPickupPointListingL3Response, profileResponse, cncPickupPointCodes);
    bulkPriceUpdateRequestData.setCnc(null);
    RequestHelper.convertFromDataToExcelHeaderMap(bulkPriceUpdateRequestData, new BasicProductResponse(),
        itemPickupPointListingL3Response, profileResponse, cncPickupPointCodes);
  }

  @Test
  public void convertFromDataToExcelHeaderMapTest2() {
    ItemPickupPointListingL3Response itemPickupPointListingL3Response = new ItemPickupPointListingL3Response();
    ProductLevel3PriceResponse productLevel3PriceResponse = new ProductLevel3PriceResponse();
    productLevel3PriceResponse.setChannelId(DEFAULT_USERNAME);
    RequestHelper.convertFromDataToExcelHeaderMap(new BulkPriceUpdateRequestData(), new BasicProductResponse(),
        null, null, cncPickupPointCodes);
    itemPickupPointListingL3Response.setViewConfigs(new ArrayList<>());
    ProductLevel3ViewConfigResponse productLevel3ViewConfigResponseDefault = new ProductLevel3ViewConfigResponse();
    productLevel3ViewConfigResponseDefault.setDisplay(false);
    productLevel3ViewConfigResponseDefault.setBuyable(true);
    productLevel3ViewConfigResponseDefault.setChannelId("DEFAULT");
    ProductLevel3ViewConfigResponse productLevel3ViewConfigResponseCnc = new ProductLevel3ViewConfigResponse();
    productLevel3ViewConfigResponseCnc.setDisplay(false);
    productLevel3ViewConfigResponseCnc.setBuyable(true);
    productLevel3ViewConfigResponseCnc.setChannelId("CNC");
    itemPickupPointListingL3Response.getViewConfigs().add(productLevel3ViewConfigResponseDefault);
    itemPickupPointListingL3Response.getViewConfigs().add(productLevel3ViewConfigResponseCnc);
    RequestHelper.convertFromDataToExcelHeaderMap(new BulkPriceUpdateRequestData(), new BasicProductResponse(),
        itemPickupPointListingL3Response, null, cncPickupPointCodes);

    itemPickupPointListingL3Response.setPrices(Collections.singletonList(productLevel3PriceResponse));
    ProfileResponse profileResponse = new ProfileResponse();
    profileResponse.setCompany(new CompanyDTO());
    itemPickupPointListingL3Response.setCncActive(true);
    itemPickupPointListingL3Response.setOff2OnActiveFlag(true);
    RequestHelper.convertFromDataToExcelHeaderMap(new BulkPriceUpdateRequestData(), new BasicProductResponse(),
        itemPickupPointListingL3Response, profileResponse, cncPickupPointCodes);
    BulkPriceUpdateRequestData bulkPriceUpdateRequestData = new BulkPriceUpdateRequestData();
    bulkPriceUpdateRequestData.setSellerCode(SELLER_CODE);
    bulkPriceUpdateRequestData.setProductSku(PRODUCT_CODE);
    bulkPriceUpdateRequestData.setItemSku(ITEM_SKU);
    bulkPriceUpdateRequestData.setPickupPointCode(PICKUP_POINT_CODE);
    bulkPriceUpdateRequestData.setCnc(0);
    bulkPriceUpdateRequestData.setDelivery(0);
    bulkPriceUpdateRequestData.setInstore(1);
    bulkPriceUpdateRequestData.setWarehouseStock(1);
    bulkPriceUpdateRequestData.setStock(1);
    bulkPriceUpdateRequestData.setListPrice(String.valueOf(100.0));
    bulkPriceUpdateRequestData.setSalesPrice(String.valueOf(100.0));
    bulkPriceUpdateRequestData.setProductName(PRODUCT_NAME);
    bulkPriceUpdateRequestData.setParentProductName(PRODUCT_NAME);
    bulkPriceUpdateRequestData.setStatus(0);
    RequestHelper.convertFromDataToExcelHeaderMap(bulkPriceUpdateRequestData, new BasicProductResponse(),
        itemPickupPointListingL3Response, profileResponse, cncPickupPointCodes);
    RequestHelper.convertFromDataToExcelHeaderMap(bulkPriceUpdateRequestData, new BasicProductResponse(),
        null, profileResponse, cncPickupPointCodes);
    itemPickupPointListingL3Response.setWebSyncStock(true);
    bulkPriceUpdateRequestData.setStock(null);
    bulkPriceUpdateRequestData.setSellerSku(SELLER_CODE);
    profileResponse.getCompany().setCncActivated(true);
    RequestHelper.convertFromDataToExcelHeaderMap(bulkPriceUpdateRequestData, new BasicProductResponse(),
        itemPickupPointListingL3Response, profileResponse, cncPickupPointCodes);
    bulkPriceUpdateRequestData.setCnc(null);
    RequestHelper.convertFromDataToExcelHeaderMap(bulkPriceUpdateRequestData, new BasicProductResponse(),
        itemPickupPointListingL3Response, profileResponse, cncPickupPointCodes);
    RequestHelper.convertFromDataToExcelHeaderMap(bulkPriceUpdateRequestData, new BasicProductResponse(),
        itemPickupPointListingL3Response, profileResponse, cncPickupPointCodes);
    cncPickupPointCodes.add(PICKUP_POINT_CODE);
    bulkPriceUpdateRequestData.setCnc(1);
    RequestHelper.convertFromDataToExcelHeaderMap(bulkPriceUpdateRequestData, new BasicProductResponse(),
        itemPickupPointListingL3Response, profileResponse, cncPickupPointCodes);
    bulkPriceUpdateRequestData.setCnc(null);
    RequestHelper.convertFromDataToExcelHeaderMap(bulkPriceUpdateRequestData, new BasicProductResponse(),
        itemPickupPointListingL3Response, profileResponse, cncPickupPointCodes);
  }

  @Test
  public void validateHeaderByProcessTypeBulkProductTaggingTest() {
    Map<String, String> data = new TreeMap<>();
    data.put(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name(),
      BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    RequestHelper.validateHeaderByProcessType(Collections.singletonList(data), bulkInternalProcess);
  }

  @Test
  public void validateHeaderByProcessTypeProductTagging2() {
    Map<String, String> data = new TreeMap<>();
    data.put(BulkParameters.DELETE_PRODUCT_TAGGING, "Yes");
    data.put(BulkParameters.PRODUCT_TYPE_PRODUCT_TAGGING, "KVL");
    data.put(BulkParameters.ITEM_SKU_PRODUCT_TAGGING, ITEM_SKU);
    data.put(BulkParameters.PICKUP_POINT_CODE_PRODUCT_TAGGING, PICKUP_POINT_CODE);
    data.put(BulkParameters.STORE_ID, "storeId");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    RequestHelper.validateHeaderByProcessType(Collections.singletonList(data), bulkInternalProcess);
  }

  @Test
  public void getBulkInternalProcessDataForBulkProductTaggingTest() throws Exception {
    Map<String, String> data = new HashMap<>();
    data.put(BulkParameters.DELETE_PRODUCT_TAGGING, "Yes");
    data.put(BulkParameters.PRODUCT_TYPE_PRODUCT_TAGGING, "KVL");
    data.put(BulkParameters.ITEM_SKU_PRODUCT_TAGGING, "STT-15149-80388-00001");
    data.put(BulkParameters.PICKUP_POINT_CODE_PRODUCT_TAGGING, PICKUP_POINT_CODE);
    data.put(BulkParameters.STORE_ID, "storeId");
    data.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Collections.singletonList(data));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setBulkProductTypeTaggingMaxRows(10);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
      RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void getBulkInternalProcessDataForBulkProductTaggingInvalidItemSkuTest() throws Exception {
    Map<String, String> data = new HashMap<>();
    data.put(BulkParameters.DELETE_PRODUCT_TAGGING, "YES");
    data.put(BulkParameters.PRODUCT_TYPE_PRODUCT_TAGGING, "KVL");
    data.put(BulkParameters.ITEM_SKU_PRODUCT_TAGGING, ITEM_SKU);
    data.put(BulkParameters.PICKUP_POINT_CODE_PRODUCT_TAGGING, PICKUP_POINT_CODE);
    data.put(BulkParameters.STORE_ID, "storeId");
    data.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Collections.singletonList(data));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setBulkProductTypeTaggingMaxRows(10);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
      RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void getBulkInternalProcessDataForBulkProductTaggingInValidDeleteTaggingTest() throws Exception {
    Map<String, String> data = new HashMap<>();
    data.put(BulkParameters.DELETE_PRODUCT_TAGGING, "OK");
    data.put(BulkParameters.PRODUCT_TYPE_PRODUCT_TAGGING, "KVL");
    data.put(BulkParameters.ITEM_SKU_PRODUCT_TAGGING, "STT-15149-80388-00001");
    data.put(BulkParameters.PICKUP_POINT_CODE_PRODUCT_TAGGING, PICKUP_POINT_CODE);
    data.put(BulkParameters.STORE_ID, "storeId");
    data.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Collections.singletonList(data));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setBulkProductTypeTaggingMaxRows(10);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
      RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void getBulkInternalProcessDataForBulkProductTaggingEmptyTaggingTest() throws Exception {
    Map<String, String> data = new HashMap<>();
    data.put(BulkParameters.DELETE_PRODUCT_TAGGING, "yes");
    data.put(BulkParameters.PRODUCT_TYPE_PRODUCT_TAGGING, "");
    data.put(BulkParameters.ITEM_SKU_PRODUCT_TAGGING, "STT-15149-80388-00001");
    data.put(BulkParameters.PICKUP_POINT_CODE_PRODUCT_TAGGING, PICKUP_POINT_CODE);
    data.put(BulkParameters.ROW_NUMBER, "1");
    data.put(BulkParameters.STORE_ID, "storeId");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Collections.singletonList(data));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setBulkProductTypeTaggingMaxRows(10);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
      RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void getBulkInternalProcessDataForBulkProductTaggingEmptyItemSkuTest() throws Exception {
    Map<String, String> data = new HashMap<>();
    data.put(BulkParameters.DELETE_PRODUCT_TAGGING, "Yes");
    data.put(BulkParameters.PRODUCT_TYPE_PRODUCT_TAGGING, "ABCD");
    data.put(BulkParameters.ITEM_SKU_PRODUCT_TAGGING, "");
    data.put(BulkParameters.PICKUP_POINT_CODE_PRODUCT_TAGGING, PICKUP_POINT_CODE);
    data.put(BulkParameters.STORE_ID, "storeId");
    data.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    bulkInternalProcessDataGenerationDTO.setBulkProductTypeTaggingMaxRows(10);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Collections.singletonList(data));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
      RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void getBulkInternalProcessDataForBulkProductTaggingEmptyPPCodeTest() throws Exception {
    Map<String, String> data = new HashMap<>();
    data.put(BulkParameters.DELETE_PRODUCT_TAGGING, "Yes");
    data.put(BulkParameters.PRODUCT_TYPE_PRODUCT_TAGGING, "ABCD");
    data.put(BulkParameters.ITEM_SKU_PRODUCT_TAGGING, "STT-15149-80388-00001");
    data.put(BulkParameters.PICKUP_POINT_CODE_PRODUCT_TAGGING, "");
    data.put(BulkParameters.STORE_ID, "storeId");
    data.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Collections.singletonList(data));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setBulkProductTypeTaggingMaxRows(10);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
      RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void getBulkInternalProcessDataForBulkProductTaggingDuplicateL5Test() throws Exception {
    Map<String, String> data = new HashMap<>();
    Map<String, String> data2 = new HashMap<>();

    data.put(BulkParameters.DELETE_PRODUCT_TAGGING, "Yes");
    data.put(BulkParameters.PRODUCT_TYPE_PRODUCT_TAGGING, "ABCD");
    data.put(BulkParameters.ITEM_SKU_PRODUCT_TAGGING, "STT-15149-80388-00001");
    data.put(BulkParameters.PICKUP_POINT_CODE_PRODUCT_TAGGING, "PP-1234");
    data.put(BulkParameters.STORE_ID, "storeId");
    data.put(BulkParameters.ROW_NUMBER, "1");
    data2.put(BulkParameters.ROW_NUMBER, "2");
    data2.put(BulkParameters.DELETE_PRODUCT_TAGGING, "NO");
    data2.put(BulkParameters.PRODUCT_TYPE_PRODUCT_TAGGING, "XYZ");
    data2.put(BulkParameters.ITEM_SKU_PRODUCT_TAGGING, "STT-15149-80388-00001");
    data2.put(BulkParameters.PICKUP_POINT_CODE_PRODUCT_TAGGING, "PP-1234");
    data2.put(BulkParameters.STORE_ID, "storeId");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Arrays.asList(data,
      data2));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setBulkProductTypeTaggingMaxRows(10);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
      RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(2, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(ProcessStatus.FAILED.name(),
      bulkInternalProcessDataList.get(1).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.DUPLICATE_ROW_PRODUCT_TAGGING_ERROR,
      bulkInternalProcessDataList.get(1).getErrorMessage());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.DUPLICATE_ROW_PRODUCT_TAGGING_ERROR,
      bulkInternalProcessDataList.get(0).getErrorMessage());
  }

  @Test
  public void getBulkInternalProcessDataForBulkProductTaggingEmptyTest() throws Exception {
    Map<String, String> data = new HashMap<>();
    data.put(BulkParameters.DELETE_PRODUCT_TAGGING, "Yes");
    data.put(BulkParameters.PRODUCT_TYPE_PRODUCT_TAGGING, "");
    data.put(BulkParameters.ITEM_SKU_PRODUCT_TAGGING, "STT-15149-80388-00001");
    data.put(BulkParameters.PICKUP_POINT_CODE_PRODUCT_TAGGING, PICKUP_POINT_CODE);
    data.put(BulkParameters.STORE_ID, "storeId");
    data.put(BulkParameters.ROW_NUMBER, "1");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Collections.singletonList(data));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setBulkProductTypeTaggingMaxRows(10);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
      RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void getBulkInternalProcessDataForBulkProductExceptionTest() throws Exception {
    Map<String, String> data = new HashMap<>();
    data.put(BulkParameters.DELETE_PRODUCT_TAGGING, "Yes");
    data.put(BulkParameters.PRODUCT_TYPE_PRODUCT_TAGGING, "KVL");
    data.put(BulkParameters.ITEM_SKU_PRODUCT_TAGGING, "STT-15149-80388-00001");
    data.put(BulkParameters.PICKUP_POINT_CODE_PRODUCT_TAGGING, PICKUP_POINT_CODE);
    data.put(BulkParameters.STORE_ID, "storeId");
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BULK_PRICE_PRODUCT_TYPE_TAGGING.name());
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(Collections.singletonList(data));
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    ObjectMapper objectMapperMock = Mockito.mock(ObjectMapper.class);
    Mockito.when(objectMapperMock.writeValueAsString(Mockito.any(Object.class))).thenThrow(JsonProcessingException.class);


    List<BulkInternalProcessData> bulkInternalProcessDataList = null;
    try {
      bulkInternalProcessDataList = RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    } catch (Exception e) {
      Assertions.assertTrue(e instanceof JsonProcessingException);
    }
    Assertions.assertNotNull(bulkInternalProcessDataList);
  }

  @Test
  public void toUpdateRemoveProductTaggingRequestTest(){
    List<BulkPriceProductTypeTaggingRequest> bulkPriceProductTypeTaggingRequests =
      new ArrayList<>();
    BulkPriceProductTypeTaggingRequest bulkPriceProductTypeTaggingRequest =
      new BulkPriceProductTypeTaggingRequest();
    bulkPriceProductTypeTaggingRequest.setItemSku(ITEM_SKU);
    bulkPriceProductTypeTaggingRequest.setPickupPointCode(PICKUP_POINT_CODE);
    bulkPriceProductTypeTaggingRequest.setProductTypeTagging("KVL");
    bulkPriceProductTypeTaggingRequest.setId("Id");
    bulkPriceProductTypeTaggingRequests.add(bulkPriceProductTypeTaggingRequest);
    List<UpdateRemoveProductTaggingRequest> updateRemoveProductTaggingRequest =
      RequestHelper.toUpdateRemoveProductTaggingRequest(bulkPriceProductTypeTaggingRequests);
    Assertions.assertEquals(ITEM_SKU.concat(Constant.HYPHEN).concat(PICKUP_POINT_CODE),
      updateRemoveProductTaggingRequest.get(0).getItemPickUpPointId());
  }

  @Test
  public void getWaitingDeletionTest() {
    Assertions.assertFalse(RequestHelper.getWaitingDeletion(true));
    Assertions.assertNull(RequestHelper.getWaitingDeletion(false));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeBrandAuthFormatterDateTest() throws Exception {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (String string : ExcelHeaderNames.BRAND_AUTH_ADD_TEMPLATE_HEADER) {
      map.put(string, "");
    }
    map.put(ExcelHeaderNames.AUTH_START_DATE, "12/05/2024");
    map.put(ExcelHeaderNames.AUTH_END_DATE, "13/06/2024");
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_ADD.name());
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setBrandAuthEndYear(5);
    List<BulkInternalProcessData> bulkInternalProcessDataList =
      RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, bulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.IN_PROGRESS.name(), bulkInternalProcess.getStatus());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataList.get(0).getStatus());
  }

  @Test
  public void isExcelHeaderValidBulkIPRProductAddReview_validHeaderSize() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    int ind = 0;
    for (String header :
        BulkIPRProductsParameter.BULK_ADD_REVIEW_IPR_PRODUCTS_HEADERS) {
      map.put(header, Integer.toString(ind++));
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    Assertions.assertTrue(
        RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidBulkIPRProductAddReview_validHeadersTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    int ind = 0;
    for (String header :
        BulkIPRProductsParameter.BULK_ADD_REVIEW_IPR_PRODUCTS_HEADERS) {
      map.put(header, Integer.toString(ind++));
    }
    map.put("rowNumber", Integer.toString(ind++));
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    Assertions.assertFalse(
        RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void isExcelHeaderValidBulkIPRProductAddReview_invalidHeaders() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    int ind = 0;
    for (String header :
        BulkIPRProductsParameter.BULK_ADD_REVIEW_IPR_PRODUCTS_HEADERS) {
      map.put(header + " invalid",Integer.toString(ind++));
    }
    map.put("rowNumber", Integer.toString(ind));
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    Assertions.assertTrue(
        RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }


  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkIPRProductAddReviewTest()
      throws Exception {
    BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
        BulkAddReviewIPRProductsRequestData.builder().productSku(PRODUCT_SKU)
            .productName(PRODUCT_NAME).reasons(REASON_VALUE).violationType(VIOLATION_TYPE_VALUE)
            .action(ACTION_VALUE).sellerNotes(SELLER_NOTES).reviewerNotes(REVIEWER_NOTES)
            .source(SOURCE).excelRowNumber(1).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_SKU,
        bulkAddReviewIPRProductsRequestData.getProductSku());
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_NAME,
        bulkAddReviewIPRProductsRequestData.getProductName());
    keyValuePairMap.put(BulkIPRProductsParameter.VIOLATION_TYPE,
        bulkAddReviewIPRProductsRequestData.getViolationType());
      keyValuePairMap.put(BulkIPRProductsParameter.ACTION,
        bulkAddReviewIPRProductsRequestData.getAction());
    keyValuePairMap.put(BulkIPRProductsParameter.SELLER_NOTES,
        bulkAddReviewIPRProductsRequestData.getSellerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REVIEWER_NOTES,
        bulkAddReviewIPRProductsRequestData.getReviewerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REASON,
        bulkAddReviewIPRProductsRequestData.getReasons());
    keyValuePairMap.put(BulkIPRProductsParameter.SOURCE,
        bulkAddReviewIPRProductsRequestData.getSource());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setIprValidActions(Collections.singleton(ACTION_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprSource(Collections.singleton(SOURCE));
    bulkInternalProcessDataGenerationDTO.setIprReasons(Collections.singleton(REASON_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprViolationTypes(
        Collections.singleton(VIOLATION_TYPE_VALUE));
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkIPRProductsParameter.IPR_REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(bulkAddReviewIPRProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
            BulkAddReviewIPRProductsRequestData.class));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkIPRProductAddReviewTest_emptySource()
      throws Exception {
    BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
        BulkAddReviewIPRProductsRequestData.builder().productSku(PRODUCT_SKU)
            .productName(PRODUCT_NAME).reasons(REASON_VALUE).violationType(VIOLATION_TYPE_VALUE)
            .action(ACTION_VALUE).sellerNotes(SELLER_NOTES).reviewerNotes(REVIEWER_NOTES)
            .source(StringUtils.EMPTY).excelRowNumber(1).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_SKU,
        bulkAddReviewIPRProductsRequestData.getProductSku());
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_NAME,
        bulkAddReviewIPRProductsRequestData.getProductName());
    keyValuePairMap.put(BulkIPRProductsParameter.VIOLATION_TYPE,
        bulkAddReviewIPRProductsRequestData.getViolationType());
    keyValuePairMap.put(BulkIPRProductsParameter.ACTION,
        bulkAddReviewIPRProductsRequestData.getAction());
    keyValuePairMap.put(BulkIPRProductsParameter.SELLER_NOTES,
        bulkAddReviewIPRProductsRequestData.getSellerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REVIEWER_NOTES,
        bulkAddReviewIPRProductsRequestData.getReviewerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REASON,
        bulkAddReviewIPRProductsRequestData.getReasons());
    keyValuePairMap.put(BulkIPRProductsParameter.SOURCE,
        bulkAddReviewIPRProductsRequestData.getSource());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setIprValidActions(Collections.singleton(ACTION_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprSource(Collections.singleton(SOURCE));
    bulkInternalProcessDataGenerationDTO.setIprReasons(Collections.singleton(REASON_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprViolationTypes(
        Collections.singleton(VIOLATION_TYPE_VALUE));
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkIPRProductsParameter.IPR_REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(bulkAddReviewIPRProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
            BulkAddReviewIPRProductsRequestData.class));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkIPRProductAddReviewTest_invalidSource()
      throws Exception {
    BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
        BulkAddReviewIPRProductsRequestData.builder().productSku(PRODUCT_SKU)
            .productName(PRODUCT_NAME).reasons(REASON_VALUE).violationType(VIOLATION_TYPE_VALUE)
            .action(ACTION_VALUE).sellerNotes(SELLER_NOTES).reviewerNotes(REVIEWER_NOTES)
            .source(INVALID).excelRowNumber(1).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_SKU,
        bulkAddReviewIPRProductsRequestData.getProductSku());
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_NAME,
        bulkAddReviewIPRProductsRequestData.getProductName());
    keyValuePairMap.put(BulkIPRProductsParameter.VIOLATION_TYPE,
        bulkAddReviewIPRProductsRequestData.getViolationType());
    keyValuePairMap.put(BulkIPRProductsParameter.ACTION,
        bulkAddReviewIPRProductsRequestData.getAction());
    keyValuePairMap.put(BulkIPRProductsParameter.SELLER_NOTES,
        bulkAddReviewIPRProductsRequestData.getSellerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REVIEWER_NOTES,
        bulkAddReviewIPRProductsRequestData.getReviewerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REASON,
        bulkAddReviewIPRProductsRequestData.getReasons());
    keyValuePairMap.put(BulkIPRProductsParameter.SOURCE,
        bulkAddReviewIPRProductsRequestData.getSource());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setIprValidActions(Collections.singleton(ACTION_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprSource(Collections.singleton(SOURCE));
    bulkInternalProcessDataGenerationDTO.setIprReasons(Collections.singleton(REASON_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprViolationTypes(
        Collections.singleton(VIOLATION_TYPE_VALUE));
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkIPRProductsParameter.IPR_REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.UNKNOWN_SOURCE,
        actualBulkInternalProcessDataList.get(0).getErrorMessage());
    Assertions.assertEquals(bulkAddReviewIPRProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
            BulkAddReviewIPRProductsRequestData.class));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkIPRProductAddReviewTestEmptyBrandReport()
      throws Exception {
    BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
        BulkAddReviewIPRProductsRequestData.builder().productSku(PRODUCT_SKU)
            .productName(PRODUCT_NAME).reasons(REASON_VALUE).violationType(VIOLATION_TYPE_VALUE)
            .action(ACTION_VALUE).sellerNotes(SELLER_NOTES).reviewerNotes(REVIEWER_NOTES)
            .source(BRAND_REPORT).excelRowNumber(1).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_SKU,
        bulkAddReviewIPRProductsRequestData.getProductSku());
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_NAME,
        bulkAddReviewIPRProductsRequestData.getProductName());
    keyValuePairMap.put(BulkIPRProductsParameter.VIOLATION_TYPE,
        bulkAddReviewIPRProductsRequestData.getViolationType());
    keyValuePairMap.put(BulkIPRProductsParameter.ACTION,
        bulkAddReviewIPRProductsRequestData.getAction());
    keyValuePairMap.put(BulkIPRProductsParameter.SELLER_NOTES,
        bulkAddReviewIPRProductsRequestData.getSellerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REVIEWER_NOTES,
        bulkAddReviewIPRProductsRequestData.getReviewerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REASON,
        bulkAddReviewIPRProductsRequestData.getReasons());
    keyValuePairMap.put(BulkIPRProductsParameter.SOURCE,
        bulkAddReviewIPRProductsRequestData.getSource());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setIprValidActions(Collections.singleton(ACTION_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprSource(Collections.singleton(BRAND_REPORT));
    bulkInternalProcessDataGenerationDTO.setIprReasons(Collections.singleton(REASON_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprViolationTypes(
        Collections.singleton(VIOLATION_TYPE_VALUE));
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkIPRProductsParameter.IPR_REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.MANDATORY_FIELDS_MUST_NOT_BE_BLANK,
        actualBulkInternalProcessDataList.get(0).getErrorMessage());
    Assertions.assertEquals(bulkAddReviewIPRProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
            BulkAddReviewIPRProductsRequestData.class));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkIPRProductAddReviewTestEmptyBrandReportReporter()
      throws Exception {
    BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
        BulkAddReviewIPRProductsRequestData.builder().productSku(PRODUCT_SKU)
            .productName(PRODUCT_NAME).reasons(REASON_VALUE).violationType(VIOLATION_TYPE_VALUE)
            .action(ACTION_VALUE).sellerNotes(SELLER_NOTES).reviewerNotes(REVIEWER_NOTES)
            .source(BRAND_REPORT).excelRowNumber(1).reportDate(REPORT_DATE).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_SKU,
        bulkAddReviewIPRProductsRequestData.getProductSku());
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_NAME,
        bulkAddReviewIPRProductsRequestData.getProductName());
    keyValuePairMap.put(BulkIPRProductsParameter.VIOLATION_TYPE,
        bulkAddReviewIPRProductsRequestData.getViolationType());
    keyValuePairMap.put(BulkIPRProductsParameter.ACTION,
        bulkAddReviewIPRProductsRequestData.getAction());
    keyValuePairMap.put(BulkIPRProductsParameter.SELLER_NOTES,
        bulkAddReviewIPRProductsRequestData.getSellerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REVIEWER_NOTES,
        bulkAddReviewIPRProductsRequestData.getReviewerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REASON,
        bulkAddReviewIPRProductsRequestData.getReasons());
    keyValuePairMap.put(BulkIPRProductsParameter.SOURCE,
        bulkAddReviewIPRProductsRequestData.getSource());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORT_DATE,
        bulkAddReviewIPRProductsRequestData.getReportDate());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setIprValidActions(Collections.singleton(ACTION_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprSource(Collections.singleton(BRAND_REPORT));
    bulkInternalProcessDataGenerationDTO.setIprReasons(Collections.singleton(REASON_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprViolationTypes(
        Collections.singleton(VIOLATION_TYPE_VALUE));
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkIPRProductsParameter.IPR_REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.MANDATORY_FIELDS_MUST_NOT_BE_BLANK,
        actualBulkInternalProcessDataList.get(0).getErrorMessage());
    Assertions.assertEquals(bulkAddReviewIPRProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
            BulkAddReviewIPRProductsRequestData.class));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkIPRProductAddReviewTestEmptyBrandReportReporterNameEmpty()
      throws Exception {
    BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
        BulkAddReviewIPRProductsRequestData.builder().productSku(PRODUCT_SKU)
            .productName(PRODUCT_NAME).reasons(REASON_VALUE).violationType(VIOLATION_TYPE_VALUE)
            .action(ACTION_VALUE).sellerNotes(SELLER_NOTES).reviewerNotes(REVIEWER_NOTES)
            .source(BRAND_REPORT).excelRowNumber(1).reportDate(REPORT_DATE).reporter(REPORTER).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_SKU,
        bulkAddReviewIPRProductsRequestData.getProductSku());
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_NAME,
        bulkAddReviewIPRProductsRequestData.getProductName());
    keyValuePairMap.put(BulkIPRProductsParameter.VIOLATION_TYPE,
        bulkAddReviewIPRProductsRequestData.getViolationType());
    keyValuePairMap.put(BulkIPRProductsParameter.ACTION,
        bulkAddReviewIPRProductsRequestData.getAction());
    keyValuePairMap.put(BulkIPRProductsParameter.SELLER_NOTES,
        bulkAddReviewIPRProductsRequestData.getSellerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REVIEWER_NOTES,
        bulkAddReviewIPRProductsRequestData.getReviewerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REASON,
        bulkAddReviewIPRProductsRequestData.getReasons());
    keyValuePairMap.put(BulkIPRProductsParameter.SOURCE,
        bulkAddReviewIPRProductsRequestData.getSource());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORT_DATE,
        bulkAddReviewIPRProductsRequestData.getReportDate());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORTER,
        bulkAddReviewIPRProductsRequestData.getReporter());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setIprValidActions(Collections.singleton(ACTION_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprSource(Collections.singleton(BRAND_REPORT));
    bulkInternalProcessDataGenerationDTO.setIprReasons(Collections.singleton(REASON_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprViolationTypes(
        Collections.singleton(VIOLATION_TYPE_VALUE));
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkIPRProductsParameter.IPR_REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.MANDATORY_FIELDS_MUST_NOT_BE_BLANK,
        actualBulkInternalProcessDataList.get(0).getErrorMessage());
    Assertions.assertEquals(bulkAddReviewIPRProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
            BulkAddReviewIPRProductsRequestData.class));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkIPRProductAddReviewTestEmptyBrandReportReporterEmailEmpty()
      throws Exception {
    BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
        BulkAddReviewIPRProductsRequestData.builder().productSku(PRODUCT_SKU)
            .productName(PRODUCT_NAME).reasons(REASON_VALUE).violationType(VIOLATION_TYPE_VALUE)
            .action(ACTION_VALUE).sellerNotes(SELLER_NOTES).reviewerNotes(REVIEWER_NOTES)
            .source(BRAND_REPORT).excelRowNumber(1).reportDate(REPORT_DATE).reporter(REPORTER)
            .reporterName(REPORTER_NAME).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_SKU,
        bulkAddReviewIPRProductsRequestData.getProductSku());
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_NAME,
        bulkAddReviewIPRProductsRequestData.getProductName());
    keyValuePairMap.put(BulkIPRProductsParameter.VIOLATION_TYPE,
        bulkAddReviewIPRProductsRequestData.getViolationType());
    keyValuePairMap.put(BulkIPRProductsParameter.ACTION,
        bulkAddReviewIPRProductsRequestData.getAction());
    keyValuePairMap.put(BulkIPRProductsParameter.SELLER_NOTES,
        bulkAddReviewIPRProductsRequestData.getSellerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REVIEWER_NOTES,
        bulkAddReviewIPRProductsRequestData.getReviewerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REASON,
        bulkAddReviewIPRProductsRequestData.getReasons());
    keyValuePairMap.put(BulkIPRProductsParameter.SOURCE,
        bulkAddReviewIPRProductsRequestData.getSource());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORT_DATE,
        bulkAddReviewIPRProductsRequestData.getReportDate());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORTER,
        bulkAddReviewIPRProductsRequestData.getReporter());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORTER_NAME,
        bulkAddReviewIPRProductsRequestData.getReporterName());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setIprValidActions(Collections.singleton(ACTION_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprSource(Collections.singleton(BRAND_REPORT));
    bulkInternalProcessDataGenerationDTO.setIprReasons(Collections.singleton(REASON_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprViolationTypes(
        Collections.singleton(VIOLATION_TYPE_VALUE));
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkIPRProductsParameter.IPR_REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.MANDATORY_FIELDS_MUST_NOT_BE_BLANK,
        actualBulkInternalProcessDataList.get(0).getErrorMessage());
    Assertions.assertEquals(bulkAddReviewIPRProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
            BulkAddReviewIPRProductsRequestData.class));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkIPRProductAddReviewTestEmptyBrandReportReporterReasonEmpty()
      throws Exception {
    BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
        BulkAddReviewIPRProductsRequestData.builder().productSku(PRODUCT_SKU)
            .productName(PRODUCT_NAME).reasons(REASON_VALUE).violationType(VIOLATION_TYPE_VALUE)
            .action(ACTION_VALUE).sellerNotes(SELLER_NOTES).reviewerNotes(REVIEWER_NOTES)
            .source(BRAND_REPORT).excelRowNumber(1).reportDate(REPORT_DATE).reporter(REPORTER)
            .reporterName(REPORTER_NAME).reporterEmail(REPORTER_EMAIL).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_SKU,
        bulkAddReviewIPRProductsRequestData.getProductSku());
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_NAME,
        bulkAddReviewIPRProductsRequestData.getProductName());
    keyValuePairMap.put(BulkIPRProductsParameter.VIOLATION_TYPE,
        bulkAddReviewIPRProductsRequestData.getViolationType());
    keyValuePairMap.put(BulkIPRProductsParameter.ACTION,
        bulkAddReviewIPRProductsRequestData.getAction());
    keyValuePairMap.put(BulkIPRProductsParameter.SELLER_NOTES,
        bulkAddReviewIPRProductsRequestData.getSellerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REVIEWER_NOTES,
        bulkAddReviewIPRProductsRequestData.getReviewerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REASON,
        bulkAddReviewIPRProductsRequestData.getReasons());
    keyValuePairMap.put(BulkIPRProductsParameter.SOURCE,
        bulkAddReviewIPRProductsRequestData.getSource());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORT_DATE,
        bulkAddReviewIPRProductsRequestData.getReportDate());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORTER,
        bulkAddReviewIPRProductsRequestData.getReporter());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORTER_NAME,
        bulkAddReviewIPRProductsRequestData.getReporterName());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORTER_EMAIL,
        bulkAddReviewIPRProductsRequestData.getReporterEmail());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setIprValidActions(Collections.singleton(ACTION_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprSource(Collections.singleton(BRAND_REPORT));
    bulkInternalProcessDataGenerationDTO.setIprReasons(Collections.singleton(REASON_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprViolationTypes(
        Collections.singleton(VIOLATION_TYPE_VALUE));
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkIPRProductsParameter.IPR_REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.MANDATORY_FIELDS_MUST_NOT_BE_BLANK,
        actualBulkInternalProcessDataList.get(0).getErrorMessage());
    Assertions.assertEquals(bulkAddReviewIPRProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
            BulkAddReviewIPRProductsRequestData.class));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkIPRProductAddReviewTestEmptyBrandReportReportDateNotCorrect()
      throws Exception {
    BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
        BulkAddReviewIPRProductsRequestData.builder().productSku(PRODUCT_SKU)
            .productName(PRODUCT_NAME).reasons(REASON_VALUE).violationType(VIOLATION_TYPE_VALUE)
            .action(ACTION_VALUE).sellerNotes(SELLER_NOTES).reviewerNotes(REVIEWER_NOTES)
            .source(BRAND_REPORT).excelRowNumber(1).reportDate(REPORT_DATE_WRONG).reporter(REPORTER)
            .reporterName(REPORTER_NAME).reporterEmail(REPORTER_EMAIL).reporterReason(REPORTER_REASON).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_SKU,
        bulkAddReviewIPRProductsRequestData.getProductSku());
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_NAME,
        bulkAddReviewIPRProductsRequestData.getProductName());
    keyValuePairMap.put(BulkIPRProductsParameter.VIOLATION_TYPE,
        bulkAddReviewIPRProductsRequestData.getViolationType());
    keyValuePairMap.put(BulkIPRProductsParameter.ACTION,
        bulkAddReviewIPRProductsRequestData.getAction());
    keyValuePairMap.put(BulkIPRProductsParameter.SELLER_NOTES,
        bulkAddReviewIPRProductsRequestData.getSellerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REVIEWER_NOTES,
        bulkAddReviewIPRProductsRequestData.getReviewerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REASON,
        bulkAddReviewIPRProductsRequestData.getReasons());
    keyValuePairMap.put(BulkIPRProductsParameter.SOURCE,
        bulkAddReviewIPRProductsRequestData.getSource());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORT_DATE,
        bulkAddReviewIPRProductsRequestData.getReportDate());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORTER,
        bulkAddReviewIPRProductsRequestData.getReporter());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORTER_NAME,
        bulkAddReviewIPRProductsRequestData.getReporterName());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORTER_EMAIL,
        bulkAddReviewIPRProductsRequestData.getReporterEmail());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORTER_REASON,
        bulkAddReviewIPRProductsRequestData.getReporterReason());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setIprValidActions(Collections.singleton(ACTION_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprSource(Collections.singleton(BRAND_REPORT));
    bulkInternalProcessDataGenerationDTO.setIprReasons(Collections.singleton(REASON_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprViolationTypes(
        Collections.singleton(VIOLATION_TYPE_VALUE));
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkIPRProductsParameter.IPR_REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.DATE_NOT_IN_CORRECT_FORMAT,
        actualBulkInternalProcessDataList.get(0).getErrorMessage());
    Assertions.assertEquals(bulkAddReviewIPRProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
            BulkAddReviewIPRProductsRequestData.class));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkIPRProductAddReviewTestEmptyBrandReportReportDateNumericNotCorrect()
      throws Exception {
    BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
        BulkAddReviewIPRProductsRequestData.builder().productSku(PRODUCT_SKU)
            .productName(PRODUCT_NAME).reasons(REASON_VALUE).violationType(VIOLATION_TYPE_VALUE)
            .action(ACTION_VALUE).sellerNotes(SELLER_NOTES).reviewerNotes(REVIEWER_NOTES)
            .source(BRAND_REPORT).excelRowNumber(1).reportDate(REPORT_DATE_NUMERIC_WRONG)
            .reporter(REPORTER).reporterName(REPORTER_NAME).reporterEmail(REPORTER_EMAIL)
            .reporterReason(REPORTER_REASON).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_SKU,
        bulkAddReviewIPRProductsRequestData.getProductSku());
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_NAME,
        bulkAddReviewIPRProductsRequestData.getProductName());
    keyValuePairMap.put(BulkIPRProductsParameter.VIOLATION_TYPE,
        bulkAddReviewIPRProductsRequestData.getViolationType());
    keyValuePairMap.put(BulkIPRProductsParameter.ACTION,
        bulkAddReviewIPRProductsRequestData.getAction());
    keyValuePairMap.put(BulkIPRProductsParameter.SELLER_NOTES,
        bulkAddReviewIPRProductsRequestData.getSellerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REVIEWER_NOTES,
        bulkAddReviewIPRProductsRequestData.getReviewerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REASON,
        bulkAddReviewIPRProductsRequestData.getReasons());
    keyValuePairMap.put(BulkIPRProductsParameter.SOURCE,
        bulkAddReviewIPRProductsRequestData.getSource());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORT_DATE,
        bulkAddReviewIPRProductsRequestData.getReportDate());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORTER,
        bulkAddReviewIPRProductsRequestData.getReporter());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORTER_NAME,
        bulkAddReviewIPRProductsRequestData.getReporterName());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORTER_EMAIL,
        bulkAddReviewIPRProductsRequestData.getReporterEmail());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORTER_REASON,
        bulkAddReviewIPRProductsRequestData.getReporterReason());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setIprValidActions(Collections.singleton(ACTION_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprSource(Collections.singleton(BRAND_REPORT));
    bulkInternalProcessDataGenerationDTO.setIprReasons(Collections.singleton(REASON_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprViolationTypes(
        Collections.singleton(VIOLATION_TYPE_VALUE));
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkIPRProductsParameter.IPR_REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.DATE_NOT_IN_CORRECT_FORMAT,
        actualBulkInternalProcessDataList.get(0).getErrorMessage());
    Assertions.assertEquals(bulkAddReviewIPRProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
            BulkAddReviewIPRProductsRequestData.class));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkIPRProductAddReviewTestEmptyBrandReportReporterReasonNotEmpty()
      throws Exception {
    BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
        BulkAddReviewIPRProductsRequestData.builder().productSku(PRODUCT_SKU)
            .productName(PRODUCT_NAME).reasons(REASON_VALUE).violationType(VIOLATION_TYPE_VALUE)
            .action(ACTION_VALUE).sellerNotes(SELLER_NOTES).reviewerNotes(REVIEWER_NOTES)
            .source(BRAND_REPORT).excelRowNumber(1).reportDate(REPORT_DATE).reporter(REPORTER)
            .reporterName(REPORTER_NAME).reporterEmail(REPORTER_EMAIL)
            .reporterReason(REPORTER_REASON).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_SKU,
        bulkAddReviewIPRProductsRequestData.getProductSku());
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_NAME,
        bulkAddReviewIPRProductsRequestData.getProductName());
    keyValuePairMap.put(BulkIPRProductsParameter.VIOLATION_TYPE,
        bulkAddReviewIPRProductsRequestData.getViolationType());
    keyValuePairMap.put(BulkIPRProductsParameter.ACTION,
        bulkAddReviewIPRProductsRequestData.getAction());
    keyValuePairMap.put(BulkIPRProductsParameter.SELLER_NOTES,
        bulkAddReviewIPRProductsRequestData.getSellerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REVIEWER_NOTES,
        bulkAddReviewIPRProductsRequestData.getReviewerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REASON,
        bulkAddReviewIPRProductsRequestData.getReasons());
    keyValuePairMap.put(BulkIPRProductsParameter.SOURCE,
        bulkAddReviewIPRProductsRequestData.getSource());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORT_DATE,
        bulkAddReviewIPRProductsRequestData.getReportDate());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORTER,
        bulkAddReviewIPRProductsRequestData.getReporter());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORTER_NAME,
        bulkAddReviewIPRProductsRequestData.getReporterName());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORTER_EMAIL,
        bulkAddReviewIPRProductsRequestData.getReporterEmail());
    keyValuePairMap.put(BulkIPRProductsParameter.REPORTER_REASON,
        bulkAddReviewIPRProductsRequestData.getReporterReason());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setIprValidActions(Collections.singleton(ACTION_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprSource(Collections.singleton(BRAND_REPORT));
    bulkInternalProcessDataGenerationDTO.setIprReasons(Collections.singleton(REASON_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprViolationTypes(
        Collections.singleton(VIOLATION_TYPE_VALUE));
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkIPRProductsParameter.IPR_REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(bulkAddReviewIPRProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
            BulkAddReviewIPRProductsRequestData.class));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkIPRProductAddReviewTest_emptyViolationType()
      throws Exception {
    BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
        BulkAddReviewIPRProductsRequestData.builder().productSku(PRODUCT_SKU)
            .productName(PRODUCT_NAME).reasons(REASON_VALUE).violationType(StringUtils.EMPTY)
            .action(ACTION_VALUE).sellerNotes(SELLER_NOTES).reviewerNotes(REVIEWER_NOTES)
            .source(SOURCE).excelRowNumber(1).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_SKU,
        bulkAddReviewIPRProductsRequestData.getProductSku());
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_NAME,
        bulkAddReviewIPRProductsRequestData.getProductName());
    keyValuePairMap.put(BulkIPRProductsParameter.VIOLATION_TYPE,
        bulkAddReviewIPRProductsRequestData.getViolationType());
    keyValuePairMap.put(BulkIPRProductsParameter.ACTION,
        bulkAddReviewIPRProductsRequestData.getAction());
    keyValuePairMap.put(BulkIPRProductsParameter.SELLER_NOTES,
        bulkAddReviewIPRProductsRequestData.getSellerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REVIEWER_NOTES,
        bulkAddReviewIPRProductsRequestData.getReviewerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REASON,
        bulkAddReviewIPRProductsRequestData.getReasons());
    keyValuePairMap.put(BulkIPRProductsParameter.SOURCE,
        bulkAddReviewIPRProductsRequestData.getSource());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setIprValidActions(Collections.singleton(ACTION_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprSource(Collections.singleton(SOURCE));
    bulkInternalProcessDataGenerationDTO.setIprReasons(Collections.singleton(REASON_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprViolationTypes(
        Collections.singleton(VIOLATION_TYPE_VALUE));
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkIPRProductsParameter.IPR_REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(bulkAddReviewIPRProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
            BulkAddReviewIPRProductsRequestData.class));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkIPRProductAddReviewTest_invalidViolationType()
      throws Exception {
    BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
        BulkAddReviewIPRProductsRequestData.builder().productSku(PRODUCT_SKU)
            .productName(PRODUCT_NAME).reasons(REASON_VALUE).violationType(INVALID)
            .action(ACTION_VALUE).sellerNotes(SELLER_NOTES).reviewerNotes(REVIEWER_NOTES)
            .source(SOURCE).excelRowNumber(1).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_SKU,
        bulkAddReviewIPRProductsRequestData.getProductSku());
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_NAME,
        bulkAddReviewIPRProductsRequestData.getProductName());
    keyValuePairMap.put(BulkIPRProductsParameter.VIOLATION_TYPE,
        bulkAddReviewIPRProductsRequestData.getViolationType());
    keyValuePairMap.put(BulkIPRProductsParameter.ACTION,
        bulkAddReviewIPRProductsRequestData.getAction());
    keyValuePairMap.put(BulkIPRProductsParameter.SELLER_NOTES,
        bulkAddReviewIPRProductsRequestData.getSellerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REVIEWER_NOTES,
        bulkAddReviewIPRProductsRequestData.getReviewerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REASON,
        bulkAddReviewIPRProductsRequestData.getReasons());
    keyValuePairMap.put(BulkIPRProductsParameter.SOURCE,
        bulkAddReviewIPRProductsRequestData.getSource());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setIprValidActions(Collections.singleton(ACTION_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprSource(Collections.singleton(SOURCE));
    bulkInternalProcessDataGenerationDTO.setIprReasons(Collections.singleton(REASON_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprViolationTypes(
        Collections.singleton(VIOLATION_TYPE_VALUE));
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkIPRProductsParameter.IPR_REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.UNKNOWN_VIOLATION_TYPE,
        actualBulkInternalProcessDataList.get(0).getErrorMessage());
    Assertions.assertEquals(bulkAddReviewIPRProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
            BulkAddReviewIPRProductsRequestData.class));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkIPRProductAddReviewTest_emptyReason()
      throws Exception {
    BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
        BulkAddReviewIPRProductsRequestData.builder().productSku(PRODUCT_SKU)
            .productName(PRODUCT_NAME).reasons(StringUtils.EMPTY)
            .violationType(VIOLATION_TYPE_VALUE).action(ACTION_VALUE).sellerNotes(SELLER_NOTES)
            .reviewerNotes(REVIEWER_NOTES).source(SOURCE).excelRowNumber(1).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_SKU,
        bulkAddReviewIPRProductsRequestData.getProductSku());
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_NAME,
        bulkAddReviewIPRProductsRequestData.getProductName());
    keyValuePairMap.put(BulkIPRProductsParameter.VIOLATION_TYPE,
        bulkAddReviewIPRProductsRequestData.getViolationType());
    keyValuePairMap.put(BulkIPRProductsParameter.ACTION,
        bulkAddReviewIPRProductsRequestData.getAction());
    keyValuePairMap.put(BulkIPRProductsParameter.SELLER_NOTES,
        bulkAddReviewIPRProductsRequestData.getSellerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REVIEWER_NOTES,
        bulkAddReviewIPRProductsRequestData.getReviewerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REASON,
        bulkAddReviewIPRProductsRequestData.getReasons());
    keyValuePairMap.put(BulkIPRProductsParameter.SOURCE,
        bulkAddReviewIPRProductsRequestData.getSource());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setIprValidActions(Collections.singleton(ACTION_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprSource(Collections.singleton(SOURCE));
    bulkInternalProcessDataGenerationDTO.setIprReasons(Collections.singleton(REASON_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprViolationTypes(
        Collections.singleton(VIOLATION_TYPE_VALUE));
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkIPRProductsParameter.IPR_REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(bulkAddReviewIPRProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
            BulkAddReviewIPRProductsRequestData.class));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkIPRProductAddReviewTest_invalidReason()
      throws Exception {
    BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
        BulkAddReviewIPRProductsRequestData.builder().productSku(PRODUCT_SKU)
            .productName(PRODUCT_NAME).reasons(INVALID).violationType(VIOLATION_TYPE_VALUE)
            .action(ACTION_VALUE).sellerNotes(SELLER_NOTES).reviewerNotes(REVIEWER_NOTES)
            .source(SOURCE).excelRowNumber(1).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_SKU,
        bulkAddReviewIPRProductsRequestData.getProductSku());
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_NAME,
        bulkAddReviewIPRProductsRequestData.getProductName());
    keyValuePairMap.put(BulkIPRProductsParameter.VIOLATION_TYPE,
        bulkAddReviewIPRProductsRequestData.getViolationType());
    keyValuePairMap.put(BulkIPRProductsParameter.ACTION,
        bulkAddReviewIPRProductsRequestData.getAction());
    keyValuePairMap.put(BulkIPRProductsParameter.SELLER_NOTES,
        bulkAddReviewIPRProductsRequestData.getSellerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REVIEWER_NOTES,
        bulkAddReviewIPRProductsRequestData.getReviewerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REASON,
        bulkAddReviewIPRProductsRequestData.getReasons());
    keyValuePairMap.put(BulkIPRProductsParameter.SOURCE,
        bulkAddReviewIPRProductsRequestData.getSource());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setIprValidActions(Collections.singleton(ACTION_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprSource(Collections.singleton(SOURCE));
    bulkInternalProcessDataGenerationDTO.setIprReasons(Collections.singleton(REASON_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprViolationTypes(
        Collections.singleton(VIOLATION_TYPE_VALUE));
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkIPRProductsParameter.IPR_REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.UNKNOWN_REASONS,
        actualBulkInternalProcessDataList.get(0).getErrorMessage());
    Assertions.assertEquals(bulkAddReviewIPRProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
            BulkAddReviewIPRProductsRequestData.class));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkIPRProductAddReviewTest_invalidReviewer()
      throws Exception {
    BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
        BulkAddReviewIPRProductsRequestData.builder().productSku(PRODUCT_SKU)
            .productName(PRODUCT_NAME).reasons(REASON_VALUE).violationType(VIOLATION_TYPE_VALUE)
            .action(ACTION_VALUE).sellerNotes(SELLER_NOTES).reviewerNotes(REVIEWER_NOTES)
            .source(SOURCE).assignee(ASSIGNEE).excelRowNumber(1).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_SKU,
        bulkAddReviewIPRProductsRequestData.getProductSku());
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_NAME,
        bulkAddReviewIPRProductsRequestData.getProductName());
    keyValuePairMap.put(BulkIPRProductsParameter.VIOLATION_TYPE,
        bulkAddReviewIPRProductsRequestData.getViolationType());
    keyValuePairMap.put(BulkIPRProductsParameter.ACTION,
        bulkAddReviewIPRProductsRequestData.getAction());
    keyValuePairMap.put(BulkIPRProductsParameter.SELLER_NOTES,
        bulkAddReviewIPRProductsRequestData.getSellerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REVIEWER_NOTES,
        bulkAddReviewIPRProductsRequestData.getReviewerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REASON,
        bulkAddReviewIPRProductsRequestData.getReasons());
    keyValuePairMap.put(BulkIPRProductsParameter.SOURCE,
        bulkAddReviewIPRProductsRequestData.getSource());
    keyValuePairMap.put(BulkIPRProductsParameter.ASSIGNEE,
        bulkAddReviewIPRProductsRequestData.getAssignee());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setIprValidActions(Collections.singleton(ACTION_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprSource(Collections.singleton(SOURCE));
    bulkInternalProcessDataGenerationDTO.setIprReasons(Collections.singleton(REASON_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprViolationTypes(
        Collections.singleton(VIOLATION_TYPE_VALUE));
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkIPRProductsParameter.IPR_REVIEWERS, List.of(ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(BulkProcessValidationErrorMessages.INVALID_REVIEWER,
        actualBulkInternalProcessDataList.get(0).getErrorMessage());
    Assertions.assertEquals(bulkAddReviewIPRProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
            BulkAddReviewIPRProductsRequestData.class));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkIPRProductAddReviewTest_emptyReviewer()
      throws Exception {
    BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
        BulkAddReviewIPRProductsRequestData.builder().productSku(PRODUCT_SKU)
            .productName(PRODUCT_NAME).reasons(REASON_VALUE).violationType(VIOLATION_TYPE_VALUE)
            .action(ACTION_VALUE).sellerNotes(SELLER_NOTES).reviewerNotes(REVIEWER_NOTES)
            .source(SOURCE).assignee(StringUtils.EMPTY).excelRowNumber(1).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_SKU,
        bulkAddReviewIPRProductsRequestData.getProductSku());
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_NAME,
        bulkAddReviewIPRProductsRequestData.getProductName());
    keyValuePairMap.put(BulkIPRProductsParameter.VIOLATION_TYPE,
        bulkAddReviewIPRProductsRequestData.getViolationType());
    keyValuePairMap.put(BulkIPRProductsParameter.ACTION,
        bulkAddReviewIPRProductsRequestData.getAction());
    keyValuePairMap.put(BulkIPRProductsParameter.SELLER_NOTES,
        bulkAddReviewIPRProductsRequestData.getSellerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REVIEWER_NOTES,
        bulkAddReviewIPRProductsRequestData.getReviewerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REASON,
        bulkAddReviewIPRProductsRequestData.getReasons());
    keyValuePairMap.put(BulkIPRProductsParameter.SOURCE,
        bulkAddReviewIPRProductsRequestData.getSource());
    keyValuePairMap.put(BulkIPRProductsParameter.ASSIGNEE,
        bulkAddReviewIPRProductsRequestData.getAssignee());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setIprValidActions(Collections.singleton(ACTION_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprSource(Collections.singleton(SOURCE));
    bulkInternalProcessDataGenerationDTO.setIprReasons(Collections.singleton(REASON_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprViolationTypes(
        Collections.singleton(VIOLATION_TYPE_VALUE));
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkIPRProductsParameter.IPR_REVIEWERS, List.of(ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(bulkAddReviewIPRProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
            BulkAddReviewIPRProductsRequestData.class));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkIPRProductAddReviewTest_validReviewer()
      throws Exception {
    BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
        BulkAddReviewIPRProductsRequestData.builder().productSku(PRODUCT_SKU)
            .productName(PRODUCT_NAME).reasons(REASON_VALUE).violationType(VIOLATION_TYPE_VALUE)
            .action(ACTION_VALUE).sellerNotes(SELLER_NOTES).reviewerNotes(REVIEWER_NOTES)
            .source(SOURCE).assignee(ASSIGNEE).excelRowNumber(1).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_SKU,
        bulkAddReviewIPRProductsRequestData.getProductSku());
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_NAME,
        bulkAddReviewIPRProductsRequestData.getProductName());
    keyValuePairMap.put(BulkIPRProductsParameter.VIOLATION_TYPE,
        bulkAddReviewIPRProductsRequestData.getViolationType());
    keyValuePairMap.put(BulkIPRProductsParameter.ACTION,
        bulkAddReviewIPRProductsRequestData.getAction());
    keyValuePairMap.put(BulkIPRProductsParameter.SELLER_NOTES,
        bulkAddReviewIPRProductsRequestData.getSellerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REVIEWER_NOTES,
        bulkAddReviewIPRProductsRequestData.getReviewerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REASON,
        bulkAddReviewIPRProductsRequestData.getReasons());
    keyValuePairMap.put(BulkIPRProductsParameter.SOURCE,
        bulkAddReviewIPRProductsRequestData.getSource());
    keyValuePairMap.put(BulkIPRProductsParameter.ASSIGNEE,
        bulkAddReviewIPRProductsRequestData.getAssignee());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    bulkInternalProcessDataGenerationDTO.setIprValidActions(Collections.singleton(ACTION_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprSource(Collections.singleton(SOURCE));
    bulkInternalProcessDataGenerationDTO.setIprReasons(Collections.singleton(REASON_VALUE));
    bulkInternalProcessDataGenerationDTO.setIprViolationTypes(
        Collections.singleton(VIOLATION_TYPE_VALUE));
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkIPRProductsParameter.IPR_REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.PENDING.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(bulkAddReviewIPRProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
            BulkAddReviewIPRProductsRequestData.class));
  }

  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkIPRProductAddReviewEmptyProductNameTest()
      throws Exception {
    BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
        BulkAddReviewIPRProductsRequestData.builder().productSku(PRODUCT_SKU)
            .productName(StringUtils.EMPTY).reasons(REASON_VALUE)
            .violationType(VIOLATION_TYPE_VALUE).action(ACTION_VALUE).sellerNotes(SELLER_NOTES)
            .reviewerNotes(REVIEWER_NOTES).excelRowNumber(1).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_SKU,
        bulkAddReviewIPRProductsRequestData.getProductSku());
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_NAME,
        bulkAddReviewIPRProductsRequestData.getProductName());
    keyValuePairMap.put(BulkIPRProductsParameter.VIOLATION_TYPE,
        bulkAddReviewIPRProductsRequestData.getViolationType());
    keyValuePairMap.put(BulkIPRProductsParameter.ACTION,
        bulkAddReviewIPRProductsRequestData.getAction());
    keyValuePairMap.put(BulkIPRProductsParameter.SELLER_NOTES,
        bulkAddReviewIPRProductsRequestData.getSellerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REVIEWER_NOTES,
        bulkAddReviewIPRProductsRequestData.getReviewerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REASON,
        bulkAddReviewIPRProductsRequestData.getReasons());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkIPRProductsParameter.IPR_REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(bulkAddReviewIPRProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
            BulkAddReviewIPRProductsRequestData.class));
  }
  @Test
  public void generateBulkInternalProcessDataByProcessTypeForBulkIPRProductAddReviewEmptyActionTest()
      throws Exception {
    BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData =
        BulkAddReviewIPRProductsRequestData.builder().productSku(PRODUCT_SKU)
            .productName(PRODUCT_NAME).reasons(REASON_VALUE).violationType(VIOLATION_TYPE_VALUE)
            .action(StringUtils.EMPTY).sellerNotes(SELLER_NOTES).reviewerNotes(REVIEWER_NOTES)
            .excelRowNumber(1).build();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> keyValuePairMap = new HashMap<>();
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_SKU,
        bulkAddReviewIPRProductsRequestData.getProductSku());
    keyValuePairMap.put(BulkIPRProductsParameter.PRODUCT_NAME,
        bulkAddReviewIPRProductsRequestData.getProductName());
    keyValuePairMap.put(BulkIPRProductsParameter.VIOLATION_TYPE,
        bulkAddReviewIPRProductsRequestData.getViolationType());
    keyValuePairMap.put(BulkIPRProductsParameter.ACTION,
        bulkAddReviewIPRProductsRequestData.getAction());
    keyValuePairMap.put(BulkIPRProductsParameter.SELLER_NOTES,
        bulkAddReviewIPRProductsRequestData.getSellerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REVIEWER_NOTES,
        bulkAddReviewIPRProductsRequestData.getReviewerNotes());
    keyValuePairMap.put(BulkIPRProductsParameter.REASON,
        bulkAddReviewIPRProductsRequestData.getReasons());
    internalProcessDataFromExcel.add(
        keyValuePairMap);
    bulkInternalProcessDataGenerationDTO.setBulkInternalProcess(bulkInternalProcess);
    bulkInternalProcessDataGenerationDTO.setInternalProcessDataFromExcel(internalProcessDataFromExcel);
    Map<String,List<String>> reviewersMap = new HashMap<>();
    reviewersMap.put(BulkIPRProductsParameter.IPR_REVIEWERS, Arrays.asList(ASSIGNEE, ASSIGNEE2));
    bulkInternalProcessDataGenerationDTO.setReviewers(reviewersMap);
    List<BulkInternalProcessData> actualBulkInternalProcessDataList =
        RequestHelper.generateBulkInternalProcessDataByProcessType(bulkInternalProcessDataGenerationDTO);
    Assertions.assertEquals(1, actualBulkInternalProcessDataList.size());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), actualBulkInternalProcessDataList.get(0).getStatus());
    Assertions.assertEquals(bulkAddReviewIPRProductsRequestData,
        new ObjectMapper().readValue(actualBulkInternalProcessDataList.get(0).getData(),
            BulkAddReviewIPRProductsRequestData.class));
  }

  @Test
  public void isExcelHeaderValidSuspendWrongSizeTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (String header : POIUtil.SUSPENSION_HEADERS) {
      map.put(String.valueOf(map.size()), header);
    }
    map.put(String.valueOf(map.size()), EXTRA_HEADER_1);
    map.put(String.valueOf(map.size()), EXTRA_HEADER_2);
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.SUSPEND.name());
    boolean result = RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(result);
  }

  @Test
  public void validateHeaderByProcessTypeSuspendTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    for (String header : POIUtil.SUSPENSION_HEADERS) {
      map.put(header, StringUtils.EMPTY);
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.SUSPEND.name());
    boolean result = RequestHelper.validateHeaderByProcessType(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertFalse(result);
  }

  @Test
  public void isExcelHeaderValid_emptyListTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.SUSPEND.name());

    boolean result = RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertEquals(StoreCopyConstants.FILE_IS_EMPTY, bulkInternalProcess.getNotes());
  }

  @Test
  public void validateHeaderByProcessType_emptyListTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.SUSPEND.name());

    boolean result = RequestHelper.validateHeaderByProcessType(internalProcessDataFromExcel, bulkInternalProcess);

    Assertions.assertTrue(result, "Should return true (invalid headers) for empty list");
  }

  @Test
  public void validateHeaderByProcessTypeSuspendMissingHeaderTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    map.put(BulkProductSuspensionParameters.SELLER_CODE, StringUtils.EMPTY);
    map.put(BulkProductSuspensionParameters.PRODUCT_CODE, StringUtils.EMPTY);
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.SUSPEND.name());
    boolean result = RequestHelper.validateHeaderByProcessType(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(result);
  }

  @Test
  public void isExcelHeaderValidSuspendHeadersInDifferentOrderTest() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    map.put("0", BulkProductSuspensionParameters.PRODUCT_CODE);
    map.put("1", BulkProductSuspensionParameters.PRODUCT_NAME);
    map.put("2", BulkProductSuspensionParameters.SELLER_CODE);
    map.put("3", BulkProductSuspensionParameters.SELLER_NAME);
    map.put("4", BulkProductSuspensionParameters.REASON);
    map.put("5", BulkProductSuspensionParameters.SELLER_REASON_DESCRIPTION);
    map.put("6", BulkProductSuspensionParameters.FAILURE_REASON);
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.SUSPEND.name());
    boolean result = RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess);
    Assertions.assertTrue(result);
  }

  @Test
  public void isExcelHeaderValidSuspendProducts_validHeaders() {
    List<Map<String, String>> internalProcessDataFromExcel = new ArrayList<>();
    Map<String, String> map = new HashMap<>();
    int ind = 0;
    for (String header : POIUtil.SUSPENSION_HEADERS) {
      map.put(header, Integer.toString(ind++));
    }
    internalProcessDataFromExcel.add(map);
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(
      BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());
    Assertions.assertTrue(
      RequestHelper.isExcelHeaderValid(internalProcessDataFromExcel, bulkInternalProcess));
  }

  @Test
  public void removeL5sWithStockToPreventDeletion_requestsFilteredCorrectly() {
    List<DeleteOfflineItemRequest> requests = new ArrayList<>();
    requests.add(new DeleteOfflineItemRequest(ITEM_SKU, MERCHANT_CODE, PICKUP_POINT_CODE));
    requests.add(new DeleteOfflineItemRequest(ITEM_SKU1, MERCHANT_CODE, PICKUP_POINT_CODE));

    List<DeleteOfflineItemDetailResponse> responses = new ArrayList<>();
    responses.add(new DeleteOfflineItemDetailResponse(ITEM_SKU, PICKUP_POINT_CODE, null));
    responses.add(new DeleteOfflineItemDetailResponse(ITEM_SKU1, PICKUP_POINT_CODE1, null));

    Assertions.assertEquals(2, requests.size());

    RequestHelper.removeL5sWithStockToPreventDeletion(requests, responses);
    Assertions.assertEquals(1, requests.size());
    Assertions.assertEquals(ITEM_SKU1, requests.get(0).getItemSku());
  }

  @Test
  public void testSetBasicInternalProcessDataDetailsForInternalBrandUpdate_BrandAuthorised()
      throws JsonProcessingException {
    // Given
    String storeId = "store123";
    String userName = "testUser";
    String productCode = "PROD123";
    boolean brandAuthorisedForUpdate = true;
    
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    
    // When
    RequestHelper.setBasicInternalProcessDataDetailsForInternalBrandUpdate(
        storeId, bulkInternalProcess, userName, bulkInternalProcessData, 
        productCode, brandAuthorisedForUpdate, bulkInternalProcessDataList, "brandcode");
    
    // Then
    Assertions.assertEquals(productCode, bulkInternalProcessData.getParentCode());
  }

  @Test
  public void testSetBasicInternalProcessDataDetailsForInternalBrandUpdate_BrandNotAuthorised()
      throws JsonProcessingException {
    // Given
    String storeId = "store456";
    String userName = "testUser2";
    String productCode = "PROD456";
    boolean brandAuthorisedForUpdate = false;
    
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    
    // When
    RequestHelper.setBasicInternalProcessDataDetailsForInternalBrandUpdate(
        storeId, bulkInternalProcess, userName, bulkInternalProcessData, 
        productCode, brandAuthorisedForUpdate, bulkInternalProcessDataList, "brandCode");
    
    // Then
    Assertions.assertEquals(productCode, bulkInternalProcessData.getParentCode());

  }

  @Test
  public void testSetBasicInternalProcessDataDetailsForInternalBrandUpdate_MultipleEntries()
      throws JsonProcessingException {
    // Given
    String storeId = "store789";
    String userName = "testUser3";
    String productCode = "PROD789";
    boolean brandAuthorisedForUpdate = true;
    
    BulkInternalProcess bulkInternalProcess = new BulkInternalProcess();
    BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    
    // Add existing entry
    BulkInternalProcessData existingData = new BulkInternalProcessData();
    bulkInternalProcessDataList.add(existingData);
    
    // When
    RequestHelper.setBasicInternalProcessDataDetailsForInternalBrandUpdate(
        storeId, bulkInternalProcess, userName, bulkInternalProcessData, 
        productCode, brandAuthorisedForUpdate, bulkInternalProcessDataList, "brandCode");
    
    // Then
    Assertions.assertEquals(2, bulkInternalProcessDataList.size());
  }

  @Test
  public void calculateCompletionPercentageTest() {
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> RequestHelper.calculateCompletionPercentage(10, 100, StringUtils.EMPTY, new HashSet<>()));
    Assertions.assertThrows(ApplicationRuntimeException.class,
        () -> RequestHelper.calculateCompletionPercentage(10, 100, PRODUCT_CODE, new HashSet<>()));
    Assertions.assertEquals(0,
        RequestHelper.calculateCompletionPercentage(10, 100, PRODUCT_CODE, Collections.singleton(PRODUCT_CODE)));
  }
}
