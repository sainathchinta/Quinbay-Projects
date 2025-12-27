package com.gdn.mta.bulk.service.util;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.dto.BulkProcessDataDTO;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.BulkUpdateErrorDTO;
import com.gdn.mta.bulk.dto.GenericTemplateFileType;
import com.gdn.mta.bulk.dto.ProductDetailsRequest;
import com.gdn.mta.bulk.dto.QrCodeRowInfo;
import com.gdn.mta.bulk.dto.ShippingTypeEligibility;
import com.gdn.mta.bulk.dto.product.DownloadQRCodeRequest;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkProcessNotes;
import com.gdn.mta.bulk.entity.WorkOrderEventModel;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.mta.model.enums.ProductType;
import org.apache.commons.collections4.CollectionUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.entity.BulkProcessData;
import com.gdn.mta.bulk.models.DeletePickupPointResponseEventModel;
import com.gdn.mta.bulk.models.RestrictedKeywordRequestData;
import com.gdn.mta.bulk.models.download.CampaignProductDownloadRequest;
import com.gdn.x.businesspartner.dto.CompanyDTO;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.rest.web.model.request.CampaignItemSummaryRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuSummaryRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryKeywordUpdateRequestList;
import com.google.common.collect.ImmutableSet;

public class ConverterUtilTest {

  private static final String CATEGORY_CODE = "categoryCode";
  private static final String BRAND = "brand";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String MERCHANT_NAME = "merchantName";
  private static final String TEMPLATE_SIZE = "1";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String ITEM_SKU = "itemSku-itemSku";
  private static final String STATUS = "FAILED";
  private static final String SYSTEM_ERROR = "System error";
  private static final String DELETE_PICKUP_POINT_NOT_ALLOWED = "Delete not allowed, Item will be left with no active L5 ";
  private static final String REQUEST_JSON = "{\"itemSku\":\"itemSku\"}";
  private static final String CC_MERCHANT = "CC";
  private static final String TD_MERCHANT = "TD";
  private static final String CNC_MERCHANT = "CM,CC";
  private static final String KEYWORD = "keyword";
  private static final String KEYWORD_TYPE = "keywordType";
  private static final String KEYWORD_ACTION = "No action";
  private static final String MESSAGE = "message";
  private static final String DESTINATION_CATEGORY = "AK-10001";
  private static final String EXCLUDE_CATEGORY = "AK-10002";
  private static final int EXCEL_ROW_NUMBER = 1;
  private static final String DEFAULT_BULK_PROCESS_CODE = "BP-1234";

  private CampaignProductDownloadRequest campaignProductDownloadRequest =
    new CampaignProductDownloadRequest();
  private ProfileResponse profileResponse;
  private DownloadQRCodeRequest downloadQRCodeRequest;
  private QrCodeRowInfo qrCodeRowInfo;
  private BulkProcess bulkProcess;
  private CompanyDTO companyDTO;

  @BeforeEach
  public void setUp() throws Exception {
    campaignProductDownloadRequest.setCampaignItemSummaryRequest(new CampaignItemSummaryRequest());
    campaignProductDownloadRequest.getCampaignItemSummaryRequest()
      .setCategories(Collections.singletonList(CATEGORY_CODE));
    campaignProductDownloadRequest.getCampaignItemSummaryRequest()
      .setBrands(Collections.singletonList(BRAND));

    profileResponse = new ProfileResponse();
    companyDTO = new CompanyDTO();

    downloadQRCodeRequest = DownloadQRCodeRequest
        .builder()
        .allStores(true)
        .merchantCode(MERCHANT_CODE)
        .isDarkTheme(true)
        .merchantName(MERCHANT_NAME)
        .productDetailsRequestList(new ArrayList<>())
        .printPrice(true)
        .qrPerPage(1)
        .templateSize(TEMPLATE_SIZE)
        .build();

    qrCodeRowInfo = new QrCodeRowInfo();
    bulkProcess = new BulkProcess();
  }

  @Test
  public void toProductSkuSummaryRequestTest() {
    ProductSkuSummaryRequest response =
      ConverterUtil.toProductSkuSummaryRequest(campaignProductDownloadRequest);
    Assertions.assertFalse(response.getIsArchived());
    Assertions.assertTrue(response.getCategoryCodes().contains(CATEGORY_CODE));
    Assertions.assertTrue(response.getBrand().contains(BRAND));
  }

  @Test
  public void toProductSkuSummaryRequest_emptyRequestTest() {
    campaignProductDownloadRequest.setCampaignItemSummaryRequest(new CampaignItemSummaryRequest());
    ProductSkuSummaryRequest response =
      ConverterUtil.toProductSkuSummaryRequest(campaignProductDownloadRequest);
    Assertions.assertFalse(response.getIsArchived());
    Assertions.assertTrue(CollectionUtils.isEmpty(response.getCategoryCodes()));
    Assertions.assertTrue(CollectionUtils.isEmpty(response.getBrand()));
  }

  @Test
  public void toDeletePickupPointResponseEventModelTest() throws IOException {
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData1.setErrorMessage(
        "{\"itemSku\":\"HIC-60001-00003-00001\",\"pickupPointCode\":\"PP-3010025\",\"reason\":\"System error\"}");
    bulkProcessData2.setErrorMessage(
        "{\"itemSku\":\"HIC-60001-00003-00002\",\"pickupPointCode\":\"PP-3010025\",\"reason\":\"Delete not allowed, Item will be left with no active L5\"}");
    bulkProcessData2.setBulkRequestData(REQUEST_JSON);

    DeletePickupPointResponseEventModel deletePickupPointResponseEventModel =
        ConverterUtil.toDeletePickupPointResponseEventModel(BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE, STATUS,
            Arrays.asList(bulkProcessData1, bulkProcessData2), false);

    Assertions.assertEquals(BUSINESS_PARTNER_CODE, deletePickupPointResponseEventModel.getBusinessPartnerCode());
    Assertions.assertEquals(PICKUP_POINT_CODE, deletePickupPointResponseEventModel.getPickupPointCode());
    Assertions.assertEquals(STATUS, deletePickupPointResponseEventModel.getStatus());
    Assertions.assertEquals("HIC-60001-00003-00001", deletePickupPointResponseEventModel.getFailedItemAndReasonResponseList().get(0).getItemSku());
    Assertions.assertEquals("System error", deletePickupPointResponseEventModel.getFailedItemAndReasonResponseList().get(0).getReason());
  }

  @Test
  public void toDeletePickupPointResponseEventModelSwitchOnTest() throws IOException {
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData1.setErrorMessage(
        "{\"itemSku\":\"HIC-60001-00003-00001\",\"pickupPointCode\":\"PP-3010025\",\"reason\":\"System error\"}");
    bulkProcessData2.setErrorMessage(
        "{\"itemSku\":\"HIC-60001-00003-00002\",\"pickupPointCode\":\"PP-3010025\",\"reason\":\"Delete not allowed, Item will be left with no active L5\"}");
    bulkProcessData2.setBulkRequestData(REQUEST_JSON);

    DeletePickupPointResponseEventModel deletePickupPointResponseEventModel =
        ConverterUtil.toDeletePickupPointResponseEventModel(BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE, STATUS,
            Arrays.asList(bulkProcessData1, bulkProcessData2), true);

    Assertions.assertEquals(BUSINESS_PARTNER_CODE, deletePickupPointResponseEventModel.getBusinessPartnerCode());
    Assertions.assertEquals(PICKUP_POINT_CODE, deletePickupPointResponseEventModel.getPickupPointCode());
    Assertions.assertEquals(STATUS, deletePickupPointResponseEventModel.getStatus());
    Assertions.assertEquals("HIC-60001-00003-00001", deletePickupPointResponseEventModel.getFailedItemAndReasonResponseList().get(0).getItemSku());
    Assertions.assertEquals("System error", deletePickupPointResponseEventModel.getFailedItemAndReasonResponseList().get(0).getReason());
  }

  @Test
  public void toDeletePickupPointResponseEventModelFailStatusTest() throws IOException {
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData1.setErrorMessage(
        "{\"itemSku\":\"HIC-60001-00003-00001\",\"pickupPointCode\":\"PP-3010025\",\"reason\":\"System error\"}");
    bulkProcessData2.setErrorMessage(
        "{\"itemSku\":\"HIC-60001-00003-00002\",\"pickupPointCode\":\"PP-3010025\",\"reason\":\"Delete not allowed, Item will be left with no active L5\"}");
    bulkProcessData2.setBulkRequestData(REQUEST_JSON);

    DeletePickupPointResponseEventModel deletePickupPointResponseEventModel =
        ConverterUtil.toDeletePickupPointResponseEventModel(BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE,
            BulkProcess.STATUS_FAIL, Arrays.asList(bulkProcessData1, bulkProcessData2), true);

    Assertions.assertEquals(BUSINESS_PARTNER_CODE, deletePickupPointResponseEventModel.getBusinessPartnerCode());
    Assertions.assertEquals(PICKUP_POINT_CODE, deletePickupPointResponseEventModel.getPickupPointCode());
    Assertions.assertEquals("FAILED", deletePickupPointResponseEventModel.getStatus());
    Assertions.assertEquals("HIC-60001-00003-00001",
        deletePickupPointResponseEventModel.getFailedItemAndReasonResponseList().get(0).getItemSku());
    Assertions.assertEquals("System error",
        deletePickupPointResponseEventModel.getFailedItemAndReasonResponseList().get(0).getReason());
  }

  @Test
  public void toDeletePickupPointResponseEventModelPartiallyCompletedStatusTest() throws IOException {
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData1.setErrorMessage(
        "{\"itemSku\":\"HIC-60001-00003-00001\",\"pickupPointCode\":\"PP-3010025\",\"reason\":\"System error\"}");
    bulkProcessData2.setErrorMessage(
        "{\"itemSku\":\"HIC-60001-00003-00002\",\"pickupPointCode\":\"PP-3010025\",\"reason\":\"Delete not allowed, Item will be left with no active L5\"}");
    bulkProcessData2.setBulkRequestData(REQUEST_JSON);

    DeletePickupPointResponseEventModel deletePickupPointResponseEventModel =
        ConverterUtil.toDeletePickupPointResponseEventModel(BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE,
            BulkProcess.STATUS_PARTIALLY_DONE, Arrays.asList(bulkProcessData1, bulkProcessData2), true);

    Assertions.assertEquals(BUSINESS_PARTNER_CODE, deletePickupPointResponseEventModel.getBusinessPartnerCode());
    Assertions.assertEquals(PICKUP_POINT_CODE, deletePickupPointResponseEventModel.getPickupPointCode());
    Assertions.assertEquals("FAILED", deletePickupPointResponseEventModel.getStatus());
    Assertions.assertEquals("HIC-60001-00003-00001",
        deletePickupPointResponseEventModel.getFailedItemAndReasonResponseList().get(0).getItemSku());
    Assertions.assertEquals("System error",
        deletePickupPointResponseEventModel.getFailedItemAndReasonResponseList().get(0).getReason());
  }

  @Test
  public void toDeletePickupPointResponseEventModelAbortedStatusTest() throws IOException {
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData1.setErrorMessage(
        "{\"itemSku\":\"HIC-60001-00003-00001\",\"pickupPointCode\":\"PP-3010025\",\"reason\":\"System error\"}");
    bulkProcessData2.setErrorMessage(
        "{\"itemSku\":\"HIC-60001-00003-00002\",\"pickupPointCode\":\"PP-3010025\",\"reason\":\"Delete not allowed, Item will be left with no active L5\"}");
    bulkProcessData2.setBulkRequestData(REQUEST_JSON);

    DeletePickupPointResponseEventModel deletePickupPointResponseEventModel =
        ConverterUtil.toDeletePickupPointResponseEventModel(BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE,
            BulkProcess.STATUS_ABORTED, Arrays.asList(bulkProcessData1, bulkProcessData2), true);

    Assertions.assertEquals(BUSINESS_PARTNER_CODE, deletePickupPointResponseEventModel.getBusinessPartnerCode());
    Assertions.assertEquals(PICKUP_POINT_CODE, deletePickupPointResponseEventModel.getPickupPointCode());
    Assertions.assertEquals("FAILED", deletePickupPointResponseEventModel.getStatus());
    Assertions.assertEquals("HIC-60001-00003-00001",
        deletePickupPointResponseEventModel.getFailedItemAndReasonResponseList().get(0).getItemSku());
    Assertions.assertEquals("System error",
        deletePickupPointResponseEventModel.getFailedItemAndReasonResponseList().get(0).getReason());
  }

  @Test
  public void toDeletePickupPointResponseEventModelInValidStateStatusTest() throws IOException {
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData1.setErrorMessage(
        "{\"itemSku\":\"HIC-60001-00003-00001\",\"pickupPointCode\":\"PP-3010025\",\"reason\":\"System error\"}");
    bulkProcessData2.setErrorMessage(
        "{\"itemSku\":\"HIC-60001-00003-00002\",\"pickupPointCode\":\"PP-3010025\",\"reason\":\"Delete not allowed, Item will be left with no active L5\"}");
    bulkProcessData2.setBulkRequestData(REQUEST_JSON);

    DeletePickupPointResponseEventModel deletePickupPointResponseEventModel =
        ConverterUtil.toDeletePickupPointResponseEventModel(BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE,
            BulkProcess.STATUS_IN_PROGRESS, Arrays.asList(bulkProcessData1, bulkProcessData2), true);

    Assertions.assertEquals(BUSINESS_PARTNER_CODE, deletePickupPointResponseEventModel.getBusinessPartnerCode());
    Assertions.assertEquals(PICKUP_POINT_CODE, deletePickupPointResponseEventModel.getPickupPointCode());
    Assertions.assertEquals("HIC-60001-00003-00001",
        deletePickupPointResponseEventModel.getFailedItemAndReasonResponseList().get(0).getItemSku());
    Assertions.assertEquals("System error",
        deletePickupPointResponseEventModel.getFailedItemAndReasonResponseList().get(0).getReason());
  }

  @Test
  public void toDeletePickupPointResponseEventModelExceptionTest() throws IOException {
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    BulkProcessData bulkProcessData2 = null;
    bulkProcessData1.setErrorMessage(
        "{\"itemSku\":\"HIC-60001-00003-00001\",\"pickupPointCode\":\"PP-3010025\",\"reason\":\"System error\"}");
      Assertions.assertThrows(RuntimeException.class,
          () -> ConverterUtil.toDeletePickupPointResponseEventModel(BUSINESS_PARTNER_CODE,
              PICKUP_POINT_CODE, STATUS, Arrays.asList(bulkProcessData1, bulkProcessData2), false));
  }

  @Test
  public void toDeletePickupPointResponseEventModelEmptyErrorMessageTest() throws IOException {
    BulkProcessData bulkProcessData1 = new BulkProcessData();
    BulkProcessData bulkProcessData2 = new BulkProcessData();
    bulkProcessData2.setErrorMessage(
        "{\"itemSku\":\"HIC-60001-00003-00002\",\"pickupPointCode\":\"PP-3010025\",\"reason\":\"Delete not allowed, Item will be left with no active L5\"}");
    bulkProcessData2.setBulkRequestData(REQUEST_JSON);

    DeletePickupPointResponseEventModel deletePickupPointResponseEventModel =
        ConverterUtil.toDeletePickupPointResponseEventModel(BUSINESS_PARTNER_CODE, PICKUP_POINT_CODE, STATUS,
            Arrays.asList(bulkProcessData1, bulkProcessData2), false);

    Assertions.assertEquals(BUSINESS_PARTNER_CODE, deletePickupPointResponseEventModel.getBusinessPartnerCode());
    Assertions.assertEquals(PICKUP_POINT_CODE, deletePickupPointResponseEventModel.getPickupPointCode());
    Assertions.assertEquals(STATUS, deletePickupPointResponseEventModel.getStatus());
    Assertions.assertEquals("HIC-60001-00003-00002",
        deletePickupPointResponseEventModel.getFailedItemAndReasonResponseList().get(0).getItemSku());
    Assertions.assertEquals("Delete not allowed, Item will be left with no active L5",
        deletePickupPointResponseEventModel.getFailedItemAndReasonResponseList().get(0).getReason());
  }

  @Test
  public void checkIfMPPIsAllowedNullProfileResponseTest() {
    ConverterUtil.checkIfMPPIsAllowed(CC_MERCHANT, null);
  }

  @Test
  public void checkIfMPPIsAllowedNullCompanyResponseTest() {
    ConverterUtil.checkIfMPPIsAllowed(CC_MERCHANT, profileResponse);
  }

  @Test
  public void checkIfMPPIsAllowedCompanyCncActivatedResponseTest() {
    companyDTO.setCncActivated(true);
    profileResponse.setCompany(companyDTO);
    ConverterUtil.checkIfMPPIsAllowed(CC_MERCHANT, profileResponse);
  }

  @Test
  public void checkIfMPPIsAllowedMultiAddressFalseTest() {
    profileResponse.setCompany(companyDTO);
    ConverterUtil.checkIfMPPIsAllowed(CC_MERCHANT, profileResponse);
  }

  @Test
  public void checkIfMPPIsAllowedMultiAddressTrueTest() {
    profileResponse.setMultiDefaultAddressFlag(true);
    companyDTO.setMerchantType(CC_MERCHANT);
    profileResponse.setCompany(companyDTO);
    ConverterUtil.checkIfMPPIsAllowed(CNC_MERCHANT, profileResponse);
  }

  @Test
  public void constructQrCodeRowInfoTest() {
    ConverterUtil.constructQrCodeRowInfo(downloadQRCodeRequest);
  }

  @Test
  public void constructQrCodeRowInfoWithProductDetailListTest() {
    List<ProductDetailsRequest> productDetailsRequestList = new ArrayList<>();
    ProductDetailsRequest productDetailsRequest = ProductDetailsRequest.builder()
        .pickupPointCode(PICKUP_POINT_CODE)
        .build();
    productDetailsRequestList.add(productDetailsRequest);
    downloadQRCodeRequest.setProductDetailsRequestList(productDetailsRequestList);
    ConverterUtil.constructQrCodeRowInfo(downloadQRCodeRequest);
  }

  @Test
  public void constructQrCodeBulkProcessDataTest() throws Exception {
    ConverterUtil.constructQrCodeBulkProcessData(qrCodeRowInfo, 1, bulkProcess);
  }

  @Test
  public void checkIfMPPIsAllowedNonCncTest() {
    profileResponse.setMultiDefaultAddressFlag(true);
    companyDTO.setMerchantType(TD_MERCHANT);
    profileResponse.setCompany(companyDTO);
    ConverterUtil.checkIfMPPIsAllowed(CNC_MERCHANT, profileResponse);
  }

  @Test
  public void toRestrictedKeywordAddRequestTest() throws JsonProcessingException {
    RestrictedKeywordRequestData restrictedKeywordRequestData =
        RestrictedKeywordRequestData.builder().keyword(KEYWORD).keywordType(KEYWORD_TYPE).keywordAction(KEYWORD_ACTION)
            .categoryCode(CATEGORY_CODE).destinationCategory(DESTINATION_CATEGORY).message(MESSAGE)
            .exclusionList(ImmutableSet.of(EXCLUDE_CATEGORY)).excelRowNumber(EXCEL_ROW_NUMBER).build();
    BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
    bulkInternalProcessData.setData(new ObjectMapper().writeValueAsString(restrictedKeywordRequestData));

    CategoryKeywordUpdateRequestList categoryKeywordUpdateRequestList =
        ConverterUtil.toRestrictedKeywordAdditionRequest(Arrays.asList(bulkInternalProcessData));

    Assertions.assertEquals(KEYWORD, categoryKeywordUpdateRequestList.getAddedKeywords().get(0).getKeyword());
    Assertions.assertEquals(KEYWORD_TYPE, categoryKeywordUpdateRequestList.getAddedKeywords().get(0).getType());
    Assertions.assertEquals(1, categoryKeywordUpdateRequestList.getAddedKeywords().get(0).getAction());
    Assertions.assertEquals(MESSAGE, categoryKeywordUpdateRequestList.getAddedKeywords().get(0).getMessage());
    Assertions.assertEquals(DESTINATION_CATEGORY,
        categoryKeywordUpdateRequestList.getAddedKeywords().get(0).getDestinationCategory());
    Assertions.assertEquals(EXCLUDE_CATEGORY,
        categoryKeywordUpdateRequestList.getAddedKeywords().get(0).getExclusionList().iterator().next());
    Assertions.assertTrue(categoryKeywordUpdateRequestList.getDeletedKeywords().isEmpty());
  }

  @Test
  public void toRestrictedKeywordDeletionRequestTest() throws JsonProcessingException {
    RestrictedKeywordRequestData restrictedKeywordRequestData =
        RestrictedKeywordRequestData.builder().keyword(KEYWORD).keywordType(KEYWORD_TYPE).keywordAction(KEYWORD_ACTION)
            .categoryCode(CATEGORY_CODE).destinationCategory(DESTINATION_CATEGORY).message(MESSAGE)
            .exclusionList(ImmutableSet.of(EXCLUDE_CATEGORY)).excelRowNumber(EXCEL_ROW_NUMBER).build();
    BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
    bulkInternalProcessData.setData(new ObjectMapper().writeValueAsString(restrictedKeywordRequestData));

    CategoryKeywordUpdateRequestList categoryKeywordUpdateRequestList =
        ConverterUtil.toRestrictedKeywordDeletionRequest(Arrays.asList(bulkInternalProcessData));

    Assertions.assertEquals(KEYWORD, categoryKeywordUpdateRequestList.getDeletedKeywords().get(0).getKeyword());
    Assertions.assertEquals(EXCLUDE_CATEGORY,
        categoryKeywordUpdateRequestList.getDeletedKeywords().get(0).getExclusionList().iterator().next());
    Assertions.assertTrue(categoryKeywordUpdateRequestList.getAddedKeywords().isEmpty());
  }

  @Test
  public void populateBulkProcessStoreTest() throws Exception {
    downloadQRCodeRequest.setQrGenerationType("STORE");
    BulkProcess bulkProcess = ConverterUtil.populateBulkProcess(downloadQRCodeRequest);
    Assertions.assertEquals(BulkProcessType.QR_GENERATION.getValue(), bulkProcess.getBulkProcessType());
    Assertions.assertEquals("READY_TO_PROCESS", bulkProcess.getStatus());
    Assertions.assertNotNull(bulkProcess.getNotes());
  }

  @Test
  public void populateBulkProcessAllProductsTest() throws Exception {
    downloadQRCodeRequest.setQrGenerationType("ALL_PRODUCTS");
    BulkProcess bulkProcess = ConverterUtil.populateBulkProcess(downloadQRCodeRequest);
    Assertions.assertEquals(BulkProcessType.QR_GENERATION.getValue(), bulkProcess.getBulkProcessType());
    Assertions.assertEquals("READY_TO_PROCESS", bulkProcess.getStatus());
    Assertions.assertNotNull(bulkProcess.getNotes());
  }

  @Test
  public void populateBulkProcessProductTest() throws Exception {
    downloadQRCodeRequest.setQrGenerationType("PRODUCT");
    BulkProcess bulkProcess = ConverterUtil.populateBulkProcess(downloadQRCodeRequest);
    Assertions.assertEquals(BulkProcessType.QR_GENERATION.getValue(), bulkProcess.getBulkProcessType());
    Assertions.assertEquals("PENDING", bulkProcess.getStatus());
    Assertions.assertNotNull(bulkProcess.getNotes());
  }

  @Test
  public void getShippingEligibilitySwitchOffTest() {
    ShippingTypeEligibility shippingTypeEligibility =
      ConverterUtil.getShippingEligibility(profileResponse, false);
    Assertions.assertTrue(shippingTypeEligibility.isEligibleForBopisProduct());
    Assertions.assertTrue(shippingTypeEligibility.isEligibleForBigProduct());
  }

  @Test
  public void getShippingEligibilityBothTrueTest() {
    profileResponse.setBigProductFlag(Boolean.TRUE);
    profileResponse.setBopisFlag(Boolean.TRUE);
    ShippingTypeEligibility shippingTypeEligibility =
      ConverterUtil.getShippingEligibility(profileResponse, true);
    Assertions.assertTrue(shippingTypeEligibility.isEligibleForBopisProduct());
    Assertions.assertTrue(shippingTypeEligibility.isEligibleForBigProduct());
  }

  @Test
  public void getShippingEligibilityBothFalseTest() {
    profileResponse.setBigProductFlag(Boolean.FALSE);
    profileResponse.setBopisFlag(Boolean.FALSE);
    ShippingTypeEligibility shippingTypeEligibility =
      ConverterUtil.getShippingEligibility(profileResponse, true);
    Assertions.assertFalse(shippingTypeEligibility.isEligibleForBopisProduct());
    Assertions.assertFalse(shippingTypeEligibility.isEligibleForBigProduct());
  }
  @Test
  public void checkIfEligibleForShippingTypeSwitchOffTest() {
    Assertions.assertTrue(ConverterUtil.checkIfEligibleForShippingType(ProductType.BIGPRODUCT.getCode(),
      profileResponse, false));
  }

  @Test
  public void checkIfEligibleForShippingTypeBigProductChecksTest() {
    profileResponse.setBigProductFlag(Boolean.TRUE);
    Assertions.assertTrue(ConverterUtil.checkIfEligibleForShippingType(ProductType.BIGPRODUCT.getCode(),
      profileResponse, true));
    profileResponse.setBigProductFlag(Boolean.FALSE);
    Assertions.assertFalse(ConverterUtil.checkIfEligibleForShippingType(ProductType.BIGPRODUCT.getCode(),
      profileResponse, true));
  }

  @Test
  public void checkIfEligibleForShippingTypeBopisChecksTest() {
    profileResponse.setBopisFlag(Boolean.TRUE);
    Assertions.assertTrue(ConverterUtil.checkIfEligibleForShippingType(ProductType.BOPIS.getCode(),
      profileResponse, true));
    profileResponse.setBopisFlag(Boolean.FALSE);
    Assertions.assertFalse(ConverterUtil.checkIfEligibleForShippingType(ProductType.BOPIS.getCode(),
      profileResponse, true));
  }

  @Test
  public void covertToWorkOrderEventModelTest() throws Exception {
    BulkProcessDataDTO processDataDTO = new BulkProcessDataDTO();
    processDataDTO.setId("id");
    processDataDTO.setBulkProcessCode(DEFAULT_BULK_PROCESS_CODE);
    processDataDTO.setParentCode(ITEM_SKU);
    WorkOrderEventModel workOrderEventModel =
      ConverterUtil.convertToWorkOrderEventModel(Constant.STORE_ID, processDataDTO);
    Assertions.assertEquals(workOrderEventModel.getBulkProcessCode(), DEFAULT_BULK_PROCESS_CODE);
  }

  @Test
  public void setBulkProcessStatusTest() throws Exception{
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(BulkProcess.COLUMN_BULK_PROCESS_CODE);
    ConverterUtil.setBulkProcessStatus(bulkProcess, 4, 4);
    Assertions.assertEquals(BulkProcess.STATUS_FINISHED, bulkProcess.getStatus());
  }

  @Test
  public void setBulkProcessStatusAbortedTest() throws Exception{
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(BulkProcess.COLUMN_BULK_PROCESS_CODE);
    ConverterUtil.setBulkProcessStatus(bulkProcess, 4, 0);
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcess.getStatus());
  }

  @Test
  public void setBulkProcessStatusPartiallyDoneTest() throws Exception{
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(BulkProcess.COLUMN_BULK_PROCESS_CODE);
    ConverterUtil.setBulkProcessStatus(bulkProcess, 4, 2);
    Assertions.assertEquals(BulkProcess.STATUS_PARTIALLY_DONE, bulkProcess.getStatus());
  }

  @Test
  public void testSetBulkProcessNotesForWorkOrderUpload_WithExistingNotes() {
    BulkProcess bulkProcess = new BulkProcess();
    BulkProcessNotes existingNotes = new BulkProcessNotes();
    List<BulkProcessNotes> existingNotesList = new ArrayList<>();
    existingNotesList.add(existingNotes);
    bulkProcess.setBulkProcessNotes(existingNotesList);

    List<BulkUpdateErrorDTO> errorDTOList = new ArrayList<>();
    BulkUpdateErrorDTO errorDTO = new BulkUpdateErrorDTO();
    errorDTO.setProductSku("SKU123");
    errorDTO.setReason("Reason");
    errorDTOList.add(errorDTO);
    ConverterUtil.setBulkProcessNotesForWorkOrderUpload(errorDTOList, Constant.STORE_ID, bulkProcess);
    Assertions.assertEquals(2, bulkProcess.getBulkProcessNotes().size());
  }

  @Test
  public void testSetBulkProcessNotesForWorkOrderUpload_WithNoExistingNotes() {
    BulkProcess bulkProcess = new BulkProcess();
    List<BulkUpdateErrorDTO> errorDTOList = new ArrayList<>();
    BulkUpdateErrorDTO errorDTO = new BulkUpdateErrorDTO();
    errorDTO.setProductSku("SKU123");
    errorDTO.setReason("Reason");
    errorDTOList.add(errorDTO);
    ConverterUtil.setBulkProcessNotesForWorkOrderUpload(errorDTOList, "storeId", bulkProcess);
    Assertions.assertEquals(1, bulkProcess.getBulkProcessNotes().size());
  }

  @Test
  public void testSetBulkProcessNotesForWorkOrderUpload_InternationalMerchant() {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setInternationalMerchant(true);
    List<BulkUpdateErrorDTO> errorDTOList = new ArrayList<>();
    BulkUpdateErrorDTO errorDTO = new BulkUpdateErrorDTO();
    errorDTO.setProductSku("SKU123");
    errorDTO.setReason("Reason");
    errorDTOList.add(errorDTO);
    ConverterUtil.setBulkProcessNotesForWorkOrderUpload(errorDTOList, Constant.STORE_ID, bulkProcess);
    Assertions.assertEquals("Blibli SKU: SKU123, Failure Reason: Reason",
      bulkProcess.getBulkProcessNotes().get(0).getNotes());
  }

  @Test
  public void testSetBulkProcessNotesForWorkOrderUpload_NonInternationalMerchant() {
    // Setup
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setInternationalMerchant(false);
    List<BulkUpdateErrorDTO> errorDTOList = new ArrayList<>();
    BulkUpdateErrorDTO errorDTO = new BulkUpdateErrorDTO();
    errorDTO.setProductSku("SKU123");
    errorDTO.setReason("Reason");
    errorDTOList.add(errorDTO);
    ConverterUtil.setBulkProcessNotesForWorkOrderUpload(errorDTOList, Constant.STORE_ID, bulkProcess);
    Assertions.assertEquals("Blibli SKU: SKU123, Alasan Kegagalan: Reason",
      bulkProcess.getBulkProcessNotes().get(0).getNotes());
  }

  @Test
  public void testSetBulkProcessNotesForWorkOrderUpload_NoExistingNotes_FalseCase() {
    BulkProcess bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessNotes(null);
    List<BulkUpdateErrorDTO> errorDTOList = new ArrayList<>();
    BulkUpdateErrorDTO errorDTO = new BulkUpdateErrorDTO();
    errorDTO.setProductSku("SKU123");
    errorDTO.setReason("Reason");
    errorDTOList.add(errorDTO);
    ConverterUtil.setBulkProcessNotesForWorkOrderUpload(errorDTOList, Constant.STORE_ID, bulkProcess);
  }

  @Test
  public void testGetVariableValueByProcessTypeForAbortion() {
    Assertions.assertEquals(SystemParameterConfigNames.PRODUCT_CREATION_UPLOAD_STRUCK_PROCESS_IN_MINUTES,
      ConverterUtil.getVariableValueByProcessTypeForAbortion(BulkProcessType.PRODUCT_CREATION_UPLOAD.getValue()));

    Assertions.assertEquals(SystemParameterConfigNames.PRODUCT_CREATION_UPLOAD_PRIORITY_ABORT_STRUCK_PROCESS_IN_MINUTES,
      ConverterUtil.getVariableValueByProcessTypeForAbortion(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY.getValue()));

    Assertions.assertEquals(SystemParameterConfigNames.PRODUCT_CREATION_UPLOAD_PRIORITY_1_ABORT_STRUCK_PROCESS_IN_MINUTES,
      ConverterUtil.getVariableValueByProcessTypeForAbortion(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_1.getValue()));

    Assertions.assertEquals(SystemParameterConfigNames.PRODUCT_LEVEL3_ABORT_STRUCK_PROCESS_IN_MINUTES,
      ConverterUtil.getVariableValueByProcessTypeForAbortion(BulkProcessType.PRODUCT_LEVEL_3.getValue()));

    Assertions.assertEquals(SystemParameterConfigNames.PRODUCT_LEVEL3_UPDATE_PRIORITY1_ABORT_STRUCK_PROCESS_IN_MINUTES,
      ConverterUtil.getVariableValueByProcessTypeForAbortion(BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_1.getValue()));

    Assertions.assertEquals(SystemParameterConfigNames.PRODUCT_LEVEL3_UPDATE_PRIORITY2_ABORT_STRUCK_PROCESS_IN_MINUTES,
      ConverterUtil.getVariableValueByProcessTypeForAbortion(BulkProcessType.PRODUCT_LEVEL_3_UPDATE_PRIORITY_2.getValue()));

    Assertions.assertEquals(SystemParameterConfigNames.PRODUCT_LEVEL_3_GENERIC_STRUCK_PROCESS_IN_MINUTES,
      ConverterUtil.getVariableValueByProcessTypeForAbortion(BulkProcessType.PRODUCT_LEVEL_3_GENERIC.getValue()));

    Assertions.assertEquals(SystemParameterConfigNames.INSTANT_PICKUP_PRODUCT_UPSERT_ABORT_STRUCK_PROCESS_IN_MINUTES,
      ConverterUtil.getVariableValueByProcessTypeForAbortion(BulkProcessType.INSTANT_PICKUP_PRODUCT_UPSERT.getValue()));

    Assertions.assertEquals(SystemParameterConfigNames.INSTANT_PICKUP_PRODUCT_DELETE_ABORT_STRUCK_PROCESS_IN_MINUTES,
      ConverterUtil.getVariableValueByProcessTypeForAbortion(BulkProcessType.INSTANT_PICKUP_PRODUCT_DELETE.getValue()));

    Assertions.assertEquals(SystemParameterConfigNames.IN_STORE_ABORT_STRUCK_PROCESS_IN_MINUTES,
      ConverterUtil.getVariableValueByProcessTypeForAbortion(BulkProcessType.IN_STORE.getValue()));

    Assertions.assertEquals(SystemParameterConfigNames.DELETE_PICKUP_POINT_STRUCK_PROCESS_IN_MINUTES,
      ConverterUtil.getVariableValueByProcessTypeForAbortion(BulkProcessType.DELETE_PICKUP_POINT.getValue()));

    Assertions.assertEquals(SystemParameterConfigNames.TRANSFER_REQUEST_STRUCK_PROCESS_IN_MINUTES,
      ConverterUtil.getVariableValueByProcessTypeForAbortion(BulkProcessType.TRANSFER_REQUEST.getValue()));

    Assertions.assertEquals(SystemParameterConfigNames.ASSEMBLY_REQUEST_STRUCK_PROCESS_IN_MINUTES,
      ConverterUtil.getVariableValueByProcessTypeForAbortion(BulkProcessType.ASSEMBLY_REQUEST.getValue()));

    Assertions.assertEquals(SystemParameterConfigNames.ARCHIVE_ABORT_STRUCK_PROCESS_IN_MINUTES,
      ConverterUtil.getVariableValueByProcessTypeForAbortion(BulkProcessType.ARCHIVE.getValue()));

    Assertions.assertEquals(SystemParameterConfigNames.SUBJECT_TO_VAT_PROCESS_IN_MINUTES,
      ConverterUtil.getVariableValueByProcessTypeForAbortion(BulkProcessType.SUBJECT_TO_VAT.getValue()));

    Assertions.assertEquals(SystemParameterConfigNames.CAMPAIGN_STRUCK_PROCESS_IN_MINUTES,
      ConverterUtil.getVariableValueByProcessTypeForAbortion(BulkProcessType.CAMPAIGN.getValue()));

    Assertions.assertEquals(SystemParameterConfigNames.DISASSEMBLY_REQUEST_STRUCK_PROCESS_IN_MINUTES,
      ConverterUtil.getVariableValueByProcessTypeForAbortion(BulkProcessType.DISASSEMBLY_REQUEST.getValue()));
    Assertions.assertEquals(SystemParameterConfigNames.PRODUCT_CREATION_UPLOAD_PRIORITY_2_ABORT_STRUCK_PROCESS_IN_MINUTES,
      ConverterUtil.getVariableValueByProcessTypeForAbortion(BulkProcessType.PRODUCT_CREATION_UPLOAD_PRIORITY_2.getValue()));

    Assertions.assertEquals(SystemParameterConfigNames.CONVERTED_PRODUCT_CREATION_UPLOAD_ABORT_STRUCK_PROCESS_IN_MINUTES,
      ConverterUtil.getVariableValueByProcessTypeForAbortion(BulkProcessType.CONVERTED_PRODUCT_CREATION_UPLOAD.getValue()));
  }

  @Test
  public void checkIfSellerIsAllowedForPriceUpdateTest() {
    ConverterUtil.checkIfSellerIsAllowedForPriceUpdate(null, TD_MERCHANT);
    ConverterUtil.checkIfSellerIsAllowedForPriceUpdate(new ProfileResponse(), TD_MERCHANT);
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(TD_MERCHANT);
    profileResponse.setCompany(companyDTO);
    ConverterUtil.checkIfSellerIsAllowedForPriceUpdate(profileResponse, TD_MERCHANT);
  }

  @Test
  public void checkIfSellerIsAllowedForPriceUpdateExceptionTest() {
    ProfileResponse profileResponse = new ProfileResponse();
    CompanyDTO companyDTO = new CompanyDTO();
    companyDTO.setMerchantType(TD_MERCHANT);
    profileResponse.setCompany(companyDTO);
    Assertions.assertThrows(RuntimeException.class,
        () -> ConverterUtil.checkIfSellerIsAllowedForPriceUpdate(profileResponse, CNC_MERCHANT));
  }


  @Test
  public void convertToGenericTemplateFileTypeTest() {
    GenericTemplateFileType genericTemplateFileType = ConverterUtil.convertToGenericTemplateFileType(CNC_MERCHANT);
    Assertions.assertNull(genericTemplateFileType);
    genericTemplateFileType = ConverterUtil.convertToGenericTemplateFileType(null);
    Assertions.assertNull(genericTemplateFileType);
    genericTemplateFileType = ConverterUtil.convertToGenericTemplateFileType(GenericTemplateFileType.PURE_DELIVERY_FILE.name());
    Assertions.assertEquals(GenericTemplateFileType.PURE_DELIVERY_FILE, genericTemplateFileType);
  }

  }
