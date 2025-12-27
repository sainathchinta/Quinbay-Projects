package com.gdn.mta.bulk.helper;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.mta.bulk.models.download.responsedata.SkuRebateResponse;
import com.gdn.mta.bulk.service.FileStorageOperationsService;
import com.gdn.partners.bulk.util.Constant;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkPriceRecommendationDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.CampaignPriceInfo;
import com.gdn.mta.bulk.models.download.responsedata.DownloadSkuResponse;
import com.gdn.mta.bulk.response.BulkPriceAnalyticsResponse;
import com.gdn.mta.bulk.service.GCSService;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.google.common.collect.ImmutableList;

public class BulkPriceRecommendationProcessHelperTest {

  private static final String ITEM_SKU = "Item Sku";
  private static final String PICKUP_POINT = "Pickup point Code";
  private static final String PICKUP_POINT_NAME = "Pickup point Name";
  private static final String REQUEST_ID = "requestId";
  private static final String CAMPAIGN_CODE = "CAMP-123";
  public static final String TEMPLATE_BULK_PRICE_RECOMMENDATION_BASE_TEMPLATE_XLSX =
      "template/BulkPriceRecommendationBaseTemplate.xlsx";
  private static final String BULK_PRICE_RECOMMENDATION_DOWNLOAD_TEMPLATE_ID =
      "BULK_PRICE_RECOMMENDATION_DOWNLOAD_TEMPLATE_ID";
  private static final String EXPIRY_DATE = "4th June, 2025";
  private static final String EXPIRING_QUANTITY = "10";

  @InjectMocks
  private BulkPriceRecommendationProcessHelper bulkPriceRecommendationProcessHelper;

  @Mock
  private GCSService gcsService;

  @Mock
  private FileStorageOperationsService fileStorageOperationsService;

  private Workbook workbook;
  private BulkDownloadRequest bulkDownloadRequest;
  private static final List<String> HEADER_LIST =
      ImmutableList.<String>builder().add(ITEM_SKU).add(PICKUP_POINT).build();

  private BulkPriceRecommendationDataResponse bulkPriceRecommendationDataResponse;
  private BulkPriceAnalyticsResponse bulkPriceAnalyticsResponse;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    bulkDownloadRequest = new BulkDownloadRequest();
    bulkDownloadRequest.setBulkProcessEntity(BulkProcessEntity.DELETE_UPDATE_PICKUP_POINTS);
    bulkDownloadRequest.setRequestId(REQUEST_ID);
    workbook = new SXSSFWorkbook();
    bulkPriceRecommendationDataResponse = new BulkPriceRecommendationDataResponse();
    bulkPriceAnalyticsResponse = new BulkPriceAnalyticsResponse();
    DownloadSkuResponse downloadSkuResponse = new DownloadSkuResponse();
    downloadSkuResponse.setItemSku(ITEM_SKU);
    downloadSkuResponse.setPickupPointCode("pickupPointCode");
    downloadSkuResponse.setPickupPointName(PICKUP_POINT_NAME);
    downloadSkuResponse.setItemSkuName("itemSkuName");
    downloadSkuResponse.setStoreName("storeName");
    downloadSkuResponse.setCnCategoryName("cnCategoryName");
    downloadSkuResponse.setC1CategoryName("c1CategoryName");
    downloadSkuResponse.setBrandName("brandName");
    downloadSkuResponse.setProductType("productType");
    downloadSkuResponse.setListPrice(10.0);
    downloadSkuResponse.setMinMargin(5.0f);
    downloadSkuResponse.setMaxMargin(15.0f);
    downloadSkuResponse.setCogs(8.0f);
    downloadSkuResponse.setVatInclusive(true);
    downloadSkuResponse.setRebate(2.0f);
    downloadSkuResponse.setOfferPrice(12.0);
    downloadSkuResponse.setBmlPrice(10.0);
    downloadSkuResponse.setTotalOrders(100L);
    downloadSkuResponse.setTotalTPV(500L);
    downloadSkuResponse.setL5Stock(50L);
    downloadSkuResponse.setExpiryDate("4th June, 2025");
    downloadSkuResponse.setExpiringQuantity(10L);
    downloadSkuResponse.setL2Stock(20L);
    downloadSkuResponse.setL2DaysOfInventory(30L);
    downloadSkuResponse.setL2TargetDaysOfInventory(25L);
    CampaignPriceInfo campaignPriceInfo = CampaignPriceInfo.builder()
        .registeredInCampaign(true)
        .lockPriceUpdate(true)
        .campaignCode(CAMPAIGN_CODE)
        .campaignPrice(100D)
        .maxCampaignPrice(100D)
        .minCampaignPrice(20D)
        .lockCampaignPriceUpdate(false)
        .build();
    downloadSkuResponse.setCampaignPriceInfo(campaignPriceInfo);
    SkuRebateResponse rebateBreakdown = SkuRebateResponse.builder()
        .sellerLevel(1.0)
        .sellerCategoryLevel(0.0)
        .sellerCategoryBrandLevel(0.0)
        .sellerBrandLevel(1.0)
        .build();
    downloadSkuResponse.setRebateBreakdown(rebateBreakdown);
    downloadSkuResponse.setRebateOverwrittenByOfficer(false);
    bulkPriceAnalyticsResponse.setDownloadSkuResponseList(Collections.singletonList(downloadSkuResponse));
  }

  @Test
  public void getCsvHeadersMapTest() {
    Assertions.assertNull(bulkPriceRecommendationProcessHelper.getCsvHeadersMap(bulkDownloadRequest));
  }

  @Test
  public void getHeaderListTest() throws Exception {
    List<String> headerList = bulkPriceRecommendationProcessHelper.getHeaderList(bulkPriceRecommendationDataResponse);
    Assertions.assertEquals(BulkParameters.PRICE_RECOMMENDATION_HEADERS, headerList);
  }

  @Test
  public void getRowDataTest() {
    List<List<String>> rowData = bulkPriceRecommendationProcessHelper.getRowData(bulkPriceAnalyticsResponse);
    Assertions.assertEquals(44, rowData.get(0).size());
    Assertions.assertEquals(ITEM_SKU, rowData.get(0).get(0));
    Assertions.assertEquals(Constant.YES, rowData.get(0).get(27));
    Assertions.assertEquals(Constant.SELLING_PRICE_CHANGE_NOT_ALLOWED, rowData.get(0).get(34));
    Assertions.assertEquals(String.format(Constant.REBATE_HISTORY, 1.0, 0.0, 1.0, 0.0), rowData.get(0).get(16));
    Assertions.assertEquals(Constant.YES, rowData.get(0).get(14));
    Assertions.assertEquals(EXPIRY_DATE, rowData.get(0).get(39));
    Assertions.assertEquals(EXPIRING_QUANTITY, rowData.get(0).get(40));
  }

  @Test
  public void getRowData_whenSkuNotRegisteredInCampaignTest() {
    bulkPriceAnalyticsResponse.getDownloadSkuResponseList().get(0).getCampaignPriceInfo().setRegisteredInCampaign(false);
    List<List<String>> rowData = bulkPriceRecommendationProcessHelper.getRowData(bulkPriceAnalyticsResponse);
    Assertions.assertEquals(44, rowData.get(0).size());
    Assertions.assertEquals(ITEM_SKU, rowData.get(0).get(0));
    Assertions.assertEquals(PICKUP_POINT_NAME, rowData.get(0).get(2));
    Assertions.assertEquals(Constant.NO, rowData.get(0).get(27));
    Assertions.assertEquals(org.apache.commons.lang.StringUtils.EMPTY, rowData.get(0).get(25));
    Assertions.assertEquals(Constant.YES, rowData.get(0).get(14));
  }

  @Test
  public void getRowData_whenSkuPriceUpdateAllowedTest() {
    bulkPriceAnalyticsResponse.getDownloadSkuResponseList().get(0).getCampaignPriceInfo().setLockPriceUpdate(false);
    bulkPriceAnalyticsResponse.getDownloadSkuResponseList().get(0).getCampaignPriceInfo().setMinAllowedPrice(10);
    bulkPriceAnalyticsResponse.getDownloadSkuResponseList().get(0).getCampaignPriceInfo().setMaxAllowedPrice(20);
    List<List<String>> rowData = bulkPriceRecommendationProcessHelper.getRowData(bulkPriceAnalyticsResponse);
    Assertions.assertEquals(ITEM_SKU, rowData.get(0).get(0));
    Assertions.assertEquals(Constant.YES, rowData.get(0).get(27));
    Assertions.assertEquals(String.format(Constant.PRICE_RANGE, 10, 20), rowData.get(0).get(34));
    Assertions.assertEquals(Constant.YES, rowData.get(0).get(14));
  }

  @Test
  public void getRowData_whenCampaignPriceInfoNullTest() {
    bulkPriceAnalyticsResponse.getDownloadSkuResponseList().get(0).setCampaignPriceInfo(null);
    List<List<String>> rowData = bulkPriceRecommendationProcessHelper.getRowData(bulkPriceAnalyticsResponse);
    Assertions.assertEquals(44, rowData.get(0).size());
    Assertions.assertEquals(ITEM_SKU, rowData.get(0).get(0));
    Assertions.assertEquals(org.apache.commons.lang.StringUtils.EMPTY, rowData.get(0).get(22));
    Assertions.assertEquals(org.apache.commons.lang.StringUtils.EMPTY, rowData.get(0).get(23));
    Assertions.assertEquals(Constant.YES, rowData.get(0).get(14));
  }

  @Test
  public void getRowDataTest_whenRebateOverwrittenByOfficer() {
    bulkPriceAnalyticsResponse.getDownloadSkuResponseList().get(0).setRebateOverwrittenByOfficer(true);
    List<List<String>> rowData = bulkPriceRecommendationProcessHelper.getRowData(bulkPriceAnalyticsResponse);
    Assertions.assertEquals(44, rowData.get(0).size());
    Assertions.assertEquals(Constant.INPUT_BY_BR, rowData.get(0).get(16));
    Assertions.assertEquals(Constant.YES, rowData.get(0).get(14));
  }

  @Test
  public void getRowDataTest_whenVatInclusiveFalseTest() {
    bulkPriceAnalyticsResponse.getDownloadSkuResponseList().get(0).setVatInclusive(false);
    List<List<String>> rowData = bulkPriceRecommendationProcessHelper.getRowData(bulkPriceAnalyticsResponse);
    Assertions.assertEquals(44, rowData.get(0).size());
    Assertions.assertEquals(Constant.NO, rowData.get(0).get(14));
  }

  @Test
  public void getRowDataTest_whenCogsAndVatInclusiveNullTest() {
    bulkPriceAnalyticsResponse.getDownloadSkuResponseList().get(0).setCogs(null);
    bulkPriceAnalyticsResponse.getDownloadSkuResponseList().get(0).setVatInclusive(null);
    List<List<String>> rowData = bulkPriceRecommendationProcessHelper.getRowData(bulkPriceAnalyticsResponse);
    Assertions.assertEquals(44, rowData.get(0).size());
    Assertions.assertEquals(org.apache.commons.lang.StringUtils.EMPTY, rowData.get(0).get(13));
    Assertions.assertEquals(org.apache.commons.lang.StringUtils.EMPTY, rowData.get(0).get(14));
  }

  @Test
  public void getRowDataTest_whenExpiryDateAndExpiringQuantityNullTest() {
    bulkPriceAnalyticsResponse.getDownloadSkuResponseList().get(0).setExpiryDate(null);
    bulkPriceAnalyticsResponse.getDownloadSkuResponseList().get(0).setExpiringQuantity(null);
    List<List<String>> rowData = bulkPriceRecommendationProcessHelper.getRowData(bulkPriceAnalyticsResponse);
    Assertions.assertEquals(44, rowData.get(0).size());
    Assertions.assertEquals(org.apache.commons.lang.StringUtils.EMPTY, rowData.get(0).get(39));
    Assertions.assertEquals(org.apache.commons.lang.StringUtils.EMPTY, rowData.get(0).get(40));
  }

  @Test
  public void getDirectoryTest() {
    Assertions.assertEquals(ProcessorUtils.BULK_PRICE_RECOMMENDATION_DOWNLOAD_TEMPLATE_DIR + REQUEST_ID,
        bulkPriceRecommendationProcessHelper.getDirectory(bulkDownloadRequest));
  }

  @Test
  public void getEmailParamsTest() {
    Map<String, Object> emailParams = new HashMap<>();
    emailParams = bulkPriceRecommendationProcessHelper.getEmailParams(bulkDownloadRequest, "eng");
    Assertions.assertEquals(BULK_PRICE_RECOMMENDATION_DOWNLOAD_TEMPLATE_ID,
        emailParams.get(EmailConstants.TEMPLATE_ID_PARAM));
  }

  @Test
  public void getRecordsUpdatedTest() {
    Assertions.assertEquals(0, bulkPriceRecommendationProcessHelper.getRecordsUpdated(bulkPriceRecommendationDataResponse));
    BulkPriceAnalyticsResponse bulkPriceAnalyticsResponse1 = new BulkPriceAnalyticsResponse();
    Assertions.assertEquals(0, bulkPriceRecommendationProcessHelper.getRecordsUpdated(null));
    Assertions.assertEquals(0, bulkPriceRecommendationProcessHelper.getRecordsUpdated(bulkPriceRecommendationDataResponse));
    Assertions.assertEquals(0, bulkPriceRecommendationProcessHelper.getRecordsUpdated(bulkPriceAnalyticsResponse1));
    Assertions.assertEquals(1, bulkPriceRecommendationProcessHelper.getRecordsUpdated(bulkPriceAnalyticsResponse));
  }

  @Test
  public void generateDataSheet() throws IOException {
    List<String> headerList = BulkParameters.PRICE_RECOMMENDATION_HEADERS;
    List<List<String>> rowDataList = Arrays.asList(
        Arrays.asList("value1", "value2", "10", "11", "12", "11.1", "122", "10", "11", "12", "11.1", "122", "10", "11",
            "12", "11.1", "122", "10", "11", "12", "11.1", "122", "10", "11", "12", "11.1", "122"),
        Arrays.asList("value1", "value2", "10", "11", "12", "11.1", "122", "10", "", "12", "11.1", "122", "10", "",
            "12", "11.1", "122", "10", "11", "12", "11.1", "122", "10", "11", "12", "11.1", "122"));
    XSSFWorkbook workbook = new XSSFWorkbook();
    workbook.createSheet("Sheet1");
    for (int i = 0; i < 27; i++) {
      Row row = workbook.getSheet("Sheet1").createRow(i);
      for (int j = 0; j < 27; j++) {
        Cell cell = row.createCell(j);
      }
    }
    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
    workbook.write(outputStream);
    byte[] bytes = outputStream.toByteArray();
    Mockito.when(gcsService.downloadFile("bucket", TEMPLATE_BULK_PRICE_RECOMMENDATION_BASE_TEMPLATE_XLSX))
        .thenReturn(bytes);
    Mockito.when(fileStorageOperationsService.downloadBaseTemplateForPriceRecommendation()).thenReturn(bytes);
    Workbook workbook1 = bulkPriceRecommendationProcessHelper.generateDataSheet(headerList, rowDataList, 1);
    Row firstRow = workbook1.getSheet("Sheet1").getRow(7);
    Cell firstCell = firstRow.getCell(0);
    Mockito.verify(fileStorageOperationsService).downloadBaseTemplateForPriceRecommendation();
    Assertions.assertEquals("value1", firstCell.getStringCellValue());
  }

  @Test
  public void modifyWorkbookTest() throws Exception {
    bulkPriceRecommendationProcessHelper.modifyWorkbook(new XSSFWorkbook(), new BulkDataResponse());
  }

  @AfterEach
  public void tearDown(){
    Mockito.verifyNoMoreInteractions(gcsService);
  }

}
