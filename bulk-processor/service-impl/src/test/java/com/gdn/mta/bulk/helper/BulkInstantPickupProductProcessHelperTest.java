package com.gdn.mta.bulk.helper;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.poi.ss.usermodel.Workbook;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;

import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkInstantPickupProductResponse;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.partners.pbp.dto.offlineitem.OfflineItemInstantPickupBulkDownloadResponse;

public class BulkInstantPickupProductProcessHelperTest {

  private static final byte[] CONTENT = new byte[100];
  private static final String FILE_PATH = "filePath";

  private static final String BLIBLI_SKU = "SKU-00001-00001-00001";
  private static final String PRODUCT_NAME = "SKU Product Name";
  private static final String PICKUP_POINT_CODE = "PP-1234567";
  private static final String PICKUP_POINT_NAME = "PP Name";
  private static final String LIST_PRICE = "50000";
  private static final String OFFER_PRICE = "25000";
  private static final String STOCK = "1";

  @InjectMocks
  private BulkInstantPickupProductProcessHelper bulkInstantPickupProductProcessHelper;

  @BeforeEach
  public void setUp() {
    initMocks(this);
  }

  @Test
  public void testGetCsvHeadersMap() {
    BulkDownloadRequest bulkDownloadRequest = new BulkDownloadRequest();
    BulkCsvModel bulkCsvModel =
        this.bulkInstantPickupProductProcessHelper.getCsvHeadersMap(bulkDownloadRequest);
    Assertions.assertNull(bulkCsvModel);
  }

  @Test
  public void testGenerateCsvFile() throws Exception {
    BulkCsvModel bulkCsvModels = new BulkCsvModel(new ArrayList<>(), new HashMap<>());
    BulkDataResponse bulkDataResponse = new BulkDataResponse();
    byte[] bulkCsvModel =
        this.bulkInstantPickupProductProcessHelper.generateCsvFile(bulkCsvModels, bulkDataResponse);
    assertNotNull(bulkCsvModel);
  }

  @Test
  public void testGetHeaderList() {
    BulkDataResponse bulkDataResponse = new BulkDataResponse();
    List<String> response =
        this.bulkInstantPickupProductProcessHelper.getHeaderList(bulkDataResponse);
    assertNotNull(response);
  }

  @Test
  public void testGenerateDataSheet() {
    List<String> headers = new ArrayList<>();
    headers.add(BulkParameters.BLIBLI_SKU);
    headers.add(BulkParameters.PRODUCT_NAME_NON_EDITABLE);
    headers.add(BulkParameters.PICKUP_POINT_CODE);
    headers.add(BulkParameters.PICKUP_POINT_NAME_NON_EDITABLE);
    headers.add(BulkParameters.LIST_PRICE);
    headers.add(BulkParameters.OFFER_PRICE);
    headers.add(BulkParameters.STOCK);
    List<List<String>> rowDataList = new ArrayList<>();
    List<String> rowDatumList = new ArrayList<>();
    rowDatumList.add(BLIBLI_SKU);
    rowDatumList.add(PRODUCT_NAME);
    rowDatumList.add(PICKUP_POINT_CODE);
    rowDatumList.add(PICKUP_POINT_NAME);
    rowDatumList.add(LIST_PRICE);
    rowDatumList.add(OFFER_PRICE);
    rowDatumList.add(STOCK);
    rowDataList.add(rowDatumList);
    Workbook response = this.bulkInstantPickupProductProcessHelper.generateDataSheet(headers, rowDataList, 1);
    assertNotNull(response);
    Assertions.assertEquals(BLIBLI_SKU, response.getSheetAt(0).getRow(1).getCell(0).getStringCellValue());
    Assertions.assertEquals(PRODUCT_NAME, response.getSheetAt(0).getRow(1).getCell(1).getStringCellValue());
    Assertions.assertEquals(PICKUP_POINT_CODE, response.getSheetAt(0).getRow(1).getCell(2).getStringCellValue());
    Assertions.assertEquals(PICKUP_POINT_NAME, response.getSheetAt(0).getRow(1).getCell(3).getStringCellValue());
    Assertions.assertEquals(LIST_PRICE, response.getSheetAt(0).getRow(1).getCell(4).getStringCellValue());
    Assertions.assertEquals(OFFER_PRICE, response.getSheetAt(0).getRow(1).getCell(5).getStringCellValue());
    Assertions.assertEquals(STOCK, response.getSheetAt(0).getRow(1).getCell(6).getStringCellValue());
  }

  @Test
  public void testGetUserName() {
    String response = this.bulkInstantPickupProductProcessHelper.getUserName("target");
    assertNotNull(response);
  }

  @Test
  public void testModifyWorkbook() throws Exception {
    BulkDataResponse bulkDataResponse = new BulkDataResponse();
    Workbook workbook = mock(Workbook.class);
    this.bulkInstantPickupProductProcessHelper.modifyWorkbook(workbook, bulkDataResponse);
  }

  @Test
  public void testGetRowData() {
    List<OfflineItemInstantPickupBulkDownloadResponse> responses = new ArrayList<>();
    OfflineItemInstantPickupBulkDownloadResponse response =
        new OfflineItemInstantPickupBulkDownloadResponse();
    responses.add(response);
    BulkInstantPickupProductResponse bulkInstantPickupProductResponse =
        new BulkInstantPickupProductResponse(responses);
    List<List<String>> row =
        this.bulkInstantPickupProductProcessHelper.getRowData(bulkInstantPickupProductResponse);
    assertNotNull(row);
  }

  @Test
  public void testGetDirectory() {
    BulkDownloadRequest bulkDownloadRequest = new BulkDownloadRequest();
    bulkDownloadRequest.setRequestId("test");
    String row = this.bulkInstantPickupProductProcessHelper.getDirectory(bulkDownloadRequest);
    assertNotNull(row);
  }

  @Test
  public void testGetFilePath() {
    String file = this.bulkInstantPickupProductProcessHelper.getFilePath("FOO", "BAR");
    assertNotNull(file);
  }

  @Test
  public void testGetEmailParams() {
    BulkDownloadRequest request = new BulkDownloadRequest.BulkRequestBuilder()
        .merchant("merchantId").request("requestId").username("username").build();
    Map<String, Object> file =
        this.bulkInstantPickupProductProcessHelper.getEmailParams(request, "in");
    assertNotNull(file);
  }

  @Test
  public void testGetRecordsUpdated() {
    List<OfflineItemInstantPickupBulkDownloadResponse> responses = new ArrayList<>();
    responses.add(new OfflineItemInstantPickupBulkDownloadResponse());
    BulkInstantPickupProductResponse bulkInstantPickupProductResponse =
        new BulkInstantPickupProductResponse(responses);
    int countOfUpdatedRecords =
        bulkInstantPickupProductProcessHelper.getRecordsUpdated(bulkInstantPickupProductResponse);
    Assertions.assertEquals(1, countOfUpdatedRecords);
  }

  @Test
  public void testGenerateFile() throws Exception {
    this.bulkInstantPickupProductProcessHelper.generateFile(FILE_PATH, CONTENT);
  }
}
