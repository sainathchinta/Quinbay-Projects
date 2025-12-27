package com.gdn.mta.bulk.helper;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.service.SystemParameterConfigService;
import com.gdn.partners.bulk.util.Constant;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Name;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.bulk.dto.ShippingTypeEligibility;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkProductBasicInfoResponse;
import com.gdn.mta.bulk.models.download.responsedata.RecatFailedProductResponse;
import com.gdn.mta.bulk.service.FileStorageOperationsService;
import com.gdn.mta.bulk.util.BulkParameters;

class BulkProductBasicInfoProcessHelperTest {

  private static final String REQUEST_ID = "testRequestId";
  private static final String SHEET = "Sheet1";
  private static final String SHEET2 = "Sheet2";
  private static final String BULK_BASIC_INFO_REGULAR_TEMPLATE = "BulkBasicInfoRegularTemplate.xlsx";
  private static final String BULK_BASIC_INFO_INSTORE_TEMPLATE = "BulkBasicInfoInstoreTemplate.xlsx";

  private static final List<List<String>> MOCK_PRODUCT_CONTENT_LIST =
      Arrays.asList(Arrays.asList("item1", "item2"), Arrays.asList("item3", "item4"));

  private static final List<List<String>> ROW_DATA_LIST = Arrays.asList(Arrays.asList("value1", "value2"));
  private SystemParameterConfig systemParameterConfig;

  @Mock
  private FileStorageOperationsService fileStorageService;

  @InjectMocks
  private BulkProductBasicInfoProcessHelper helper;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue("10000");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Constant.STORE_ID,
            SystemParameterConfigNames.BULK_UPLOAD_PRODUCT_BASIC_INFO_MAXIMUM_SIZE))
        .thenReturn(systemParameterConfig);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.fileStorageService);
  }

  @Test
  void testGetHeaderList_whenBusinessPartnerO2O() throws Exception {
    BulkProductBasicInfoResponse response = mock(BulkProductBasicInfoResponse.class);
    when(response.isBusinessPartnerO2O()).thenReturn(true);
    List<String> result = helper.getHeaderList(response);
    assertEquals(BulkParameters.getBasicInfoHeaderList(true, false), result);
  }

  @Test
  void testGetHeaderList_whenNotBusinessPartnerO2O() throws Exception {
    BulkProductBasicInfoResponse response = mock(BulkProductBasicInfoResponse.class);
    when(response.isBusinessPartnerO2O()).thenReturn(false);
    List<String> result = helper.getHeaderList(response);
    assertEquals(BulkParameters.getBasicInfoHeaderList(false, false),result);
  }

  @Test
  void testGetDirectory() {
    BulkDownloadRequest request = mock(BulkDownloadRequest.class);
    when(request.getRequestId()).thenReturn(REQUEST_ID);
    String result = helper.getDirectory(request);
    assertTrue(result.contains(REQUEST_ID));
  }

  @Test
  void testGetRecordsUpdated_whenResponseIsNull() {
    int result = helper.getRecordsUpdated(null);
    assertEquals(0, result);
  }

  @Test
  void testGetRecordsUpdated_whenProductContentListIsNull() {
    BulkProductBasicInfoResponse response = mock(BulkProductBasicInfoResponse.class);
    when(response.getProductContentList()).thenReturn(null);

    int result = helper.getRecordsUpdated(response);

    assertEquals(0, result);
  }

  @Test
  void testGetRecordsUpdated_whenProductContentListIsNullNotInstance() {
    RecatFailedProductResponse response = mock(RecatFailedProductResponse.class);
    int result = helper.getRecordsUpdated(response);
    assertEquals(0, result);
  }

  @Test
  void testGetRecordsUpdated_whenProductContentListIsNotEmpty() {
    BulkProductBasicInfoResponse response = mock(BulkProductBasicInfoResponse.class);
    when(response.getProductContentList()).thenReturn(MOCK_PRODUCT_CONTENT_LIST);
    int result = helper.getRecordsUpdated(response);
    assertEquals(2, result);
  }

  @Test
  void testGenerateDataSheet_withSheet1() throws IOException {
    XSSFWorkbook validWorkbook = new XSSFWorkbook();
    XSSFSheet sheet = validWorkbook.createSheet(SHEET);
    for (int i = 0; i < 5; i++) {
      Row row = sheet.createRow(i);
      for (int j = 0; j < BulkParameters.getBasicInfoHeaderList(false, false).size(); j++) {
        row.createCell(j).setCellValue("TestValue" + i + j);
      }
    }
    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
    validWorkbook.write(outputStream);
    validWorkbook.close();
    byte[] validTemplateBytes = outputStream.toByteArray();
    when(fileStorageService.downloadBaseTemplateForBulkBasicInfoUpdate(BULK_BASIC_INFO_REGULAR_TEMPLATE)).thenReturn(
        validTemplateBytes);
    Workbook workbook =
        helper.generateDataSheet(BulkParameters.getBasicInfoHeaderList(false, false), ROW_DATA_LIST, 10);
    assertNotNull(workbook);
    assertTrue(workbook instanceof XSSFWorkbook);
    assertNotNull(workbook.getSheet(SHEET));
    for (int i = 0; i < 5; i++) {
      Row row = workbook.getSheet(SHEET).getRow(i);
      assertNotNull(row);
      for (int j = 0; j < BulkParameters.getBasicInfoHeaderList(false, false).size(); j++) {
        Cell cell = row.getCell(j);
        assertNotNull(cell);
      }
    }
    verify(fileStorageService).downloadBaseTemplateForBulkBasicInfoUpdate(BULK_BASIC_INFO_REGULAR_TEMPLATE);
  }

  @Test
  void testGenerateDataSheet_withSheet1Instore() throws IOException {
    XSSFWorkbook validWorkbook = new XSSFWorkbook();
    XSSFSheet sheet = validWorkbook.createSheet(SHEET);
    for (int i = 0; i < 5; i++) {
      Row row = sheet.createRow(i);
      for (int j = 0; j < BulkParameters.getBasicInfoHeaderList(true, false).size(); j++) {
        row.createCell(j).setCellValue("TestValue" + i + j);
      }
    }
    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
    validWorkbook.write(outputStream);
    validWorkbook.close();
    byte[] validTemplateBytes = outputStream.toByteArray();
    when(fileStorageService.downloadBaseTemplateForBulkBasicInfoUpdate(BULK_BASIC_INFO_INSTORE_TEMPLATE)).thenReturn(
        validTemplateBytes);
    Workbook workbook = helper.generateDataSheet(BulkParameters.getBasicInfoHeaderList(true, false), ROW_DATA_LIST, 10);
    assertNotNull(workbook);
    assertTrue(workbook instanceof XSSFWorkbook);
    assertNotNull(workbook.getSheet(SHEET));
    for (int i = 0; i < 5; i++) {
      Row row = workbook.getSheet(SHEET).getRow(i);
      assertNotNull(row);
      for (int j = 0; j < BulkParameters.getBasicInfoHeaderList(true, false).size(); j++) {
        Cell cell = row.getCell(j);
        assertNotNull(cell);
      }
    }
    verify(fileStorageService).downloadBaseTemplateForBulkBasicInfoUpdate(BULK_BASIC_INFO_INSTORE_TEMPLATE);
  }

  @Test
  void testGetCsvHeadersMap() {
    BulkDownloadRequest request = mock(BulkDownloadRequest.class);
    BulkCsvModel result = helper.getCsvHeadersMap(request);
    assertNull(result);
  }

  @Test
  void testModifyWorkbook_allEligibleShippingOptions() throws Exception {
    Workbook workbook = new XSSFWorkbook();
    Sheet sheet = workbook.createSheet("Main");
    Row headerRow = sheet.createRow(0);
    headerRow.createCell(0).setCellValue(BulkParameters.SHIPPING_TYPE);

    ShippingTypeEligibility eligibility = mock(ShippingTypeEligibility.class);
    when(eligibility.isEligibleForBigProduct()).thenReturn(true);
    when(eligibility.isEligibleForBopisProduct()).thenReturn(true);

    BulkDataResponse response = mock(BulkDataResponse.class);
    when(response.getShippingTypeEligibility()).thenReturn(eligibility);

    Workbook result = helper.modifyWorkbook(workbook, response);
    assertNotNull(result);

    assertNotNull(result.getSheet("Main"));
    assertEquals(0, result.getSheetIndex("Main"));
    assertNotNull(result.getName("ShippingOptions"));
  }

  @Test
  void testModifyWorkbook_onlyBaseShippingOption() throws Exception {
    Workbook workbook = new XSSFWorkbook();
    Sheet sheet = workbook.createSheet("Main");
    Row headerRow = sheet.createRow(0);
    headerRow.createCell(0).setCellValue(BulkParameters.SHIPPING_TYPE);

    ShippingTypeEligibility eligibility = mock(ShippingTypeEligibility.class);
    when(eligibility.isEligibleForBigProduct()).thenReturn(false);
    when(eligibility.isEligibleForBopisProduct()).thenReturn(false);

    BulkDataResponse response = mock(BulkDataResponse.class);
    when(response.getShippingTypeEligibility()).thenReturn(eligibility);

    Workbook result = helper.modifyWorkbook(workbook, response);
    assertNotNull(result);
    Name namedRange = result.getName("ShippingOptions");
    assertNotNull(namedRange);
    assertTrue(namedRange.getRefersToFormula().contains("$A$1:$A$1"));
  }

  @Test
  void testModifyWorkbook_shippingColumnMissing_throwsException() {
    Workbook workbook = new XSSFWorkbook();
    workbook.createSheet("Main"); // no header row

    ShippingTypeEligibility eligibility = mock(ShippingTypeEligibility.class);
    when(eligibility.isEligibleForBigProduct()).thenReturn(true);
    when(eligibility.isEligibleForBopisProduct()).thenReturn(true);

    BulkDataResponse response = mock(BulkDataResponse.class);
    when(response.getShippingTypeEligibility()).thenReturn(eligibility);

    Exception exception = assertThrows(IllegalArgumentException.class, () -> {
      helper.modifyWorkbook(workbook, response);
    });
    assertEquals("Shipping column not found in the sheet.", exception.getMessage());
  }


  @Test
  void testGetEmailParams() {
    BulkDownloadRequest request = mock(BulkDownloadRequest.class);
    Map<String, Object> result = helper.getEmailParams(request, "en");
    assertEquals(0, result.size());
  }

  @Test
  void testGetRowData() {
    BulkProductBasicInfoResponse response = mock(BulkProductBasicInfoResponse.class);
    List<List<String>> mockData = Arrays.asList(Arrays.asList("item1", "item2"), Arrays.asList("item3", "item4"));
    when(response.getProductContentList()).thenReturn(mockData);
    List<List<String>> result = helper.getRowData(response);
    assertNotNull(result);
    assertEquals(mockData, result);
  }

  @Test
  void testGenerateDataSheet_withHeaders() throws IOException {
    // Mock the file storage service to return a valid template
    XSSFWorkbook validWorkbook = new XSSFWorkbook();
    XSSFSheet sheet = validWorkbook.createSheet(SHEET);
    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
    validWorkbook.write(outputStream);
    validWorkbook.close();
    byte[] validTemplateBytes = outputStream.toByteArray();
    when(fileStorageService.downloadBaseTemplateForBulkBasicInfoUpdate(BULK_BASIC_INFO_REGULAR_TEMPLATE)).thenReturn(
        validTemplateBytes);

    // Prepare test data
    List<String> headerList = Arrays.asList(BulkParameters.BLIBLI_PRODUCT_SKU_HEADER, BulkParameters.LENGTH_HEADER,
        // Part of SHIPPING_HEADERS
        BulkParameters.BRAND_HEADER);
    List<List<String>> rowDataList =
        Arrays.asList(Arrays.asList("SKU123", "123.45", "BrandA"), // Valid double for LENGTH_HEADER
            Arrays.asList("SKU456", "InvalidDouble", "BrandB") // Invalid double for LENGTH_HEADER
        );

    // Create a mutable list for SHIPPING_HEADERS for testing
    List<String> shippingHeaders = new ArrayList<>(BulkParameters.SHIPPING_HEADERS);
    shippingHeaders.add(BulkParameters.LENGTH_HEADER);

    // Call the method
    Workbook workbook = helper.generateDataSheet(headerList, rowDataList, 10);

    // Assertions
    assertNotNull(workbook);
    XSSFSheet dataSheet = (XSSFSheet) workbook.getSheet(SHEET);
    assertNotNull(dataSheet);

    // Check rows and cells
    for (int i = 0; i < rowDataList.size(); i++) {
      Row row = dataSheet.getRow(4 + i);
      assertNotNull(row); // Covers if (Objects.isNull(row))

      List<String> rowData = rowDataList.get(i);
      for (int j = 0; j < rowData.size(); j++) {
        Cell cell = row.getCell(j);
        assertNotNull(cell); // Covers if (Objects.isNull(cell))

        if (shippingHeaders.contains(headerList.get(j))) {
          if (j == 1 && i == 1) { // Invalid double case
            assertEquals(0.0, cell.getNumericCellValue(), 0.01); // Default to 0.0
          } else {
            assertEquals(Double.parseDouble(rowData.get(j)), cell.getNumericCellValue(), 0.01);
          }
        } else {
          assertEquals(rowData.get(j), cell.getStringCellValue());
        }
      }
    }
    verify(fileStorageService).downloadBaseTemplateForBulkBasicInfoUpdate(BULK_BASIC_INFO_REGULAR_TEMPLATE);
  }

  @Test
  void testLoadWorkbook_whenExceptionThrown() {
    String fileName = "InvalidTemplate.xlsx";
    // Mock the fileStorageService to throw an exception
    when(fileStorageService.downloadBaseTemplateForBulkBasicInfoUpdate(fileName)).thenThrow(
        new RuntimeException("File not found"));
    // Assert that the exception is thrown and logged
    Exception exception = assertThrows(RuntimeException.class, () -> {
      helper.loadWorkbook(fileName);
    });
    assertEquals("File not found", exception.getMessage());
    verify(fileStorageService).downloadBaseTemplateForBulkBasicInfoUpdate(fileName);
  }

  @Test
  void testGenerateDataSheet_withSheet1SheetNotPresent() throws IOException {
    XSSFWorkbook validWorkbook = new XSSFWorkbook();
    XSSFSheet sheet = validWorkbook.createSheet(SHEET2);
    for (int i = 0; i < 1; i++) {
      Row row = sheet.createRow(i);
      for (int j = 0; j < BulkParameters.getBasicInfoHeaderList(false, false).size(); j++) {
        row.createCell(j).setCellValue("TestValue" + i + j);
      }
    }
    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
    validWorkbook.write(outputStream);
    validWorkbook.close();
    byte[] validTemplateBytes = outputStream.toByteArray();
    when(fileStorageService.downloadBaseTemplateForBulkBasicInfoUpdate(BULK_BASIC_INFO_REGULAR_TEMPLATE)).thenReturn(
        validTemplateBytes);
    assertThrows(IllegalArgumentException.class, () -> {
      helper.generateDataSheet(BulkParameters.getBasicInfoHeaderList(false, false), ROW_DATA_LIST, 10);
    });
    verify(fileStorageService).downloadBaseTemplateForBulkBasicInfoUpdate(BULK_BASIC_INFO_REGULAR_TEMPLATE);
  }
}
