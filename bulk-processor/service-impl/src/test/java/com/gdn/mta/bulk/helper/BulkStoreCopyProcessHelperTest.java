package com.gdn.mta.bulk.helper;


import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.DownloadType;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.StoreCopyDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.StoreCopyProductResponse;
import com.gdn.mta.bulk.service.FileStorageOperationsService;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.partners.bulk.util.Constant;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertNotNull;


public class BulkStoreCopyProcessHelperTest {

  private static final String PRODUCT_CODE = "product-code";
  private static final String PRODUCT_NAME = "product-name";
  private static final String CATEGORY_CODE = "category-code";
  private static final String REQUEST_ID = "request-id";
  private static final String USERNAME = "username";
  private static final ClassLoader CLASS_LOADER = ClassLoader.getSystemClassLoader();
  private static final String BASE_DIRECTORY = CLASS_LOADER.getResource(StringUtils.EMPTY).getPath();
  public static final String EXCEL_TEMPLATE = "/ExcelTemplate/";

  @Mock
  private Workbook workbook;

  @Mock
  private BulkDownloadServiceBeanUtil bulkDownloadServiceBeanUtil;

  @Mock
  private FileStorageOperationsService fileStorageOperationsService;

  private List<String> headerList;
  private List<List<String>> rowData;

  @InjectMocks
  private BulkStoreCopyProcessHelper bulkStoreCopyProcessHelper;

  private StoreCopyProductResponse storeCopyProductResponse;

  private StoreCopyProductResponse generateBulkResponse() {
    StoreCopyProductResponse storeCopyProductResponse = new StoreCopyProductResponse();
    List<ProductLevel3SummaryResponse> productLevel3SummaryResponses = new ArrayList<>();
    ProductLevel3SummaryResponse productLevel3SummaryResponse = new ProductLevel3SummaryResponse();
    productLevel3SummaryResponse.setProductCode(PRODUCT_CODE);
    productLevel3SummaryResponse.setCategoryCode(CATEGORY_CODE);
    productLevel3SummaryResponse.setId(REQUEST_ID);
    productLevel3SummaryResponses.add(productLevel3SummaryResponse);
    storeCopyProductResponse.setProductLevel3SummaryResponses(productLevel3SummaryResponses);
    storeCopyProductResponse.setBulkProcessEntity(BulkProcessEntity.STORE_COPY_PRODUCTS);
    return storeCopyProductResponse;
  }

  private BulkDownloadRequest generateBulkDownloadRequest() throws Exception {
    StoreCopyDownloadRequest builder = new StoreCopyDownloadRequest();
    builder.setBulkProcessEntity(BulkProcessEntity.STORE_COPY_PRODUCTS);
    builder.setDirectDownload(false);
    builder.setDownloadType(DownloadType.ALL);
    builder.setFilename(Constant.STORE_COPY_DOWNLOAD_BASE_TEMPLATE_XLSX);
    builder.setRequestId(REQUEST_ID);
    return builder;
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(bulkStoreCopyProcessHelper, "storeCopyDownloadTemplatePath", BASE_DIRECTORY + EXCEL_TEMPLATE);
    storeCopyProductResponse = new StoreCopyProductResponse();
    workbook = new XSSFWorkbook();
    headerList = new ArrayList<>();
    headerList.add(PRODUCT_CODE);
    headerList.add(PRODUCT_NAME);
    rowData = new ArrayList<>();
    rowData.add(headerList);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.fileStorageOperationsService);
  }

  @Test
  public void modifyWorkbookTest()throws Exception {
    bulkStoreCopyProcessHelper.modifyWorkbook(workbook, generateBulkResponse());
  }

  @Test
  public void getHeaderListTest() throws Exception {
    bulkStoreCopyProcessHelper.getHeaderList(storeCopyProductResponse);
  }

  @Test
  public void getCsvHeadersMapTest() throws Exception {
    bulkStoreCopyProcessHelper.getCsvHeadersMap(new BulkDownloadRequest());
  }

  @Test
  public void getRowDataTest() throws Exception {
    Mockito.when(bulkDownloadServiceBeanUtil.getAllProductsDetailForStoreCopy(Mockito.anyList())).thenReturn(rowData);
    bulkStoreCopyProcessHelper.getRowData(generateBulkResponse());
    Mockito.verify(bulkDownloadServiceBeanUtil).getAllProductsDetailForStoreCopy(Mockito.anyList());
  }

  @Test
  public void getRowDataTestProductType1() throws Exception {
    Mockito.when(bulkDownloadServiceBeanUtil.getAllProductsDetailForStoreCopy(Mockito.anyList())).thenReturn(rowData);
    StoreCopyProductResponse storeCopyProductResponse = generateBulkResponse();
    storeCopyProductResponse.getProductLevel3SummaryResponses().get(0).setProductType(1);
    bulkStoreCopyProcessHelper.getRowData(storeCopyProductResponse);
    Mockito.verify(bulkDownloadServiceBeanUtil).getAllProductsDetailForStoreCopy(Mockito.anyList());
  }

  @Test
  public void getRowDataTestProductType2() throws Exception {
    Mockito.when(bulkDownloadServiceBeanUtil.getAllProductsDetailForStoreCopy(Mockito.anyList())).thenReturn(rowData);
    StoreCopyProductResponse storeCopyProductResponse = generateBulkResponse();
    storeCopyProductResponse.getProductLevel3SummaryResponses().get(0).setProductType(2);
    bulkStoreCopyProcessHelper.getRowData(storeCopyProductResponse);
    Mockito.verify(bulkDownloadServiceBeanUtil).getAllProductsDetailForStoreCopy(Mockito.anyList());
  }

  @Test
  public void getRowDataTestProductType3() throws Exception {
    Mockito.when(bulkDownloadServiceBeanUtil.getAllProductsDetailForStoreCopy(Mockito.anyList())).thenReturn(rowData);
    StoreCopyProductResponse storeCopyProductResponse = generateBulkResponse();
    storeCopyProductResponse.getProductLevel3SummaryResponses().get(0).setProductType(3);
    bulkStoreCopyProcessHelper.getRowData(storeCopyProductResponse);
    Mockito.verify(bulkDownloadServiceBeanUtil).getAllProductsDetailForStoreCopy(Mockito.anyList());
  }

  @Test
  public void getDirectoryTest() throws Exception {
    bulkStoreCopyProcessHelper.getDirectory(generateBulkDownloadRequest());
  }

  @Test
  public void getEmailParamsTest() throws Exception {
    BulkDownloadRequest request = new BulkDownloadRequest.BulkRequestBuilder().merchant("merchantId")
        .request("requestId").username("username").build();
    Map<String, Object> file = this.bulkStoreCopyProcessHelper.getEmailParams(request, "in");
    assertNotNull(file);
    Mockito
      .verify(fileStorageOperationsService).getFilePrefix(BulkProcessType.STORE_COPY.getValue());
  }

  @Test
  public void getRecordsUpdatedTest() throws Exception {
    bulkStoreCopyProcessHelper.getRecordsUpdated(generateBulkResponse());
  }

  @Test
  public void generateDataSheetTest() throws Exception {
    File file =
      new File("target/test-classes//ExcelTemplate//store-copy-download-base-template" + ".xlsx");
    Mockito
      .when(fileStorageOperationsService.downloadFile(Mockito.any(BulkProcess.class), Mockito.anyString()))
      .thenReturn(FileUtils.readFileToByteArray(file));
    Workbook workbook = bulkStoreCopyProcessHelper.generateDataSheet(headerList, rowData, 1);
    Sheet dataSheet = workbook.getSheet(BulkParameters.DATA_SHEET);
    Assertions.assertTrue(dataSheet.getRow(1).getCell(0).getStringCellValue().equals(PRODUCT_CODE));
    Mockito
      .verify(fileStorageOperationsService).downloadFile(Mockito.any(BulkProcess.class), Mockito.anyString());
  }
}