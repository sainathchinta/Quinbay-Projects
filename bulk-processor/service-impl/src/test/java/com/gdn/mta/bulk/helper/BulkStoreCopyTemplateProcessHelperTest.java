package com.gdn.mta.bulk.helper;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.gdn.mta.bulk.dto.ShippingTypeEligibility;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.service.FileStorageOperationsService;
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

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.DownloadType;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.PickupPointModel;
import com.gdn.mta.bulk.models.download.responsedata.StoreCopyUploadTemplateResponse;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.partners.bulk.util.Constant;

public class BulkStoreCopyTemplateProcessHelperTest {

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
  private BulkStoreCopyTemplateProcessHelper bulkStoreCopyTemplateProcessHelper;

  private StoreCopyUploadTemplateResponse storeCopyUploadTemplateResponse;

  private StoreCopyUploadTemplateResponse generateBulkResponse() {
    StoreCopyUploadTemplateResponse storeCopyProductResponse = new StoreCopyUploadTemplateResponse();
    List<PickupPointModel> pickupPointModels = new ArrayList<>();
    PickupPointModel pickupPointModel = new PickupPointModel(CATEGORY_CODE, CATEGORY_CODE, true);
    pickupPointModels.add(pickupPointModel);
    storeCopyProductResponse.setPickupPoints(pickupPointModels);
    storeCopyProductResponse.setBulkProcessEntity(BulkProcessEntity.STORE_COPY_UPLOAD_TEMPLATE);
    return storeCopyProductResponse;
  }

  private BulkDownloadRequest generateBulkDownloadRequest() throws Exception {
    BulkDownloadRequest builder = new BulkDownloadRequest();
    builder.setBulkProcessEntity(BulkProcessEntity.STORE_COPY_UPLOAD_TEMPLATE);
    builder.setDirectDownload(false);
    builder.setDownloadType(DownloadType.ALL);
    builder.setFilename(Constant.STORE_COPY_UPLOAD_BASE_TEMPLATE_XLSX);
    builder.setRequestId(REQUEST_ID);
    return builder;
  }

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    storeCopyUploadTemplateResponse = generateBulkResponse();
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
  public void getCsvHeadersMap() {
    bulkStoreCopyTemplateProcessHelper.getCsvHeadersMap(new BulkDownloadRequest());
  }

  @Test
  public void getHeaderList() {
    bulkStoreCopyTemplateProcessHelper.getHeaderList(storeCopyUploadTemplateResponse);
  }

  @Test
  public void modifyWorkbook() throws Exception {
    File file = new File("target/test-classes//ExcelTemplate//store-copy-upload-base-template.xlsx");
    workbook.createSheet("Shipping Type");
    try (FileOutputStream fos = new FileOutputStream(file)) {
      workbook.write(fos);
    }
    ShippingTypeEligibility typeEligibility = new ShippingTypeEligibility();
    BulkDataResponse bulkDataResponse = generateBulkResponse();
    bulkDataResponse.setShippingTypeEligibility(typeEligibility);
    bulkStoreCopyTemplateProcessHelper.modifyWorkbook(workbook, bulkDataResponse);
  }

  @Test
  public void getRowData() {
    List<List<String>> rowData = bulkStoreCopyTemplateProcessHelper.getRowData(storeCopyUploadTemplateResponse);
    Assertions.assertEquals(1, rowData.size());
  }

  @Test
  public void generateDataSheet() throws IOException {
    File file = new File("target/test-classes//ExcelTemplate//store-copy-upload-base-template.xlsx");
    Mockito.when(
      fileStorageOperationsService.downloadFile(Mockito.any(BulkProcess.class), Mockito.anyString()))
      .thenReturn(FileUtils.readFileToByteArray(file));
    Workbook workbook = bulkStoreCopyTemplateProcessHelper.generateDataSheet(headerList, rowData, 1);
    Sheet dataSheet = workbook.getSheet(BulkParameters.PICKUP_POINT_SHEET);
    Assertions.assertTrue(dataSheet.getRow(0).getCell(0).getStringCellValue().equals(PRODUCT_CODE));
    Mockito.verify(bulkDownloadServiceBeanUtil)
        .generateValidationForWorkbook(workbook, 1, 11, true, BulkParameters.PICKUP_POINT_SHEET);
    Mockito.verify(fileStorageOperationsService).downloadFile(Mockito.any(BulkProcess.class),
      Mockito.anyString());
  }

  @Test
  public void getDirectory() throws Exception {
    bulkStoreCopyTemplateProcessHelper.getDirectory(generateBulkDownloadRequest());
  }

  @Test
  public void getEmailParams() throws Exception {
    Map<String, Object> file = this.bulkStoreCopyTemplateProcessHelper.getEmailParams(generateBulkDownloadRequest(), "in");
    Assertions.assertNull(file);
  }

  @Test
  public void getRecordsUpdated() {
    int records = this.bulkStoreCopyTemplateProcessHelper.getRecordsUpdated(storeCopyUploadTemplateResponse);
    Assertions.assertEquals(records, 0);
  }
}