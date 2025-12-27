package com.gdn.mta.bulk.helper;

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.DownloadType;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.MasterProductDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkMasterProductResponse;
import com.gdn.mta.bulk.service.FileStorageOperationsService;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.x.productcategorybase.dto.response.MasterProductResponse;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

public class BulkMasterProductHelperTest {

  private static final String PRODUCT_CODE = "product-code";
  private static final String PRODUCT_NAME = "product-name";
  private static final String CATEGORY_CODE = "category-code";
  private static final String REQUEST_ID = "request-id";
  private static final String USERNAME = "username";

  @Mock
  private Workbook workbook;

  @Mock
  private FileStorageOperationsService fileStorageOperationsService;

  private List<String> headerList;
  private List<List<String>> rowData;

  @InjectMocks
  private BulkMasterProductProcessHelper bulkMasterProductProcessHelper;

  private BulkMasterProductResponse bulkMasterProductResponse;

  private BulkMasterProductResponse generateBulkResponse() {
    BulkMasterProductResponse bulkMasterProductResponse = new BulkMasterProductResponse();
    List<MasterProductResponse> masterProductResponses = new ArrayList<>();
    MasterProductResponse masterProductResponse = new MasterProductResponse();
    masterProductResponse.setProductCode(PRODUCT_CODE);
    masterProductResponse.setCategoryCode(CATEGORY_CODE);
    masterProductResponse.setName(PRODUCT_NAME);
    masterProductResponse.setId(REQUEST_ID);
    masterProductResponses.add(masterProductResponse);
    bulkMasterProductResponse.setResponseList(masterProductResponses);
    bulkMasterProductResponse.setBulkProcessEntity(BulkProcessEntity.MASTER_PRODUCT);
    return bulkMasterProductResponse;
  }

  private BulkDownloadRequest generateBulkDownloadRequest() throws Exception {
    BulkDownloadRequest.BulkRequestBuilder builder = new BulkDownloadRequest.BulkRequestBuilder();
    builder.bulkProcessType(BulkProcessEntity.MASTER_PRODUCT);
    builder.directDownload(true);
    builder.downloadType(DownloadType.ALL);
    builder.filename("test.xlsx");
    builder.request(REQUEST_ID);
    return builder.build();
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    bulkMasterProductResponse = new BulkMasterProductResponse();
    workbook = new XSSFWorkbook();
    headerList = new ArrayList<>();
    headerList.add(PRODUCT_CODE);
    headerList.add(PRODUCT_NAME);
    rowData = new ArrayList<>();
    rowData.add(headerList);
  }

  @Test
  public void modifyWorkbookTest()throws Exception {
    bulkMasterProductProcessHelper.modifyWorkbook(workbook, generateBulkResponse());
  }

  @Test
  public void getHeaderListTest() throws Exception {
    bulkMasterProductProcessHelper.getHeaderList(bulkMasterProductResponse);
  }

  @Test
  public void getCsvHeadersMapTest() throws Exception {
    bulkMasterProductProcessHelper.getCsvHeadersMap(new BulkDownloadRequest());
  }

  @Test
  public void getRowDataTest() throws Exception {
    bulkMasterProductProcessHelper.getRowData(generateBulkResponse());
  }

  @Test
  public void getDirectoryTest() throws Exception {
    bulkMasterProductProcessHelper.getDirectory(generateBulkDownloadRequest());
  }

  @Test
  public void getEmailParamsTest() throws Exception {
    MasterProductDownloadRequest.MasterProductDownloadBuilder builder =
       new MasterProductDownloadRequest.MasterProductDownloadBuilder();
    builder.request(REQUEST_ID);
    builder.username(USERNAME);
    MasterProductDownloadRequest request = builder.build();
    Map<String, Object> emailParams = bulkMasterProductProcessHelper.getEmailParams(request, request.getLanguage());
    Assertions.assertEquals(USERNAME, emailParams.get("name"));
    Assertions.assertEquals(request.getRequestId(), emailParams.get("reqId"));
    Assertions.assertTrue(emailParams.containsKey(EmailConstants.TEMPLATE_ID_PARAM));
    Assertions.assertTrue(emailParams.containsKey(EmailConstants.MAIL_SENDER_PARAM));
    Assertions.assertTrue(emailParams.containsKey(EmailConstants.MAIL_SUBJECT_PARAM));
  }

  @Test
  public void getRecordsUpdatedTest() throws Exception {
    bulkMasterProductProcessHelper.getRecordsUpdated(generateBulkResponse());
  }

  @Test
  public void generateDataSheetTest() throws Exception {
    Workbook workbook = bulkMasterProductProcessHelper.generateDataSheet(headerList, rowData, 1);
    Sheet dataSheet = workbook.getSheet(BulkParameters.DATA_SHEET);
    Assertions.assertTrue(dataSheet.getRow(0).getCell(0).getStringCellValue().equals(PRODUCT_CODE));
  }

}
