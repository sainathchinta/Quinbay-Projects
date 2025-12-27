package com.gdn.mta.bulk.helper;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.mta.bulk.service.FileStorageOperationsService;
import org.apache.poi.ss.usermodel.Workbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkConfigSummaryResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.util.BulkParameters;
import com.gdn.x.productcategorybase.dto.response.BulkConfigDataResponse;

public class BulkConfigurationMerchantSummaryProcessHelperTest {

  private static final String REQUEST_ID = "requestId";
  private static final String MERCHANT_ID = "merchantId";
  private static final String USERNAME = "username";
  private static final String REVIEW_CONFIG = "reviewConfig";
  private static final String CODE = "code";
  private static final String NAME = "name";
  private static final String EMAIL_ADDRESS = "target@email.com";
  private static final String EMAIL_NAME = "target";

  @InjectMocks
  private BulkConfigurationMerchantSummaryProcessHelper bulkConfigurationMerchantSummaryProcessHelper;

  @Mock
  private FileStorageOperationsService fileStorageOperationsService;

  private BulkConfigSummaryResponse bulkConfigSummaryResponse;
  private BulkConfigDataResponse bulkConfigDataResponse;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);

    bulkConfigDataResponse = new BulkConfigDataResponse();
    bulkConfigDataResponse.setCode(CODE);
    bulkConfigDataResponse.setName(NAME);
    bulkConfigDataResponse.setReviewConfig(REVIEW_CONFIG);

    bulkConfigSummaryResponse = new BulkConfigSummaryResponse();
    bulkConfigSummaryResponse.setBulkConfigDataResponseList(Arrays.asList(bulkConfigDataResponse));
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(fileStorageOperationsService);
  }

  @Test
  public void getCsvHeadersMapTest() throws Exception {
    BulkDownloadRequest bulkDownloadRequest = new BulkDownloadRequest();
    BulkCsvModel bulkCsvModel =
        this.bulkConfigurationMerchantSummaryProcessHelper.getCsvHeadersMap(bulkDownloadRequest);
    Assertions.assertNull(bulkCsvModel);
  }

  @Test
  public void generateCsvFileTest() throws Exception {
    BulkCsvModel bulkCsvModels = new BulkCsvModel(new ArrayList<>(), new HashMap<>());
    BulkDataResponse bulkDataResponse = new BulkDataResponse();
    byte[] bulkCsvModel =
        this.bulkConfigurationMerchantSummaryProcessHelper.generateCsvFile(bulkCsvModels, bulkDataResponse);
    assertNotNull(bulkCsvModel);
  }

  @Test
  public void getHeaderListTest() throws Exception {
    BulkDataResponse bulkDataResponse = new BulkDataResponse();
    List<String> response = this.bulkConfigurationMerchantSummaryProcessHelper.getHeaderList(bulkDataResponse);
    assertNotNull(response);
    Assertions.assertEquals(BulkParameters.CONFIG_SELLER_CODE, response.get(0));
    Assertions.assertEquals(BulkParameters.CONFIG_SELLER_NAME, response.get(1));
  }

  @Test
  @Disabled
  public void generateDataSheetTest() throws Exception {
    List<String> headers = new ArrayList<>();
    List<List<String>> rows = new ArrayList<>();
    Workbook response = this.bulkConfigurationMerchantSummaryProcessHelper.generateDataSheet(headers, rows, 1);
    assertNotNull(response);
  }

  @Test
  public void getUserNameTest() throws Exception {
    String response = this.bulkConfigurationMerchantSummaryProcessHelper.getUserName(EMAIL_ADDRESS);
    assertNotNull(response);
    Assertions.assertEquals(EMAIL_NAME, response);
  }

  @Test
  public void modifyWorkbookTest() throws Exception {
    BulkDataResponse bulkDataResponse = new BulkDataResponse();
    Workbook workbook = mock(Workbook.class);
    Workbook response = this.bulkConfigurationMerchantSummaryProcessHelper.modifyWorkbook(workbook, bulkDataResponse);
    assertNotNull(response);
  }

  @Test
  public void getDirectoryTest() throws Exception {
    BulkDownloadRequest bulkDownloadRequest = new BulkDownloadRequest();
    bulkDownloadRequest.setRequestId("test");
    String row = this.bulkConfigurationMerchantSummaryProcessHelper.getDirectory(bulkDownloadRequest);
    assertNotNull(row);
    Assertions.assertTrue(row.endsWith("configurationUpload/" + "test"));
  }

  @Test
  public void getFilePathTest() throws Exception {
    String file = this.bulkConfigurationMerchantSummaryProcessHelper.getFilePath("FOO", "BAR");
    assertNotNull(file);
    Assertions.assertTrue(file.endsWith("BAR"));
  }

  @Test
  public void getEmailParamsTest() throws Exception {
    BulkDownloadRequest request =
        new BulkDownloadRequest.BulkRequestBuilder().merchant(MERCHANT_ID).request(REQUEST_ID).username(USERNAME)
            .build();
    Map<String, Object> map = this.bulkConfigurationMerchantSummaryProcessHelper.getEmailParams(request, "in");
    assertNotNull(map);
    Assertions.assertEquals(EmailConstants.CONFIGURATION_SUMMARY_SELLER_SUBJECT, map.get(EmailConstants.MAIL_SUBJECT_PARAM));
    Mockito.verify(fileStorageOperationsService).getEmailPrefix();
  }

  @Test
  public void getRowDataTest() {
    List<List<String>> row = this.bulkConfigurationMerchantSummaryProcessHelper.getRowData(bulkConfigSummaryResponse);
    assertNotNull(row);
    Assertions.assertEquals(CODE, row.get(0).get(0));
  }

  @Test
  public void getRecordsUpdatedTest() throws Exception {
    int countOfUpdatedRecords =
        this.bulkConfigurationMerchantSummaryProcessHelper.getRecordsUpdated(bulkConfigSummaryResponse);
    Assertions.assertEquals(1, countOfUpdatedRecords);
  }
}
