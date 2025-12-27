package com.gdn.mta.bulk.helper;

import static com.gdn.mta.bulk.util.BulkParameters.CATEGORY_C1;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;
import static org.mockito.MockitoAnnotations.initMocks;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.gdn.mta.bulk.models.download.responsedata.BulkProductVendorResponse;
import com.gdn.mta.bulk.models.download.responsedata.DistributionProductResponse;
import org.apache.poi.ss.usermodel.Workbook;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkVendorSummaryResponse;
import org.springframework.test.util.ReflectionTestUtils;

public class BulkVendorSummaryProcessHelperTest {

  private SimpleDateFormat simpleDateFormat = new SimpleDateFormat("dd/MM/yyyy");


  public static final String PRODUCT_NAME = "product-name";
  public static final String PRODUCT_CODE = "product-code";
  public static final String INITIATOR = "initiator";
  public static final String CONTENT_ASSIGNEE = "content-assignee";
  public static final String IMAGE_ASSIGNEE = "image-Assignee";
  public static final String ASSIGNEE = "assignee";
  public static final String CATEGORY = "category";
  public static final String BUSINESS_PARTNER_NAME = "business-partner-name";

  @InjectMocks
  BulkVendorSummaryProcessHelper bulkVendorSummaryProcessHelper;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
  }

  @Test
  public void getCsvHeadersMapTest() throws Exception {
    BulkDownloadRequest bulkDownloadRequest = new BulkDownloadRequest();
    BulkCsvModel bulkCsvModel = this.bulkVendorSummaryProcessHelper.getCsvHeadersMap(bulkDownloadRequest);
    Assertions.assertNull(bulkCsvModel);
  }

  @Test
  public void generateCsvFileTest() throws  Exception {
    BulkCsvModel bulkCsvModels = new BulkCsvModel(new ArrayList<>(), new HashMap<>());
    BulkDataResponse bulkDataResponse = new BulkDataResponse();
    byte[] bulkCsvModel = this.bulkVendorSummaryProcessHelper.generateCsvFile(bulkCsvModels, bulkDataResponse);
    assertNotNull(bulkCsvModel);
  }

  @Test
  public void getHeaderListTest() throws  Exception {
    BulkDataResponse bulkDataResponse = new BulkDataResponse();
    List<String> response = this.bulkVendorSummaryProcessHelper.getHeaderList(bulkDataResponse);
    assertNotNull(response);
  }

  @Test
  public void getHeaderListTest_addC1DetailsTrue() throws Exception {
    BulkProductVendorResponse bulkDataResponse = new BulkProductVendorResponse();
    ReflectionTestUtils.setField(bulkVendorSummaryProcessHelper, "addC1DetailsToVendorBulkDownload",
        true);
    List<String> response = this.bulkVendorSummaryProcessHelper.getHeaderList(bulkDataResponse);
    assertNotNull(response);
    assertEquals(CATEGORY_C1, response.get(2));
  }

  @Test
  @Disabled
  public void generateDataSheetTest() throws  Exception {
    List<String> headers = new ArrayList<>();
    List<List<String>> rows = new ArrayList<>();
    Workbook response = this.bulkVendorSummaryProcessHelper.generateDataSheet(headers, rows, 1);
    assertNotNull(response);
  }

  @Test
  public void getUserNameTest() throws  Exception {
    String response = this.bulkVendorSummaryProcessHelper.getUserName("target");
    assertNotNull(response);
  }

  @Test
  public void modifyWorkbookTest() throws Exception {
    BulkDataResponse bulkDataResponse = new BulkDataResponse();
    Workbook workbook = mock(Workbook.class);
    Workbook response = this.bulkVendorSummaryProcessHelper.modifyWorkbook(workbook, bulkDataResponse);
  }

  private BulkVendorSummaryResponse generateBulkResponse() throws Exception {
    BulkVendorSummaryResponse response = new BulkVendorSummaryResponse();
    List<DistributionProductResponse> list = new ArrayList<>();
    DistributionProductResponse distributionProduct = new DistributionProductResponse();
    distributionProduct.setProductCode(PRODUCT_CODE);
    distributionProduct.setProductName(PRODUCT_NAME);
    distributionProduct.setCategoryName(CATEGORY);
    distributionProduct.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    distributionProduct.setCreatedBy(INITIATOR);
    distributionProduct.setProductApproved(Boolean.TRUE);
    distributionProduct.setProductApproverAssignee(ASSIGNEE);
    list.add(distributionProduct);
    response.setResponseList(list);
    response.setBulkProcessEntity(BulkProcessEntity.PRODUCT_VENDOR);
    return response;
  }

  private String convertDate(Date date) {
    try {
      return simpleDateFormat.format(date);
    } catch(Exception e) {
      return null;
    }
  }

  @Test
  public void getRowDataTest() throws Exception {
    bulkVendorSummaryProcessHelper.getRowData(generateBulkResponse());
  }

  @Test
  public void getRowDataTest_addC1Details() throws Exception {
    ReflectionTestUtils.setField(bulkVendorSummaryProcessHelper, "addC1DetailsToVendorBulkDownload",
        true);
    bulkVendorSummaryProcessHelper.getRowData(generateBulkResponse());
  }

  @Test
  public void getRowDataWithContentApprovedFalseTest() throws Exception {
    BulkVendorSummaryResponse bulkVendorSummaryResponse = generateBulkResponse();
    bulkVendorSummaryResponse.getResponseList().get(0).setProductApproved(false);
    bulkVendorSummaryProcessHelper.getRowData(bulkVendorSummaryResponse);
  }

  @Test
  public void getRowDataWithImageApprovedTrueTest() throws Exception {
    BulkVendorSummaryResponse bulkVendorSummaryResponse = generateBulkResponse();
    bulkVendorSummaryResponse.getResponseList().get(0).setProductApproved(true);
    bulkVendorSummaryProcessHelper.getRowData(bulkVendorSummaryResponse);
  }

  @Test
  public void getDirectoryTest() throws Exception {
    BulkDownloadRequest bulkDownloadRequest = new BulkDownloadRequest();
    bulkDownloadRequest.setRequestId("test");
    String row = this.bulkVendorSummaryProcessHelper.getDirectory(bulkDownloadRequest);
    assertNotNull(row);
  }

  @Test
  public void getFilePathTest() throws Exception {
    String file = this.bulkVendorSummaryProcessHelper.getFilePath("FOO", "BAR");
    assertNotNull(file);
  }

  @Test
  public void getEmailParamsTest() throws Exception {
    BulkDownloadRequest request = new BulkDownloadRequest.BulkRequestBuilder().merchant("merchantId")
        .request("requestId").username("username").build();
    Map<String, Object> file = this.bulkVendorSummaryProcessHelper.getEmailParams(request, "in");
    assertNotNull(file);
  }

  @Test
  public void getRecordsUpdatedTest() throws Exception {
    DistributionProductResponse distributionProductResponse = new DistributionProductResponse();
    List<DistributionProductResponse> distributionProductResponses = new ArrayList<>();
    BulkVendorSummaryResponse response = new BulkVendorSummaryResponse();
    distributionProductResponses.add(distributionProductResponse);
    DistributionProductResponse distributionProductResponse1 = new DistributionProductResponse();
    distributionProductResponses.add(distributionProductResponse1);
    response.setResponseList(distributionProductResponses);
    int countOfUpdatedRecords = this.bulkVendorSummaryProcessHelper.getRecordsUpdated(response);
    Assertions.assertEquals(2, countOfUpdatedRecords);
  }
}
