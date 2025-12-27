package com.gdn.mta.bulk.helper;

import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.response.ReviewProductResponse;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkCampaignProductResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkReviewProductResponse;
import org.apache.poi.ss.usermodel.Workbook;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.Mockito.mock;
import static org.mockito.MockitoAnnotations.initMocks;

/**
 * Created by govind on 06/02/2019 AD.
 */
public class BulkReviewProductProcessHelperTest {

  private static final String PRODUCT_ID = "product-id";
  private static final String PRODUCT_CODE = "product-code";
  private static final String PRODUCT_NAME = "product-name";
  private static final String BRAND = "brand";
  private static final String CATEGORY_NAME = "category-name";
  private static final String BUSINESS_PARTNER_NAME = "businessPartnerName";
  private static final String STATE = "state";
  private static final String ASSIGNED_TO = "assignedTo";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String CATEGORY_CODE = "category_code";

  @InjectMocks
  private BulkReviewProductProcessHelper bulkReviewProductProcessHelper;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
  }

  @Test
  public void getCsvHeadersMapTest() throws Exception {
    BulkDownloadRequest bulkDownloadRequest = new BulkDownloadRequest();
    BulkCsvModel bulkCsvModel = this.bulkReviewProductProcessHelper.getCsvHeadersMap(bulkDownloadRequest);
    Assertions.assertNull(bulkCsvModel);
  }

  @Test
  public void generateCsvFileTest() throws  Exception {
    BulkCsvModel bulkCsvModels = new BulkCsvModel(new ArrayList<>(), new HashMap<>());
    BulkDataResponse bulkDataResponse = new BulkDataResponse();
    byte[] bulkCsvModel = this.bulkReviewProductProcessHelper.generateCsvFile(bulkCsvModels, bulkDataResponse);
    assertNotNull(bulkCsvModel);
  }

  @Test
  public void getHeaderListTest() throws  Exception {
    BulkDataResponse bulkDataResponse = new BulkDataResponse();
    List<String> response = this.bulkReviewProductProcessHelper.getHeaderList(bulkDataResponse);
    assertNotNull(response);
  }

  @Test
  public void generateDataSheetTest() throws Exception {
    List<String> headers = new ArrayList<>();
    headers.add(PRODUCT_CODE);
    headers.add(PRODUCT_CODE);
    headers.add(PRODUCT_CODE);
    headers.add(PRODUCT_CODE);
    List<List<String>> rows = new ArrayList<>();
    Workbook response = this.bulkReviewProductProcessHelper.generateDataSheet(headers, rows, 1);
    Assertions.assertNotNull(response);
  }

  @Test
  public void getUserNameTest() throws  Exception {
    String response = this.bulkReviewProductProcessHelper.getUserName("target");
    assertNotNull(response);
  }

  @Test
  public void modifyWorkbookTest() throws Exception {
    BulkDataResponse bulkDataResponse = new BulkDataResponse();
    Workbook workbook = mock(Workbook.class);
    Workbook response = this.bulkReviewProductProcessHelper.modifyWorkbook(workbook, bulkDataResponse);
  }

  @Test
  public void getRowDataTest() throws Exception {
    ReviewProductResponse reviewProductResponse = ReviewProductResponse.builder().productCode(PRODUCT_CODE)
        .productId(PRODUCT_ID).productName(PRODUCT_NAME).assignedTo(ASSIGNED_TO).businessPartnerCode(BUSINESS_PARTNER_CODE)
        .businessPartnerName(BUSINESS_PARTNER_NAME).categoryCode(CATEGORY_CODE).categoryName(CATEGORY_NAME).brand(BRAND)
        .submittedDate(new Date()).state(STATE).isSourceDb(false).build();
    BulkReviewProductResponse bulkDataResponse = new BulkReviewProductResponse(
        Collections.singletonList(reviewProductResponse));
    List<List<String>> row = this.bulkReviewProductProcessHelper.getRowData(bulkDataResponse);
    assertNotNull(row);
    Assertions.assertEquals(PRODUCT_CODE, row.get(0).get(0));
  }

  @Test
  public void getDirectoryTest() throws Exception {
    List<ProductLevel3SummaryResponse> responses = new ArrayList<>();
    BulkCampaignProductResponse bulkDataResponse = new BulkCampaignProductResponse(responses);
    BulkDownloadRequest bulkDownloadRequest = new BulkDownloadRequest();
    bulkDownloadRequest.setRequestId("test");
    String row = this.bulkReviewProductProcessHelper.getDirectory(bulkDownloadRequest);
    assertNotNull(row);
  }

  @Test
  public void getFilePathTest() throws Exception {
    String file = this.bulkReviewProductProcessHelper.getFilePath("FOO", "BAR");
    assertNotNull(file);
  }

  @Test
  public void getEmailParamsTest() throws Exception {
    BulkDownloadRequest request = new BulkDownloadRequest.BulkRequestBuilder().merchant("merchantId")
        .request("requestId").username("username").build();
    Map<String, Object> file = this.bulkReviewProductProcessHelper.getEmailParams(request, "in");
    assertNotNull(file);
  }

  @Test
  public void getRecordsUpdatedTest() throws Exception {
    ReviewProductResponse reviewProductResponse = new ReviewProductResponse();
    List<ReviewProductResponse> reviewProductResponses = new ArrayList<>();
    reviewProductResponses.add(reviewProductResponse);
    BulkReviewProductResponse bulkReviewProductResponse = new BulkReviewProductResponse();
    bulkReviewProductResponse.setResponseList(reviewProductResponses);
    int countOfUpdatedRecords = this.bulkReviewProductProcessHelper.getRecordsUpdated(bulkReviewProductResponse);
    Assertions.assertEquals(1, countOfUpdatedRecords);
  }
}
