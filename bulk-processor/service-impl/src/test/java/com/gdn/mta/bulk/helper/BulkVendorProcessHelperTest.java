package com.gdn.mta.bulk.helper;

import java.util.ArrayList;
import java.util.List;

import com.gdn.mta.bulk.models.download.responsedata.DistributionProductResponse;
import org.apache.poi.ss.usermodel.Workbook;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.DownloadType;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkProductVendorResponse;

public class BulkVendorProcessHelperTest {

  private static final String PRODUCT_CODE = "product-code";
  private static final String PRODUCT_NAME = "product-name";
  private static final String CATEGORY_NAME = "category-name";
  private static final String BUSINESS_PARTNER_NAME = "business-partner-name";
  private static final String INITIATOR = "initiator";
  private static final String REQUEST_ID = "request-id";

  @Mock
  private Workbook workbook;

  @InjectMocks
  private BulkVendorProcessHelper bulkVendorProcessHelper;

  private BulkProductVendorResponse bulkProductVendorResponse;

  private BulkProductVendorResponse generateBulkResponse() throws Exception {
    BulkProductVendorResponse response = new BulkProductVendorResponse();
    List<DistributionProductResponse> list = new ArrayList<>();
    DistributionProductResponse distributionProduct = new DistributionProductResponse();
    distributionProduct.setProductCode(PRODUCT_CODE);
    distributionProduct.setProductName(PRODUCT_NAME);
    distributionProduct.setCategoryName(CATEGORY_NAME);
    distributionProduct.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    distributionProduct.setCreatedBy(INITIATOR);
    list.add(distributionProduct);
    response.setResponseList(list);
    response.setBulkProcessEntity(BulkProcessEntity.PRODUCT_VENDOR);
    return response;
  }

  private BulkDownloadRequest generateBulkDownloadRequest() throws Exception {
    BulkDownloadRequest.BulkRequestBuilder builder = new BulkDownloadRequest.BulkRequestBuilder();
    builder.bulkProcessType(BulkProcessEntity.PRODUCT_VENDOR);
    builder.directDownload(true);
    builder.downloadType(DownloadType.ALL);
    builder.filename("test.xlsx");
    builder.request(REQUEST_ID);
    return builder.build();
  }

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    bulkProductVendorResponse = new BulkProductVendorResponse();
  }

  @Test
  public void modifyWorkbookTest()throws Exception {
    bulkVendorProcessHelper.modifyWorkbook(workbook, bulkProductVendorResponse);
  }

  @Test
  public void getHeaderListTest() throws Exception {
    bulkVendorProcessHelper.getHeaderList(bulkProductVendorResponse);
  }

  @Test
  public void getCsvHeadersMapTest() throws Exception {
    bulkVendorProcessHelper.getCsvHeadersMap(new BulkDownloadRequest());
  }

  @Test
  public void getRowDataTest() throws Exception {
    bulkVendorProcessHelper.getRowData(generateBulkResponse());
  }

  @Test
  public void getRowDataWhenProductApprovedTrueTest() throws Exception {
    BulkProductVendorResponse bulkProductVendorResponse = generateBulkResponse();
    bulkProductVendorResponse.getResponseList().get(0).setProductApproved(true);
    bulkVendorProcessHelper.getRowData(bulkProductVendorResponse);
  }

  @Test
  public void getDirectoryTest() throws Exception {
    bulkVendorProcessHelper.getDirectory(generateBulkDownloadRequest());
  }

  @Test
  public void getEmailParamsTest() throws Exception {
    bulkVendorProcessHelper.getEmailParams(generateBulkDownloadRequest(), "lang");
  }

  @Test
  public void getRecordsUpdatedTest() throws Exception {
    bulkVendorProcessHelper.getRecordsUpdated(generateBulkResponse());
  }
}
