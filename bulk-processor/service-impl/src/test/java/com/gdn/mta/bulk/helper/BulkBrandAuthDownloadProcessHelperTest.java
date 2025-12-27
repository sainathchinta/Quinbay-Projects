package com.gdn.mta.bulk.helper;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import org.apache.poi.ss.usermodel.Workbook;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;

import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BrandAuthDataResponse;
import com.gdn.mta.bulk.models.download.responsedata.BrandAuthResponse;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.google.common.collect.ImmutableList;

public class BulkBrandAuthDownloadProcessHelperTest {
  private static final String SELLER_ID = "Seller Code";
  private static final String SELLER_NAME = "Seller Name";
  private static final String BRAND_ID = "Brand Code";
  private static final String BRAND_NAME = "Brand Name";
  private static final String AUTHORIZATION_START_DATE = "Authorisation Start date ";
  private static final String AUTHORIZATION_END_DATE = "Authorisation End Date";
  private static final String SELLER_CODE = "sellerCode";
  private static final String BRAND_CODE = "brandCode";
  private static final String AUTH_START_DATE = "18/07/2023";
  private static final String AUTH_END_DATE = "18/07/2023";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";

  @InjectMocks
  private BulkBrandAuthDownloadProcessHelper bulkBrandAuthDownloadProcessHelper;

  private Workbook workbook;
  private BrandAuthDataResponse bulkDataResponse;
  private BulkDownloadRequest bulkDownloadRequest;
  private static final List<String> HEADER_LIST =
      ImmutableList.<String>builder().add(BRAND_ID).add(BRAND_NAME).add(SELLER_ID).add(SELLER_NAME)
          .add(AUTHORIZATION_START_DATE).add(AUTHORIZATION_END_DATE).build();

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);

    BrandAuthResponse brandAuthResponse =
        new BrandAuthResponse(BRAND_CODE, BRAND_NAME, SELLER_CODE, SELLER_NAME, AUTH_START_DATE, AUTH_END_DATE);
    bulkDataResponse = new BrandAuthDataResponse(Arrays.asList(brandAuthResponse));
    bulkDownloadRequest = new BulkDownloadRequest();
    bulkDownloadRequest.setBulkProcessEntity(BulkProcessEntity.BRAND_AUTHORIZATION_DOWNLOAD);
    bulkDownloadRequest.setRequestId(REQUEST_ID);
    bulkDownloadRequest.setUsername(USERNAME);
  }

  @Test
  public void getCsvHeadersMapTest() {
    Assertions.assertNull(bulkBrandAuthDownloadProcessHelper.getCsvHeadersMap(bulkDownloadRequest));
  }

  @Test
  public void getHeaderListTest() throws Exception {
    Assertions.assertEquals(HEADER_LIST, bulkBrandAuthDownloadProcessHelper.getHeaderList(bulkDataResponse));
  }

  @Test
  public void modifyWorkbookTest() throws Exception {
    Assertions.assertEquals(workbook, bulkBrandAuthDownloadProcessHelper.modifyWorkbook(workbook, bulkDataResponse));
  }

  @Test
  public void getRowDataTest() {
    List<List<String>> rowData = bulkBrandAuthDownloadProcessHelper.getRowData(bulkDataResponse);
    Assertions.assertEquals(SELLER_CODE, rowData.get(0).get(2));
    Assertions.assertEquals(SELLER_NAME, rowData.get(0).get(3));
    Assertions.assertEquals(BRAND_CODE, rowData.get(0).get(0));
    Assertions.assertEquals(BRAND_NAME, rowData.get(0).get(1));
    Assertions.assertEquals(AUTH_START_DATE, rowData.get(0).get(4));
    Assertions.assertEquals(AUTH_END_DATE, rowData.get(0).get(5));
  }

  @Test
  public void getDirectoryTest() {
    Assertions.assertEquals(ProcessorUtils.BULK_BRAND_AUTHORIZATION_TEMPLATE_DIR + REQUEST_ID,
        bulkBrandAuthDownloadProcessHelper.getDirectory(bulkDownloadRequest));
  }

  @Test
  public void getEmailParamsTest() {
    Map<String, Object> email = bulkBrandAuthDownloadProcessHelper.getEmailParams(bulkDownloadRequest, null);
    Assertions.assertEquals(USERNAME, email.get(EmailConstants.NAME));
    Assertions.assertEquals(REQUEST_ID, email.get(EmailConstants.REQ_ID));
    Assertions.assertEquals(EmailConstants.BRAND_AUTH_DOWNLOAD_TEMPLATE_ID, email.get(EmailConstants.TEMPLATE_ID_PARAM));
    Assertions.assertEquals(EmailConstants.MAIL_SENDER, email.get(EmailConstants.MAIL_SENDER_PARAM));
    Assertions.assertEquals(EmailConstants.BRAND_AUTH_DOWNLOAD_TEMPLATE_SUBJECT,
        email.get(EmailConstants.MAIL_SUBJECT_PARAM));
    Assertions.assertEquals(ProcessorUtils.FILETYPE_XLSX_EXCEL, email.get(EmailConstants.FILE_PREFIX));
  }

  @Test
  public void getRecordsUpdatedTest() {
    Assertions.assertEquals(1, bulkBrandAuthDownloadProcessHelper.getRecordsUpdated(bulkDataResponse));
  }
}
