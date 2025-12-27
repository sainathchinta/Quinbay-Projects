package com.gdn.mta.bulk.helper;

import java.util.List;
import java.util.Map;

import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.ItemSkuAndMasterSkuResponse;
import com.gdn.mta.bulk.models.download.responsedata.MasterSkuItemsDownloadResponse;
import com.gdn.mta.bulk.service.FileStorageOperationsService;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.google.common.collect.ImmutableList;
import org.apache.poi.ss.usermodel.Workbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class BulkMasterSkuReviewItemsDownloadProcessHelperTest {

  private static final String ITEM_SKU = "SKU item";
  private static final String SKU_NAME = "Nama SKU item";
  private static final String MASTER_SKU = "Kode master SKU";
  private static final String MASTER_SKU_NAME = "Nama master SKU";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";

  @Mock
  private FileStorageOperationsService fileStorageOperationsService;

  @InjectMocks
  private BulkMasterSkuReviewItemsDownloadProcessHelper bulkMasterSkuReviewItemsDownloadProcessHelper;

  private Workbook workbook;
  private MasterSkuItemsDownloadResponse bulkDataResponse;
  private BulkDownloadRequest bulkDownloadRequest;

  private static final List<String> HEADER_LIST =
      ImmutableList.<String>builder().add(ITEM_SKU).add(SKU_NAME).add(MASTER_SKU).add(MASTER_SKU_NAME).build();

  @BeforeEach
  public void setup() {
    bulkDownloadRequest = new BulkDownloadRequest();
    bulkDownloadRequest = new BulkDownloadRequest();
    bulkDownloadRequest.setBulkProcessEntity(BulkProcessEntity.MASTER_SKU_ALL_ITEMS_DOWNLOAD);
    bulkDownloadRequest.setRequestId(REQUEST_ID);
    bulkDownloadRequest.setUsername(USERNAME);
    ItemSkuAndMasterSkuResponse itemSkuAndMasterSkuResponse =
        new ItemSkuAndMasterSkuResponse(ITEM_SKU, SKU_NAME, MASTER_SKU, MASTER_SKU_NAME);
    bulkDataResponse = new MasterSkuItemsDownloadResponse();
    bulkDataResponse.setItemSkuAndMasterSkuResponseList(List.of(itemSkuAndMasterSkuResponse));
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(fileStorageOperationsService);
  }

  @Test
  public void getCsvHeadersMapTest() {
    Assertions.assertNull(bulkMasterSkuReviewItemsDownloadProcessHelper.getCsvHeadersMap(bulkDownloadRequest));
  }

  @Test
  public void getHeaderListTest() throws Exception {
    Assertions.assertEquals(HEADER_LIST, bulkMasterSkuReviewItemsDownloadProcessHelper.getHeaderList(bulkDataResponse));
  }

  @Test
  public void modifyWorkbookTest() throws Exception {
    Assertions.assertEquals(workbook,
        bulkMasterSkuReviewItemsDownloadProcessHelper.modifyWorkbook(workbook, bulkDataResponse));
  }

  @Test
  public void getRowDataTest() {
    List<List<String>> rowData = bulkMasterSkuReviewItemsDownloadProcessHelper.getRowData(bulkDataResponse);
    Assertions.assertEquals(ITEM_SKU, rowData.get(0).get(0));
    Assertions.assertEquals(SKU_NAME, rowData.get(0).get(1));
    Assertions.assertEquals(MASTER_SKU, rowData.get(0).get(2));
    Assertions.assertEquals(MASTER_SKU_NAME, rowData.get(0).get(3));
  }

  @Test
  public void getDirectoryTest() {
    Assertions.assertEquals(ProcessorUtils.MASTER_SKU_ITEMS_DOWNLOAD_TEMPLATE_DIR + REQUEST_ID,
        bulkMasterSkuReviewItemsDownloadProcessHelper.getDirectory(bulkDownloadRequest));
  }

  @Test
  public void getEmailParamsTest() {
    Mockito.when(
        fileStorageOperationsService.getFilePrefix(BulkProcessType.MASTER_SKU_REVIEW_ITEMS_DOWNLOAD.getValue()))
      .thenReturn("filePrefix");
    Map<String, Object> email =
      bulkMasterSkuReviewItemsDownloadProcessHelper.getEmailParams(bulkDownloadRequest, null);
    Assertions.assertEquals(USERNAME, email.get(EmailConstants.NAME));
    Assertions.assertEquals(REQUEST_ID, email.get(EmailConstants.REQ_ID));
    Assertions.assertEquals(EmailConstants.MASTER_SKU_REVIEW_ITEMS_DOWNLOAD_TEMPLATE_ID,
      email.get(EmailConstants.TEMPLATE_ID_PARAM));
    Assertions.assertEquals(EmailConstants.MAIL_SENDER,
      email.get(EmailConstants.MAIL_SENDER_PARAM));
    Assertions.assertEquals(EmailConstants.MASTER_SKU_REVIEW_ITEMS_DOWNLOAD_SUBJECT,
      email.get(EmailConstants.MAIL_SUBJECT_PARAM));
    Assertions.assertEquals("filePrefix", email.get(EmailConstants.FILE_PREFIX));
  }

  @Test
  public void getRecordsUpdatedTest() {
    Assertions.assertEquals(1, bulkMasterSkuReviewItemsDownloadProcessHelper.getRecordsUpdated(bulkDataResponse));
  }

}
