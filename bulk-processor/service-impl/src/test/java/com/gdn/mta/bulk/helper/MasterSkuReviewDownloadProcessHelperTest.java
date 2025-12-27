package com.gdn.mta.bulk.helper;

import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkMasterSkuInReviewDownloadResponse;
import com.gdn.mta.bulk.models.download.responsedata.InReviewAnchorDownloadResponse;
import com.gdn.mta.bulk.service.FileStorageOperationsService;
import com.gdn.mta.bulk.util.ProcessorUtils;
import org.apache.poi.ss.usermodel.Workbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import java.util.List;
import java.util.Map;

class MasterSkuReviewDownloadProcessHelperTest {
  private static final String FIRST_ANCHOR_SKU = "firstAnchorSku";
  private static final String SECOND_ANCHOR_SKU = "secondAnchorSku";
  private static final String FIRST_ANCHOR_NAME = "firstAnchorName";
  private static final String SECOND_ANCHOR_NAME = "secondAnchorName";
  private static final String REQUEST_ID = "requestId";
  private static final String USER_NAME = "userName";

  @Mock
  private FileStorageOperationsService fileStorageOperationsService;

  @Mock
  private BulkMasterSkuInReviewDownloadResponse mockResponse;

  @InjectMocks
  private MasterSkuReviewDownloadProcessHelper helper;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(fileStorageOperationsService);
  }

  @Test
  void testGetHeaderList() throws Exception {
    List<String> result = helper.getHeaderList(mockResponse);
    Assertions.assertNotNull(result);
  }

  @Test
  void testGetRowData() {
    Mockito.when(mockResponse.getAnchorDownloadResponseList()).thenReturn(List.of(
      new InReviewAnchorDownloadResponse(FIRST_ANCHOR_SKU, SECOND_ANCHOR_SKU, FIRST_ANCHOR_NAME,
        SECOND_ANCHOR_NAME)));
    List<List<String>> result = helper.getRowData(mockResponse);
    Assertions.assertNotNull(result);
    Assertions.assertEquals(1, result.size());
    Assertions.assertEquals(
      List.of(FIRST_ANCHOR_SKU, FIRST_ANCHOR_NAME, SECOND_ANCHOR_SKU, SECOND_ANCHOR_NAME),
      result.get(0));
  }

  @Test
  void testModifyWorkbook() throws Exception {
    Workbook mockWorkbook = Mockito.mock(Workbook.class);
    Workbook result = helper.modifyWorkbook(mockWorkbook, mockResponse);
    Assertions.assertNotNull(result);
    Assertions.assertSame(mockWorkbook, result);
  }

  @Test
  void testGetCsvHeadersMap() {
    BulkCsvModel result = helper.getCsvHeadersMap(Mockito.mock(BulkDownloadRequest.class));
    Assertions.assertNull(result);
  }

  @Test
  void testGetRecordsUpdated() {
    Mockito.when(mockResponse.getAnchorDownloadResponseList()).thenReturn(
      List.of(new InReviewAnchorDownloadResponse(), new InReviewAnchorDownloadResponse()));
    int result = helper.getRecordsUpdated(mockResponse);
    Assertions.assertEquals(2, result);
  }

  @Test
  void testGetEmailParams() {
    BulkDownloadRequest mockRequest = Mockito.mock(BulkDownloadRequest.class);
    Mockito.when(mockRequest.getUsername()).thenReturn(USER_NAME);
    Map<String, Object> result = helper.getEmailParams(mockRequest, "en");
    Assertions.assertNotNull(result);
    Assertions.assertEquals(USER_NAME, result.get(EmailConstants.NAME));
    Mockito.verify(fileStorageOperationsService)
      .getFilePrefix(BulkProcessType.MASTER_SKU_IN_REVIEW_DOWNLOAD.getValue());
  }

  @Test
  void testGetDirectory() {
    BulkDownloadRequest mockRequest = Mockito.mock(BulkDownloadRequest.class);
    Mockito.when(mockRequest.getRequestId()).thenReturn(REQUEST_ID);
    String result = helper.getDirectory(mockRequest);
    Assertions.assertEquals(
      ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.MASTER_SKU_IN_REVIEW_DOWNLOAD)
        + REQUEST_ID, result);
  }
}
