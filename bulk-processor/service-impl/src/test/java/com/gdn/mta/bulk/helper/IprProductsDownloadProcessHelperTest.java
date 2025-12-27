package com.gdn.mta.bulk.helper;

import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkCsvModel;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.BulkIprProductDownloadResponse;
import com.gdn.mta.bulk.models.download.responsedata.IprProductsResponse;
import com.gdn.mta.bulk.service.FileStorageOperationsService;
import com.gdn.mta.bulk.util.ProcessorUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.apache.poi.ss.usermodel.Workbook;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

public class IprProductsDownloadProcessHelperTest {

  @InjectMocks
  private IprProductsDownloadProcessHelper iprProductsDownloadProcessHelper;

  @Mock
  private FileStorageOperationsService fileStorageOperationsService;

  @Mock
  private Workbook workbook;

  private BulkIprProductDownloadResponse bulkIprProductDownloadResponse =
      BulkIprProductDownloadResponse.builder()
          .iprProductsResponses(Arrays.asList(IprProductsResponse.builder().build())).build();

  private static final String USER_NAME = "userName";

  private static final String REQUEST_ID = "requestId";


  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(fileStorageOperationsService);
  }

  @Test
  void getHeaderListTest() throws Exception {
    List<String> response =
        iprProductsDownloadProcessHelper.getHeaderList(bulkIprProductDownloadResponse);
    Assertions.assertNotNull(response);
  }

  @Test
  void getRowDataTest() {
    List<List<String>> response = iprProductsDownloadProcessHelper.getRowData(bulkIprProductDownloadResponse);
    Assertions.assertNotNull(response);
  }

  @Test
  void modifyWorkbookTest() throws Exception {
    Workbook response =
        iprProductsDownloadProcessHelper.modifyWorkbook(workbook, bulkIprProductDownloadResponse);
    Assertions.assertNotNull(response);
  }

  @Test
  void testGetCsvHeadersMap() {
    BulkCsvModel result = iprProductsDownloadProcessHelper.getCsvHeadersMap(Mockito.mock(BulkDownloadRequest.class));
    Assertions.assertNull(result);
  }

  @Test
  void testGetRecordsUpdated() {
    int result = iprProductsDownloadProcessHelper.getRecordsUpdated(bulkIprProductDownloadResponse);
    Assertions.assertEquals(1, result);
  }

  @Test
  void testGetEmailParams() {
    BulkDownloadRequest mockRequest = Mockito.mock(BulkDownloadRequest.class);
    Mockito.when(mockRequest.getUsername()).thenReturn(USER_NAME);
    Map<String, Object> result = iprProductsDownloadProcessHelper.getEmailParams(mockRequest, "en");
    Assertions.assertNotNull(result);
    Assertions.assertEquals(USER_NAME, result.get(EmailConstants.NAME));
    Mockito.verify(fileStorageOperationsService)
        .getFilePrefix(BulkProcessType.IPR_PRODUCTS_DOWNLOAD_ALL.getValue());
  }

  @Test
  void testGetDirectory() {
    BulkDownloadRequest mockRequest = Mockito.mock(BulkDownloadRequest.class);
    Mockito.when(mockRequest.getRequestId()).thenReturn(REQUEST_ID);
    String result = iprProductsDownloadProcessHelper.getDirectory(mockRequest);
    Assertions.assertEquals(
        ProcessorUtils.ENTITY_DIR_MAP.get(BulkProcessEntity.IPR_PRODUCTS_DOWNLOAD_ALL)
            + REQUEST_ID, result);
  }
}
