package com.gdn.mta.bulk.helper;

import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.models.BulkProcessEntity;
import com.gdn.mta.bulk.models.EmailConstants;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.models.download.responsedata.AutoApprovedProductsResponse;
import com.gdn.mta.bulk.models.download.responsedata.BulkAutoApprovedProductsDownloadResponse;
import com.gdn.mta.bulk.service.FileStorageOperationsService;
import org.apache.poi.ss.usermodel.Workbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import static com.gdn.mta.bulk.util.ProcessorUtils.AUTO_APPROVED_PRODUCTS_DOWNLOAD_TEMPLATE_DIR;
import static com.gdn.mta.bulk.util.ProcessorUtils.FILE_PREFIX;

@ExtendWith(MockitoExtension.class)
class AutoApprovedProductsDownloadProcessHelperTest {

  @Mock
  private FileStorageOperationsService fileStorageOperationsService;

  @InjectMocks
  private AutoApprovedProductsDownloadProcessHelper autoApprovedProductsDownloadProcessHelper;

  private BulkDownloadRequest bulkDownloadRequest;
  private BulkAutoApprovedProductsDownloadResponse bulkDataResponse;

  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String PRODUCT_CODE = "productCode";
  private static final String PRODUCT_NAME = "productName";
  private static final String CATEGORY = "category";
  private static final String STORE_NAME = "storeName";
  private static final String ASSIGNED_TO = "assignedTo";
  private static final String KODE_PRODUK = "Kode Produk";
  private static final String NAMA_PRODUK = "Nama Produk";
  private static final String KATEGORI = "Kategori";
  private static final String NAMA_TOKO = "Nama Toko";
  private static final String PRODUK_ASSIGNEE = "Produk Assignee";

  @BeforeEach
  void setUp() {
    bulkDownloadRequest = new BulkDownloadRequest();
    bulkDownloadRequest.setBulkProcessEntity(BulkProcessEntity.AUTO_APPROVED_PRODUCTS_ALL_DOWNLOAD);
    bulkDownloadRequest.setRequestId(REQUEST_ID);
    bulkDownloadRequest.setUsername(USERNAME);

    AutoApprovedProductsResponse response = new AutoApprovedProductsResponse();
    response.setProductCode(PRODUCT_CODE);
    response.setProductName(PRODUCT_NAME);
    response.setCategory(CATEGORY);
    response.setStoreName(STORE_NAME);
    response.setAssignedTo(ASSIGNED_TO);

    bulkDataResponse = new BulkAutoApprovedProductsDownloadResponse();
    bulkDataResponse.setAutoApprovedProductsResponses(Collections.singletonList(response));
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(fileStorageOperationsService);
  }

  @Test
  void getHeaderList() {
    List<String> expectedHeaders =
        Arrays.asList(KODE_PRODUK, NAMA_PRODUK, KATEGORI, NAMA_TOKO, PRODUK_ASSIGNEE);
    try {
      List<String> actualHeaders =
          autoApprovedProductsDownloadProcessHelper.getHeaderList(bulkDataResponse);
      Assertions.assertEquals(expectedHeaders, actualHeaders);
    } catch (Exception e) {
      e.printStackTrace();
    }
  }

  @Test
  void getRowData() {
    List<List<String>> expectedRowData = Collections.singletonList(
        Arrays.asList(PRODUCT_CODE, PRODUCT_NAME, CATEGORY, STORE_NAME, ASSIGNED_TO));
    List<List<String>> actualRowData =
        autoApprovedProductsDownloadProcessHelper.getRowData(bulkDataResponse);
    Assertions.assertEquals(expectedRowData, actualRowData);
  }

  @Test
  void modifyWorkbook() throws Exception {
    Workbook workbook = Mockito.mock(Workbook.class);
    Workbook modifiedWorkbook =
        autoApprovedProductsDownloadProcessHelper.modifyWorkbook(workbook, bulkDataResponse);
    Assertions.assertEquals(workbook, modifiedWorkbook);
  }

  @Test
  void getCsvHeadersMap() {
    Assertions.assertNull(
        autoApprovedProductsDownloadProcessHelper.getCsvHeadersMap(bulkDownloadRequest));
  }

  @Test
  void getRecordsUpdated() {
    int expectedRecords = 1;
    int actualRecords =
        autoApprovedProductsDownloadProcessHelper.getRecordsUpdated(bulkDataResponse);
    Assertions.assertEquals(expectedRecords, actualRecords);
  }

  @Test
  void getEmailParams() {
    Mockito.when(fileStorageOperationsService.getFilePrefix(
        BulkProcessType.AUTO_APPROVED_PRODUCTS_DOWNLOAD.getValue())).thenReturn(FILE_PREFIX);

    Map<String, Object> emailParams =
        autoApprovedProductsDownloadProcessHelper.getEmailParams(bulkDownloadRequest, null);

    Assertions.assertEquals(USERNAME, emailParams.get(EmailConstants.NAME));
    Assertions.assertEquals(REQUEST_ID, emailParams.get(EmailConstants.REQ_ID));
    Assertions.assertEquals(EmailConstants.AUTO_APPROVED_PRODUCTS_DOWNLOAD_TEMPLATE_ID,
        emailParams.get(EmailConstants.TEMPLATE_ID_PARAM));
    Assertions.assertEquals(EmailConstants.MAIL_SENDER,
        emailParams.get(EmailConstants.MAIL_SENDER_PARAM));
    Assertions.assertEquals(EmailConstants.AUTO_APPROVED_PRODUCTS_DOWNLOAD_TEMPLATE_SUBJECT,
        emailParams.get(EmailConstants.MAIL_SUBJECT_PARAM));
    Assertions.assertEquals(FILE_PREFIX, emailParams.get(EmailConstants.FILE_PREFIX));
  }

  @Test
  void getDirectory() {
    String expectedDirectory = AUTO_APPROVED_PRODUCTS_DOWNLOAD_TEMPLATE_DIR + REQUEST_ID;
    String actualDirectory =
        autoApprovedProductsDownloadProcessHelper.getDirectory(bulkDownloadRequest);
    Assertions.assertEquals(expectedDirectory, actualDirectory);
  }
}