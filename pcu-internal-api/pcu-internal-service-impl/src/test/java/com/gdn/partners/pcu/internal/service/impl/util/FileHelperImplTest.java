package com.gdn.partners.pcu.internal.service.impl.util;

import java.io.File;
import java.io.IOException;

import com.gdn.partners.pcu.internal.properties.KafkaTopicProperties;
import com.gdn.partners.pcu.internal.service.impl.config.KafkaPublisher;
import com.gdn.partners.pcu.internal.service.impl.event.model.BulkReviewUploadModel;
import com.gdn.partners.pcu.internal.service.model.BulkInternalProcessType;
import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.mock.web.MockMultipartFile;

import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.properties.SystemParameterProperties;
import com.gdn.partners.pcu.internal.service.FileStorageService;
import com.gdn.partners.pcu.internal.service.impl.FileHelperImpl;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;


public class FileHelperImplTest {

  public static final String PROCESS_TYPE_SALES_CATEGORY = "SALES_CATEGORY_UPDATE";
  public static final String PROCESS_TYPE_BULK_PRICE_UPDATE = "BULK_PRICE_UPDATE";
  public static final String PROCESS_TYPE_STORE_COPY = "STORE_COPY";
  public static final String PROCESS_TYPE_DELETE_BRAND_AUTHORISATION = "DELETE_BRAND_AUTHORISATION";
  public static final String DEFAULT = "DEFAULT";
  public static final String BIP_REQUEST_CODE = "BIP-0000001";
  public static final String FILE = "originalFileName.xlsx";
  public static final String PATH = "target/test-classes/internalProcess/";
  public static final String EVENT = "EVENT";
  public static final String MOCK_FILE_PATH =
      new StringBuilder().append(PATH).append(BIP_REQUEST_CODE).append(Constants.SLASH).append(FILE).toString();

  private byte[] fileContent;
  private static final String ORIGINAL_FILENAME = "originalFilename.xls";
  private static final String USERNAME = "username";
  private static final String STORE_ID = "store-id";
  private static final String REQUEST_ID = "requestId";
  private MockMultipartFile multipartFile;

  @Mock
  private SystemParameterProperties systemParameterProperties;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @InjectMocks
  private FileHelperImpl fileHelper;

  @Captor
  private ArgumentCaptor<BulkReviewUploadModel> bulkReviewUploadModelArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    fileContent = new byte[] {-1, -40, -20, -10};
  }

  @AfterEach
  public void tearDown() throws IOException {
    Mockito.verifyNoMoreInteractions(systemParameterProperties, fileStorageService, kafkaProducer,
        kafkaTopicProperties);
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, fileContent);
  }

  @Test
  public void uploadFileBasedOnProcessTypeSalesCategoryUpdateTest() throws Exception {
    mockFile(MOCK_FILE_PATH);
    multipartFile = new MockMultipartFile("request", FILE, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(MOCK_FILE_PATH)));
    Mockito.when(fileStorageService.uploadFilePath(Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(MOCK_FILE_PATH);
    fileHelper.uploadFileBasedOnProcessType(multipartFile, PROCESS_TYPE_SALES_CATEGORY, BIP_REQUEST_CODE);
    Mockito.verify(fileStorageService).uploadFilePath(Mockito.any(), Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void uploadFileBasedOnProcessTypeBulkPriceUpdateTest() throws Exception {
    mockFile(MOCK_FILE_PATH);
    multipartFile = new MockMultipartFile("request", FILE, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(MOCK_FILE_PATH)));
    Mockito.when(fileStorageService.uploadFilePath(Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(MOCK_FILE_PATH);
    fileHelper.uploadFileBasedOnProcessType(multipartFile, PROCESS_TYPE_BULK_PRICE_UPDATE, BIP_REQUEST_CODE);
    Mockito.verify(fileStorageService).uploadFilePath(Mockito.any(), Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void uploadFileBasedOnProcessTypeStoreCopyTest() throws Exception {
    mockFile(MOCK_FILE_PATH);
    multipartFile = new MockMultipartFile("request", FILE, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(MOCK_FILE_PATH)));
    Mockito.when(fileStorageService.uploadFilePath(Mockito.any(), Mockito.anyString(), Mockito.anyString()))
        .thenReturn(MOCK_FILE_PATH);
    fileHelper.uploadFileBasedOnProcessType(multipartFile, PROCESS_TYPE_STORE_COPY, BIP_REQUEST_CODE);
    Mockito.verify(fileStorageService).uploadFilePath(Mockito.any(), Mockito.anyString(), Mockito.anyString());
  }

  @Test
  public void uploadFileBasedOnProcessTypeDeleteBrandAuthTest() throws Exception {
    mockFile(MOCK_FILE_PATH);
    multipartFile = new MockMultipartFile("request", FILE, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(MOCK_FILE_PATH)));
    Mockito.when(systemParameterProperties.getDeleteBrandAuthorisation()).thenReturn(PATH);
    fileHelper.uploadFileBasedOnProcessType(multipartFile, PROCESS_TYPE_DELETE_BRAND_AUTHORISATION, BIP_REQUEST_CODE);
    Mockito.verify(systemParameterProperties).getDeleteBrandAuthorisation();
  }

  @Test
  public void uploadFileBasedOnProcessTypeDefaultTest() throws Exception {
    mockFile(MOCK_FILE_PATH);
    multipartFile = new MockMultipartFile("request", FILE, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(MOCK_FILE_PATH)));
    Mockito.when(systemParameterProperties.getStoreCopyUploadFilePath()).thenReturn(PATH);
    fileHelper.uploadFileBasedOnProcessType(multipartFile, DEFAULT, BIP_REQUEST_CODE);
  }

  @Test
  public void uploadBulkFileTest() throws Exception {
    mockFile(MOCK_FILE_PATH);
    MockMultipartFile multipartFile =
        new MockMultipartFile("request", ORIGINAL_FILENAME, "application/vnd.ms-excel",
            FileUtils.readFileToByteArray(new File(MOCK_FILE_PATH)));
    when(fileStorageService.uploadFilePath(any(), anyString(), anyString())).thenReturn(PATH);
    when(kafkaTopicProperties.getBulkReviewUploadEvent()).thenReturn(EVENT);
    fileHelper.uploadBulkFile(multipartFile,
        BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name(), REQUEST_ID, STORE_ID, USERNAME);
    verify(fileStorageService).uploadFilePath(any(), anyString(), anyString());
    verify(kafkaTopicProperties, times(2)).getBulkReviewUploadEvent();
    verify(kafkaProducer).send(eq(EVENT), anyString(),
        bulkReviewUploadModelArgumentCaptor.capture());
    Assertions.assertNotNull(bulkReviewUploadModelArgumentCaptor.getValue());
    Assertions.assertEquals(BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name(),
        bulkReviewUploadModelArgumentCaptor.getValue().getBulkProcessType());
    Assertions.assertEquals(REQUEST_ID,
        bulkReviewUploadModelArgumentCaptor.getValue().getRequestId());
    Assertions.assertEquals(STORE_ID, bulkReviewUploadModelArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(USERNAME,
        bulkReviewUploadModelArgumentCaptor.getValue().getCreatedBy());
  }

}
