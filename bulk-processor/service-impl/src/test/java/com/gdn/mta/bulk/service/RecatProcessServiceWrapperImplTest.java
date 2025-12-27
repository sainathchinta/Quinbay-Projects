package com.gdn.mta.bulk.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.MockitoAnnotations.initMocks;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.io.FileUtils;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import com.gdn.mta.bulk.service.util.BeanUtils;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.mock.web.MockMultipartFile;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkInternalUploadRequestDTO;
import com.gdn.mta.bulk.dto.BulkProcessPath;
import com.gdn.mta.bulk.dto.RecatProcessSummaryRequest;
import com.gdn.mta.bulk.dto.RecatProcessSummaryResponse;
import com.gdn.mta.bulk.dto.RecatProductCountResponse;
import com.gdn.mta.bulk.entity.ProductCodeAndCategoryCodeEvent;
import com.gdn.mta.bulk.entity.ProductCodeAndCategoryCodeListEvent;
import com.gdn.mta.bulk.entity.ProductRecatStatus;
import com.gdn.mta.bulk.entity.RecatProcess;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.service.download.BulkProcessDownloadService;
import com.gdn.mta.bulk.util.BulkDownloadServiceBeanUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.RecatConstants;

public class RecatProcessServiceWrapperImplTest {

  private static final String STORE_ID = "10001";
  private static final String RECAT_REQUEST_CODE = "request-code";
  private static final String FILE_NAME = "recat-sample.xlsx";
  private static final String RECAT_FILE_NUMBERS = "recat-sample-numbers.xlsx";
  private static final String RECAT_FILE_PARTIAL_EMPTY = "recat-sample-partial-empty.xlsx";
  private static final String EMPTY_RECAT_FILE = "empty-recat-sample.xlsx";
  private static final String EMPTY_RECAT_FILE_1 = "empty-recat-sample-1.xlsx";
  private static final String PRODUCT_CODE = "MTA-";
  private static final String PRODUCT_CODE_1 = "MTA-1";
  private static final String PRODUCT_NAME = "Iphone-";
  private static final String CATEGORY_CODE = "LO-";
  private static final String CATEGORY_CODE_1 = "LO-1";
  private static final String CATEGORY_NAME = "Long Dress ";
  private static final String CATEGORY_NAME_1 = "Long Dress 1";
  private static final String NEW_CATEGORY_CODE = "KA-";
  private static final String NEW_CATEGORY_NAME = "Kaos ";
  private static final String RECAT = "recat";
  private static final String ID_1 = "1";
  private static final String ID_2 = "2";
  private static final String ID_3 = "3";
  private static final String ID_4 = "4";
  private static final String ID_5 = "5";
  private static final String ID_6 = "6";
  private static final String REQUEST_ID = "request-id";
  private static final String USERNAME = "username";
  private static final int PAGE = 0;
  private static final int SIZE = 25;
  private static final String SCHEDULED_TIME = "scheduledTime";
  private static final String VALID_SCHEDULED_TIME = "28/08/2021 12:11:00";
  private static final String FILE_NAME_NOT_EXIST = "recat-not-exist.xlsx";
  private static final String USER_NAME = "userName";


  private SystemParameterConfig systemParameterConfig;
  private SystemParameterConfig publishPendingSystemParameterConfig;
  private SystemParameterConfig pendingProductsFetchLimitConfig;
  private SystemParameterConfig saveRecatProcessDbBatchSizeConfig;
  private SystemParameterConfig pendingProductsPublishBatchSize;
  private SystemParameterConfig updateRecatProcessFinalStatus;
  private SystemParameterConfig changedToFailedThresholdConfig;
  private RecatProcess recatProcess;
  private List<ProductRecatStatus> productRecatStatusList;
  private ProductCodeAndCategoryCodeListEvent productCodeAndCategoryCodeListEvent;
  private List<ProductCodeAndCategoryCodeEvent> productCodeAndCategoryCodeEventList;
  private ProductCodeAndCategoryCodeEvent productCodeAndCategoryCodeEvent;
  private ProductRecatStatus productRecatStatus = new ProductRecatStatus();
  private MockMultipartFile multipartFile;
  private byte[] fileContent = new byte[]{-1, -40, -20, -10};
  private RecatProcessSummaryRequest recatProcessSummaryRequest = new RecatProcessSummaryRequest();
  private BulkInternalUploadRequestDTO bulkInternalUploadRequestDTO;


  @InjectMocks
  private RecatProcessServiceWrapperImpl recatProcessServiceWrapper;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private RecatProcessService recatProcessService;

  @Mock
  private ProductRecatStatusService productRecatStatusService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private BulkDownloadServiceBeanUtil bulkDownloadServiceBeanUtil;

  @Mock
  private BulkProcessDownloadService bulkProcessDownloadService;

  @Captor
  private ArgumentCaptor<List<ProductRecatStatus>> productRecatStatusListArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductRecatStatus> productRecatStatusArgumentCaptor;

  @Captor
  private ArgumentCaptor<String> stringArgumentCaptor;

  @Captor
  private ArgumentCaptor<RecatProcess> recatProcessArgumentCaptor;

  @Captor
  private ArgumentCaptor<List<RecatProcess>> recatProcessListArgumentCaptor;

  @Captor
  private ArgumentCaptor<ProductCodeAndCategoryCodeListEvent> productCodeAndCategoryCodeEventArgumentCaptor;

  @Mock
  private FileStorageService fileStorageService;

  public RecatProcessServiceWrapperImplTest() {
  }

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    systemParameterConfig = new SystemParameterConfig();
    systemParameterConfig.setValue(String.valueOf(Boolean.FALSE));

    publishPendingSystemParameterConfig = new SystemParameterConfig();
    publishPendingSystemParameterConfig.setValue(String.valueOf(Boolean.FALSE));

    pendingProductsFetchLimitConfig = new SystemParameterConfig();
    pendingProductsFetchLimitConfig.setValue(String.valueOf(100));

    saveRecatProcessDbBatchSizeConfig = new SystemParameterConfig();
    saveRecatProcessDbBatchSizeConfig.setValue(String.valueOf(10));

    pendingProductsPublishBatchSize = new SystemParameterConfig();
    pendingProductsPublishBatchSize.setValue(String.valueOf(1));

    updateRecatProcessFinalStatus = new SystemParameterConfig();
    updateRecatProcessFinalStatus.setValue(String.valueOf(false));

    changedToFailedThresholdConfig = new SystemParameterConfig();
    changedToFailedThresholdConfig.setValue(String.valueOf(120));

    recatProcess = new RecatProcess();
    recatProcess.setStatus(RecatConstants.NEW);
    recatProcess.setScheduledTime(new Date());
    recatProcess.setRecatRequestCode(RECAT_REQUEST_CODE);
    recatProcess.setFileName(FILE_NAME);
    recatProcess.setStoreId(STORE_ID);

    productRecatStatus = new ProductRecatStatus();
    productRecatStatus.setRecatRequestCode(RECAT_REQUEST_CODE);
    productRecatStatus.setProductCode(PRODUCT_CODE);
    productRecatStatus.setProductName(PRODUCT_NAME);
    productRecatStatus.setNewCategoryCode(NEW_CATEGORY_CODE);
    productRecatStatus.setNewCategoryName(NEW_CATEGORY_NAME);
    productRecatStatus.setCategoryName(CATEGORY_NAME);
    productRecatStatus.setCategoryCode(CATEGORY_CODE);
    productRecatStatus.setId(ID_1);
    productRecatStatus.setUpdatedDate(new Date());

    ProductRecatStatus productRecatStatus1 = new ProductRecatStatus();
    productRecatStatus1.setRecatRequestCode(RECAT_REQUEST_CODE);
    productRecatStatus1.setProductCode(PRODUCT_CODE_1);
    productRecatStatus1.setNewCategoryCode(CATEGORY_CODE_1);
    productRecatStatus1.setCategoryName(CATEGORY_NAME_1);
    productRecatStatus1.setId(ID_2);
    productRecatStatus1.setStatus(RecatConstants.PUBLISHED);
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.MINUTE, -125);
    productRecatStatus1.setUpdatedDate(calendar.getTime());


    productRecatStatusList = new ArrayList<>();
    productRecatStatusList.add(productRecatStatus);
    productRecatStatusList.add(productRecatStatus1);

    productCodeAndCategoryCodeListEvent = new ProductCodeAndCategoryCodeListEvent();
    productCodeAndCategoryCodeEvent = new ProductCodeAndCategoryCodeEvent();
    productCodeAndCategoryCodeEvent.setId(ID_1);
    ProductCodeAndCategoryCodeEvent productCodeAndCategoryCodeEvent1 = new ProductCodeAndCategoryCodeEvent();
    productCodeAndCategoryCodeEvent1.setId(ID_2);
    ProductCodeAndCategoryCodeEvent productCodeAndCategoryCodeEvent2 = new ProductCodeAndCategoryCodeEvent();
    productCodeAndCategoryCodeEvent2.setId(ID_3);
    ProductCodeAndCategoryCodeEvent productCodeAndCategoryCodeEvent3 = new ProductCodeAndCategoryCodeEvent();
    productCodeAndCategoryCodeEvent3.setId(ID_4);
    ProductCodeAndCategoryCodeEvent productCodeAndCategoryCodeEvent4 = new ProductCodeAndCategoryCodeEvent();
    productCodeAndCategoryCodeEvent4.setId(ID_5);
    ProductCodeAndCategoryCodeEvent productCodeAndCategoryCodeEvent5 = new ProductCodeAndCategoryCodeEvent();
    productCodeAndCategoryCodeEvent5.setId(ID_6);
    productCodeAndCategoryCodeEvent.setId(ID_1);
    productCodeAndCategoryCodeEvent.setRecatRequestCode(RECAT_REQUEST_CODE);
    productCodeAndCategoryCodeEvent.setCategoryCode(CATEGORY_CODE);
    productCodeAndCategoryCodeEvent.setProductCode(PRODUCT_CODE);
    productCodeAndCategoryCodeEventList = new ArrayList<>();
    productCodeAndCategoryCodeEventList.add(productCodeAndCategoryCodeEvent);
    productCodeAndCategoryCodeEventList.add(productCodeAndCategoryCodeEvent1);
    productCodeAndCategoryCodeEventList.add(productCodeAndCategoryCodeEvent2);
    productCodeAndCategoryCodeEventList.add(productCodeAndCategoryCodeEvent3);
    productCodeAndCategoryCodeEventList.add(productCodeAndCategoryCodeEvent4);
    productCodeAndCategoryCodeEventList.add(productCodeAndCategoryCodeEvent5);
    productCodeAndCategoryCodeListEvent.setProductCodeAndCategoryCodeEventList(
        Collections.singletonList(productCodeAndCategoryCodeEvent));
  }

  private void getRecatFiles(String fileName) throws IOException {
    String excelData = new String(Base64.encodeBase64(IOUtils.toByteArray(
        Thread.currentThread().getContextClassLoader().getResourceAsStream("Recat" + File.separator + fileName))),
        "UTF-8");
    byte[] excelFile = Base64.decodeBase64(excelData);
    ProcessorUtils.createDirectories(ProcessorUtils.BULK_RECAT_DIR + File.separator + RECAT_REQUEST_CODE);
    ProcessorUtils
        .createFile(ProcessorUtils.BULK_RECAT_DIR + File.separator + RECAT_REQUEST_CODE + File.separator + fileName,
            excelFile);

    SystemParameterConfig systemParameterConfigNew = new SystemParameterConfig();
    systemParameterConfigNew.setValue(String.valueOf(1));
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.RECAT_BATCH_SIZE))
        .thenReturn(systemParameterConfigNew);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(systemParameterConfigService);
    Mockito.verifyNoMoreInteractions(recatProcessService);
    Mockito.verifyNoMoreInteractions(productRecatStatusService);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
    Mockito.verifyNoMoreInteractions(fileStorageService);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void processNewRecatProcessFalseTest() throws ParseException {
    Mockito
        .when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PROCESS_NEW_REQUESTS))
        .thenReturn(systemParameterConfig);
    recatProcessServiceWrapper.processNewRecatProcess(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PROCESS_NEW_REQUESTS);
  }

  @Test
  public void processNewRecatProcessExceptionTest() throws ParseException {
    Mockito.doThrow(ApplicationRuntimeException.class)
        .when(systemParameterConfigService).findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PROCESS_NEW_REQUESTS);
    recatProcessServiceWrapper.processNewRecatProcess(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PROCESS_NEW_REQUESTS);
  }

  @Test
  public void processNewRecatProcessNullTest() throws ParseException {
    systemParameterConfig.setValue(String.valueOf(Boolean.TRUE));
    Mockito
        .when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PROCESS_NEW_REQUESTS))
        .thenReturn(systemParameterConfig);
    Mockito.when(recatProcessService.getAllEligibleNewRecatProcess(STORE_ID, RecatConstants.NEW))
        .thenReturn(null);
    recatProcessServiceWrapper.processNewRecatProcess(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PROCESS_NEW_REQUESTS);
    Mockito.verify(recatProcessService).getAllEligibleNewRecatProcess(STORE_ID, RecatConstants.NEW);
  }

  @Test
  public void processNewRecatProcessTest() throws ParseException, IOException {
    getRecatFiles(FILE_NAME);
    recatProcess.setFileName(FILE_NAME);
    XSSFWorkbook workBook;
    InputStream is = Files.newInputStream(Paths.get(
      ProcessorUtils.BULK_RECAT_DIR + File.separator + RECAT_REQUEST_CODE + File.separator
        + recatProcess.getFileName()));
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    BulkInternalUploadRequestDTO recat =
      BulkInternalUploadRequestDTO.builder().fileName(FILE_NAME).internalProcessRequestCode(RECAT_REQUEST_CODE)
        .bulkInternalProcessType(BulkInternalProcessType.RECATEGORISATION).relativePath("recat").build();
    systemParameterConfig.setValue(String.valueOf(Boolean.TRUE));
    Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class))).thenReturn(
      sheet);
    Mockito
        .when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PROCESS_NEW_REQUESTS))
        .thenReturn(systemParameterConfig);
    Mockito.when(recatProcessService.getAllEligibleNewRecatProcess(STORE_ID, RecatConstants.NEW))
        .thenReturn(Collections.singletonList(recatProcess));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE))
        .thenReturn(saveRecatProcessDbBatchSizeConfig);
    recatProcessServiceWrapper.processNewRecatProcess(STORE_ID);
    Mockito.when(fileStorageService.isFileExists(bulkInternalUploadRequestDTO)).thenReturn(Boolean.TRUE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PROCESS_NEW_REQUESTS);
    Mockito.verify(recatProcessService).getAllEligibleNewRecatProcess(STORE_ID, RecatConstants.NEW);
    Mockito.verify(productRecatStatusService)
        .saveProductRecatStatusList(productRecatStatusListArgumentCaptor.capture());
    Mockito.verify(recatProcessService).saveRecatProcess(recatProcessArgumentCaptor.capture());
    Mockito.verify(fileStorageService).getFileDataWithInternalUploadRequest(recat);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.RECAT_BATCH_SIZE);
    Assertions.assertEquals(4, recatProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(RecatConstants.IN_PROGRESS, recatProcessArgumentCaptor.getValue().getStatus());
    List<ProductRecatStatus> productRecatStatusList = productRecatStatusListArgumentCaptor.getValue();
    Assertions.assertEquals(4, productRecatStatusList.size(), 0);
    Assertions.assertNotNull(recatProcessArgumentCaptor.getValue().getStartTime());
    int counter = 1;
    for (ProductRecatStatus productRecatStatus : productRecatStatusList) {
      validateProductRecatStatus(counter, productRecatStatus, true);
      counter++;
    }
  }

  @Test
  public void processNewRecatProcessNumberTest() throws ParseException, IOException {
    getRecatFiles(RECAT_FILE_NUMBERS);
    recatProcess.setFileName(RECAT_FILE_NUMBERS);
    XSSFWorkbook workBook;
    InputStream is = Files.newInputStream(Paths.get(
      ProcessorUtils.BULK_RECAT_DIR + File.separator + RECAT_REQUEST_CODE + File.separator
        + recatProcess.getFileName()));
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class))).thenReturn(
      sheet);
    BulkInternalUploadRequestDTO recat =
      BulkInternalUploadRequestDTO.builder().fileName(RECAT_FILE_NUMBERS).internalProcessRequestCode(RECAT_REQUEST_CODE)
        .bulkInternalProcessType(BulkInternalProcessType.RECATEGORISATION).relativePath("recat").build();
    systemParameterConfig.setValue(String.valueOf(Boolean.TRUE));
    Mockito
        .when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PROCESS_NEW_REQUESTS))
        .thenReturn(systemParameterConfig);
    Mockito.when(recatProcessService.getAllEligibleNewRecatProcess(STORE_ID, RecatConstants.NEW))
        .thenReturn(Collections.singletonList(recatProcess));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE))
        .thenReturn(saveRecatProcessDbBatchSizeConfig);
    recatProcessServiceWrapper.processNewRecatProcess(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PROCESS_NEW_REQUESTS);
    Mockito.verify(recatProcessService).getAllEligibleNewRecatProcess(STORE_ID, RecatConstants.NEW);
    Mockito.verify(productRecatStatusService)
        .saveProductRecatStatusList(productRecatStatusListArgumentCaptor.capture());
    Mockito.verify(recatProcessService).saveRecatProcess(recatProcessArgumentCaptor.capture());
    Mockito.verify(fileStorageService).getFileDataWithInternalUploadRequest(recat);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.RECAT_BATCH_SIZE);
    Assertions.assertEquals(4, recatProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(RecatConstants.IN_PROGRESS, recatProcessArgumentCaptor.getValue().getStatus());
    List<ProductRecatStatus> productRecatStatusList = productRecatStatusListArgumentCaptor.getValue();
    Assertions.assertEquals(4, productRecatStatusList.size(), 0);
    Assertions.assertNotNull(recatProcessArgumentCaptor.getValue().getStartTime());
    int counter = 1;
    for (ProductRecatStatus productRecatStatus : productRecatStatusList) {
      validateProductRecatStatus(counter, productRecatStatus, false);
      counter++;
    }
  }

  @Test
  public void processNewRecatProcessPartialEmptyTest() throws ParseException, IOException {
    getRecatFiles(RECAT_FILE_PARTIAL_EMPTY);
    recatProcess.setFileName(RECAT_FILE_PARTIAL_EMPTY);
    XSSFWorkbook workBook;
    InputStream is = Files.newInputStream(Paths.get(
      ProcessorUtils.BULK_RECAT_DIR + File.separator + RECAT_REQUEST_CODE + File.separator
        + recatProcess.getFileName()));
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class))).thenReturn(
      sheet);
    BulkInternalUploadRequestDTO recat =
      BulkInternalUploadRequestDTO.builder().fileName(RECAT_FILE_PARTIAL_EMPTY).internalProcessRequestCode(RECAT_REQUEST_CODE)
        .bulkInternalProcessType(BulkInternalProcessType.RECATEGORISATION).relativePath("recat").build();
    systemParameterConfig.setValue(String.valueOf(Boolean.TRUE));
    Mockito
        .when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PROCESS_NEW_REQUESTS))
        .thenReturn(systemParameterConfig);
    Mockito.when(recatProcessService.getAllEligibleNewRecatProcess(STORE_ID, RecatConstants.NEW))
        .thenReturn(Collections.singletonList(recatProcess));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE))
        .thenReturn(saveRecatProcessDbBatchSizeConfig);
    recatProcessServiceWrapper.processNewRecatProcess(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.RECAT_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PROCESS_NEW_REQUESTS);
    Mockito.verify(recatProcessService).getAllEligibleNewRecatProcess(STORE_ID, RecatConstants.NEW);
    Mockito.verify(productRecatStatusService)
        .saveProductRecatStatusList(productRecatStatusListArgumentCaptor.capture());
    Mockito.verify(recatProcessService).saveRecatProcess(recatProcessArgumentCaptor.capture());
    Mockito.verify(fileStorageService).getFileDataWithInternalUploadRequest(recat);
    Assertions.assertEquals(1, recatProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(RecatConstants.IN_PROGRESS, recatProcessArgumentCaptor.getValue().getStatus());
    List<ProductRecatStatus> productRecatStatusList = productRecatStatusListArgumentCaptor.getValue();
    Assertions.assertEquals(1, productRecatStatusList.size(), 0);
    Assertions.assertNotNull(recatProcessArgumentCaptor.getValue().getStartTime());
    int counter = 1;
    Assertions.assertEquals(PRODUCT_CODE + String.valueOf(counter), productRecatStatusList.get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_NAME + String.valueOf(counter), productRecatStatusList.get(0).getProductName());
    Assertions.assertTrue(StringUtils.isBlank(productRecatStatusList.get(0).getCategoryCode()));
    Assertions.assertTrue(StringUtils.isBlank(productRecatStatusList.get(0).getCategoryName()));
    Assertions.assertEquals(NEW_CATEGORY_CODE + String.valueOf(counter), productRecatStatusList.get(0).getNewCategoryCode());
    Assertions.assertEquals(NEW_CATEGORY_NAME + String.valueOf(counter), productRecatStatusList.get(0).getNewCategoryName());
    Assertions.assertEquals(RECAT_REQUEST_CODE, productRecatStatusList.get(0).getRecatRequestCode());
    Assertions.assertEquals(STORE_ID, productRecatStatusList.get(0).getStoreId());
    Assertions.assertEquals(RecatConstants.PENDING, productRecatStatusList.get(0).getStatus());
  }

  @Test
  public void processNewRecatProcessExceptionTest1() throws ParseException, IOException {
    getRecatFiles(FILE_NAME);
    systemParameterConfig.setValue(String.valueOf(Boolean.TRUE));
    XSSFWorkbook workBook;
    InputStream is = Files.newInputStream(Paths.get(
      ProcessorUtils.BULK_RECAT_DIR + File.separator + RECAT_REQUEST_CODE + File.separator
        + recatProcess.getFileName()));
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class))).thenReturn(
      sheet);
    BulkInternalUploadRequestDTO recat =
      BulkInternalUploadRequestDTO.builder().fileName(FILE_NAME).internalProcessRequestCode(RECAT_REQUEST_CODE)
        .bulkInternalProcessType(BulkInternalProcessType.RECATEGORISATION).relativePath("recat").build();
    Mockito
        .when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PROCESS_NEW_REQUESTS))
        .thenReturn(systemParameterConfig);
    Mockito.when(recatProcessService.getAllEligibleNewRecatProcess(STORE_ID, RecatConstants.NEW))
        .thenReturn(Collections.singletonList(recatProcess));
    Mockito.doThrow(ApplicationRuntimeException.class).when(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE);
    recatProcessServiceWrapper.processNewRecatProcess(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PROCESS_NEW_REQUESTS);
    Mockito.verify(fileStorageService).getFileDataWithInternalUploadRequest(recat);
    Mockito.verify(recatProcessService).getAllEligibleNewRecatProcess(STORE_ID, RecatConstants.NEW);
    Mockito.verify(recatProcessService).saveRecatProcess(recatProcessArgumentCaptor.capture());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.RECAT_BATCH_SIZE);
    Assertions.assertNull(recatProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(RecatConstants.CANCELLED, recatProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(Constant.SYSTEM_ERROR, recatProcessArgumentCaptor.getValue().getNotes());
  }

  @Test
  public void processEmptyRecatProcessTest() throws ParseException, IOException {
    getRecatFiles(EMPTY_RECAT_FILE);
    recatProcess.setFileName(EMPTY_RECAT_FILE);
    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(
      ProcessorUtils.BULK_RECAT_DIR + File.separator + RECAT_REQUEST_CODE
        + File.separator + EMPTY_RECAT_FILE);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    bulkInternalUploadRequestDTO =
      BulkInternalUploadRequestDTO.builder().fileName(recatProcess.getFileName())
        .internalProcessRequestCode(recatProcess.getRecatRequestCode()).relativePath("recat")
        .bulkInternalProcessType(BulkInternalProcessType.RECATEGORISATION).build();
    Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
      any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
    systemParameterConfig.setValue(String.valueOf(Boolean.TRUE));
    Mockito
        .when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PROCESS_NEW_REQUESTS))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE))
        .thenReturn(saveRecatProcessDbBatchSizeConfig);
    Mockito.when(recatProcessService.getAllEligibleNewRecatProcess(STORE_ID, RecatConstants.NEW))
        .thenReturn(Collections.singletonList(recatProcess));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE))
        .thenReturn(saveRecatProcessDbBatchSizeConfig);
    recatProcessServiceWrapper.processNewRecatProcess(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PROCESS_NEW_REQUESTS);
    Mockito.verify(recatProcessService).getAllEligibleNewRecatProcess(STORE_ID, RecatConstants.NEW);
    Mockito.verify(recatProcessService).saveRecatProcess(recatProcessArgumentCaptor.capture());
    Mockito.verify(fileStorageService).getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.RECAT_BATCH_SIZE);
    Assertions.assertEquals(0, recatProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(RecatConstants.CANCELLED, recatProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(RecatConstants.FILE_IS_EMPTY, recatProcessArgumentCaptor.getValue().getNotes());
  }

  @Test
  public void processEmptyRecatProcess1Test() throws ParseException, IOException {
    getRecatFiles(EMPTY_RECAT_FILE_1);
    recatProcess.setFileName(EMPTY_RECAT_FILE_1);
    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(
      ProcessorUtils.BULK_RECAT_DIR + File.separator + RECAT_REQUEST_CODE
        + File.separator + EMPTY_RECAT_FILE_1);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    bulkInternalUploadRequestDTO =
      BulkInternalUploadRequestDTO.builder().fileName(recatProcess.getFileName())
        .relativePath("recat").internalProcessRequestCode(recatProcess.getRecatRequestCode())
        .bulkInternalProcessType(BulkInternalProcessType.RECATEGORISATION).build();
    Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
      any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
    systemParameterConfig.setValue(String.valueOf(Boolean.TRUE));
    Mockito
        .when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PROCESS_NEW_REQUESTS))
        .thenReturn(systemParameterConfig);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE))
        .thenReturn(saveRecatProcessDbBatchSizeConfig);
    Mockito.when(recatProcessService.getAllEligibleNewRecatProcess(STORE_ID, RecatConstants.NEW))
        .thenReturn(Collections.singletonList(recatProcess));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE))
        .thenReturn(saveRecatProcessDbBatchSizeConfig);
    recatProcessServiceWrapper.processNewRecatProcess(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PROCESS_NEW_REQUESTS);
    Mockito.verify(recatProcessService).getAllEligibleNewRecatProcess(STORE_ID, RecatConstants.NEW);
    Mockito.verify(recatProcessService).saveRecatProcess(recatProcessArgumentCaptor.capture());
    Mockito.verify(fileStorageService).getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.RECAT_BATCH_SIZE);
    Assertions.assertEquals(0, recatProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(RecatConstants.CANCELLED, recatProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(RecatConstants.FILE_IS_EMPTY, recatProcessArgumentCaptor.getValue().getNotes());
  }

  @Test
  public void processNewRecatProcessBatchedTest() throws ParseException, IOException {
    saveRecatProcessDbBatchSizeConfig.setValue(String.valueOf(2));
    getRecatFiles(FILE_NAME);
    recatProcess.setFileName(FILE_NAME);
    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(
      ProcessorUtils.BULK_RECAT_DIR + File.separator + RECAT_REQUEST_CODE
        + File.separator + FILE_NAME);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    bulkInternalUploadRequestDTO =
      BulkInternalUploadRequestDTO.builder().fileName(recatProcess.getFileName())
        .internalProcessRequestCode(recatProcess.getRecatRequestCode())
        .bulkInternalProcessType(BulkInternalProcessType.RECATEGORISATION).relativePath("recat").build();
    Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(
      any(BulkInternalUploadRequestDTO.class))).thenReturn(sheet);
    systemParameterConfig.setValue(String.valueOf(Boolean.TRUE));
    Mockito
        .when(systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PROCESS_NEW_REQUESTS))
        .thenReturn(systemParameterConfig);
    Mockito.when(recatProcessService.getAllEligibleNewRecatProcess(STORE_ID, RecatConstants.NEW))
        .thenReturn(Collections.singletonList(recatProcess));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE))
        .thenReturn(saveRecatProcessDbBatchSizeConfig);
    recatProcessServiceWrapper.processNewRecatProcess(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PROCESS_NEW_REQUESTS);
    Mockito.verify(recatProcessService).getAllEligibleNewRecatProcess(STORE_ID, RecatConstants.NEW);
    Mockito.verify(productRecatStatusService, Mockito.times(2))
        .saveProductRecatStatusList(productRecatStatusListArgumentCaptor.capture());
    Mockito.verify(recatProcessService).saveRecatProcess(recatProcessArgumentCaptor.capture());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE);
    Mockito.verify(fileStorageService).getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.RECAT_BATCH_SIZE);
    Assertions.assertEquals(4, recatProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(RecatConstants.IN_PROGRESS, recatProcessArgumentCaptor.getValue().getStatus());
    List<List<ProductRecatStatus>> productRecatStatusList = productRecatStatusListArgumentCaptor.getAllValues();
    Assertions.assertEquals(2, productRecatStatusList.get(0).size(), 0);
    Assertions.assertEquals(2, productRecatStatusList.get(1).size(), 0);
    Assertions.assertEquals(2, productRecatStatusList.size());
    int counter = 1;
    for (ProductRecatStatus productRecatStatus : productRecatStatusList.get(0)) {
      validateProductRecatStatus(counter, productRecatStatus, true);
      counter++;
    }
    for (ProductRecatStatus productRecatStatus : productRecatStatusList.get(1)) {
      validateProductRecatStatus(counter, productRecatStatus, true);
      counter++;
    }
  }

  @Test
  public void getProductCountsByRecatRequestCodePartialSuccessStatusTest() {
    recatProcess.setStatus(RecatConstants.FINISHED);
    recatProcess.setTotalCount(20);
    recatProcess.setSuccessCount(10);
    recatProcess.setErrorCount(10);
    recatProcess.setStoreId(STORE_ID);
    Mockito.when(recatProcessService.findRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE))
        .thenReturn(recatProcess);
    RecatProductCountResponse recatProductCountResponse =
        recatProcessServiceWrapper.getProductCountsByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Mockito.verify(recatProcessService).findRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Assertions.assertEquals(20L, recatProductCountResponse.getTotalProductCount());
    Assertions.assertEquals(10L, recatProductCountResponse.getSuccessCount());
    Assertions.assertEquals(10L, recatProductCountResponse.getFailedCount());
    Assertions.assertEquals(0L, recatProductCountResponse.getInProgressCount());
    Assertions.assertEquals(RecatConstants.FINISHED, recatProductCountResponse.getStatus());
  }

  @Test
  public void getProductCountsByRecatRequestCodeFailedStatusTest() {
    recatProcess.setStatus(RecatConstants.FAILED);
    recatProcess.setTotalCount(20);
    recatProcess.setSuccessCount(0);
    recatProcess.setErrorCount(20);
    recatProcess.setStoreId(STORE_ID);
    Mockito.when(recatProcessService.findRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE))
        .thenReturn(recatProcess);
    RecatProductCountResponse recatProductCountResponse =
        recatProcessServiceWrapper.getProductCountsByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Mockito.verify(recatProcessService).findRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Assertions.assertEquals(20L, recatProductCountResponse.getTotalProductCount());
    Assertions.assertEquals(0L, recatProductCountResponse.getSuccessCount());
    Assertions.assertEquals(20L, recatProductCountResponse.getFailedCount());
    Assertions.assertEquals(0L, recatProductCountResponse.getInProgressCount());
    Assertions.assertEquals(RecatConstants.FAILED, recatProductCountResponse.getStatus());
  }

  @Test
  public void getProductCountsByRecatRequestCodeFinishedStatusTest() {
    recatProcess.setStatus(RecatConstants.FINISHED);
    recatProcess.setTotalCount(10);
    recatProcess.setSuccessCount(10);
    recatProcess.setErrorCount(0);
    recatProcess.setStoreId(STORE_ID);
    Mockito.when(recatProcessService.findRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE))
        .thenReturn(recatProcess);
    RecatProductCountResponse recatProductCountResponse =
        recatProcessServiceWrapper.getProductCountsByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Mockito.verify(recatProcessService).findRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Assertions.assertEquals(10L, recatProductCountResponse.getTotalProductCount());
    Assertions.assertEquals(10L, recatProductCountResponse.getSuccessCount());
    Assertions.assertEquals(0L, recatProductCountResponse.getFailedCount());
    Assertions.assertEquals(0L, recatProductCountResponse.getInProgressCount());
    Assertions.assertEquals(RecatConstants.FINISHED, recatProductCountResponse.getStatus());
  }

  @Test
  public void getProductCountsByRecatRequestCodeInProgressStatusTest() {
    recatProcess.setStatus(RecatConstants.IN_PROGRESS);
    recatProcess.setTotalCount(30);
    recatProcess.setStoreId(STORE_ID);
    Map<String, Integer> productCountMap = new HashMap<>();
    productCountMap.put(RecatConstants.PENDING, 5);
    productCountMap.put(RecatConstants.PUBLISHED, 5);
    productCountMap.put(RecatConstants.FINISHED, 10);
    productCountMap.put(RecatConstants.FAILED, 10);
    Mockito.when(recatProcessService.findRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE))
        .thenReturn(recatProcess);
    Mockito.when(productRecatStatusService.getProductCountByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE))
        .thenReturn(productCountMap);
    RecatProductCountResponse recatProductCountResponse =
        recatProcessServiceWrapper.getProductCountsByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Mockito.verify(recatProcessService).findRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Mockito.verify(productRecatStatusService).getProductCountByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Assertions.assertEquals(30L, recatProductCountResponse.getTotalProductCount());
    Assertions.assertEquals(10L, recatProductCountResponse.getSuccessCount());
    Assertions.assertEquals(10L, recatProductCountResponse.getFailedCount());
    Assertions.assertEquals(10L, recatProductCountResponse.getInProgressCount());
    Assertions.assertEquals(RecatConstants.IN_PROGRESS, recatProductCountResponse.getStatus());
  }

  @Test
  public void getProductCountsByRecatRequestCodeNewStatusTest() {
    Mockito.when(recatProcessService.findRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE)).thenReturn(recatProcess);
    RecatProductCountResponse recatProductCountResponse = recatProcessServiceWrapper.getProductCountsByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Mockito.verify(recatProcessService).findRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Assertions.assertEquals(0L, recatProductCountResponse.getTotalProductCount());
    Assertions.assertEquals(0L, recatProductCountResponse.getSuccessCount());
    Assertions.assertEquals(0L, recatProductCountResponse.getFailedCount());
    Assertions.assertEquals(0L, recatProductCountResponse.getInProgressCount());
    Assertions.assertEquals(RecatConstants.NEW, recatProductCountResponse.getStatus());
  }

  private void validateProductRecatStatus(int counter, ProductRecatStatus productRecatStatus, boolean categoryCodeName) {
    Assertions.assertEquals(PRODUCT_CODE + String.valueOf(counter), productRecatStatus.getProductCode());
    Assertions.assertEquals(PRODUCT_NAME + String.valueOf(counter), productRecatStatus.getProductName());
    Assertions.assertEquals(CATEGORY_CODE + String.valueOf(counter), productRecatStatus.getCategoryCode());
    if(categoryCodeName) {
      Assertions.assertEquals(CATEGORY_NAME + String.valueOf(counter), productRecatStatus.getCategoryName());
    } else {
      Assertions.assertEquals(String.valueOf(counter), productRecatStatus.getCategoryName());
    }
    Assertions.assertEquals(NEW_CATEGORY_CODE + String.valueOf(counter), productRecatStatus.getNewCategoryCode());
    Assertions.assertEquals(NEW_CATEGORY_NAME + String.valueOf(counter), productRecatStatus.getNewCategoryName());
    Assertions.assertEquals(RECAT_REQUEST_CODE, productRecatStatus.getRecatRequestCode());
    Assertions.assertEquals(STORE_ID, productRecatStatus.getStoreId());
    Assertions.assertEquals(RecatConstants.PENDING, productRecatStatus.getStatus());
  }

  @Test
  public void publishPendingProductsDisabledTest() {
    Mockito.when(
        systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PUBLISH_PENDING_PRODUCTS))
        .thenReturn(publishPendingSystemParameterConfig);
    recatProcessServiceWrapper.publishPendingProducts(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PUBLISH_PENDING_PRODUCTS);
  }

  @Test
  public void publishPendingProductsExceptionTest() {
    Mockito.doThrow(ApplicationRuntimeException.class).when(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PUBLISH_PENDING_PRODUCTS);
    recatProcessServiceWrapper.publishPendingProducts(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PUBLISH_PENDING_PRODUCTS);
  }

  @Test
  public void publishPendingProductsTest() {
    publishPendingSystemParameterConfig.setValue(String.valueOf(Boolean.TRUE));
    Mockito.when(
        systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PUBLISH_PENDING_PRODUCTS))
        .thenReturn(publishPendingSystemParameterConfig);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PENDING_PRODUCTS_FETCH_LIMIT))
        .thenReturn(pendingProductsFetchLimitConfig);
    Mockito.when(productRecatStatusService.findProductRecatStatusByStoreIdAndAndStatus(STORE_ID, RecatConstants.PENDING,
        Integer.parseInt(pendingProductsFetchLimitConfig.getValue()))).thenReturn(productRecatStatusList);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE))
        .thenReturn(saveRecatProcessDbBatchSizeConfig);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PENDING_PRODUCTS_PUBLISH_BATCH_SIZE))
        .thenReturn(pendingProductsPublishBatchSize);
    recatProcessServiceWrapper.publishPendingProducts(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PUBLISH_PENDING_PRODUCTS);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PENDING_PRODUCTS_FETCH_LIMIT);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PENDING_PRODUCTS_PUBLISH_BATCH_SIZE);
    Mockito.verify(productRecatStatusService)
        .findProductRecatStatusByStoreIdAndAndStatus(STORE_ID, RecatConstants.PENDING,
            Integer.parseInt(pendingProductsFetchLimitConfig.getValue()));
    Mockito.verify(productRecatStatusService)
        .saveProductRecatStatusList(productRecatStatusListArgumentCaptor.capture());
    Mockito.verify(kafkaProducer, Mockito.times(2))
        .send(Mockito.eq(kafkaTopicProperties.getProductRecatProcess()), productCodeAndCategoryCodeEventArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productRecatStatusListArgumentCaptor.getValue().get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_CODE_1, productRecatStatusListArgumentCaptor.getValue().get(1).getProductCode());
    Assertions.assertEquals(NEW_CATEGORY_CODE, productRecatStatusListArgumentCaptor.getValue().get(0).getNewCategoryCode());
    Assertions.assertEquals(CATEGORY_CODE_1, productRecatStatusListArgumentCaptor.getValue().get(1).getNewCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME, productRecatStatusListArgumentCaptor.getValue().get(0).getCategoryName());
    Assertions.assertEquals(CATEGORY_NAME_1, productRecatStatusListArgumentCaptor.getValue().get(1).getCategoryName());
    Assertions.assertEquals(RecatConstants.PUBLISHED, productRecatStatusListArgumentCaptor.getValue().get(1).getStatus());
    Assertions.assertEquals(RecatConstants.PUBLISHED, productRecatStatusListArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertEquals(ID_1,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(0).getProductCodeAndCategoryCodeEventList()
            .get(0).getId());
    Assertions.assertEquals(PRODUCT_CODE,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(0).getProductCodeAndCategoryCodeEventList()
            .get(0).getProductCode());
    Assertions.assertEquals(RECAT_REQUEST_CODE,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(0).getProductCodeAndCategoryCodeEventList()
            .get(0).getRecatRequestCode());
    Assertions.assertEquals(NEW_CATEGORY_CODE,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(0).getProductCodeAndCategoryCodeEventList()
            .get(0).getCategoryCode());
    Assertions.assertEquals(ID_2,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(1).getProductCodeAndCategoryCodeEventList()
            .get(0).getId());
    Assertions.assertEquals(PRODUCT_CODE_1,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(1).getProductCodeAndCategoryCodeEventList()
            .get(0).getProductCode());
    Assertions.assertEquals(RECAT_REQUEST_CODE,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(1).getProductCodeAndCategoryCodeEventList()
            .get(0).getRecatRequestCode());
    Assertions.assertEquals(CATEGORY_CODE_1,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(1).getProductCodeAndCategoryCodeEventList()
            .get(0).getCategoryCode());
    Mockito.verify(kafkaTopicProperties, Mockito.times(3)).getProductRecatProcess();
  }

  @Test
  public void publishPendingProductsBatchedSaveTest() {
    saveRecatProcessDbBatchSizeConfig.setValue(String.valueOf(1));
    publishPendingSystemParameterConfig.setValue(String.valueOf(Boolean.TRUE));
    Mockito.when(
        systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PUBLISH_PENDING_PRODUCTS))
        .thenReturn(publishPendingSystemParameterConfig);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PENDING_PRODUCTS_FETCH_LIMIT))
        .thenReturn(pendingProductsFetchLimitConfig);
    Mockito.when(productRecatStatusService.findProductRecatStatusByStoreIdAndAndStatus(STORE_ID, RecatConstants.PENDING,
        Integer.parseInt(pendingProductsFetchLimitConfig.getValue()))).thenReturn(productRecatStatusList);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE))
        .thenReturn(saveRecatProcessDbBatchSizeConfig);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PENDING_PRODUCTS_PUBLISH_BATCH_SIZE))
        .thenReturn(pendingProductsPublishBatchSize);
    recatProcessServiceWrapper.publishPendingProducts(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PUBLISH_PENDING_PRODUCTS);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PENDING_PRODUCTS_FETCH_LIMIT);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PENDING_PRODUCTS_PUBLISH_BATCH_SIZE);
    Mockito.verify(productRecatStatusService)
        .findProductRecatStatusByStoreIdAndAndStatus(STORE_ID, RecatConstants.PENDING,
            Integer.parseInt(pendingProductsFetchLimitConfig.getValue()));
    Mockito.verify(productRecatStatusService, Mockito.times(2))
        .saveProductRecatStatusList(productRecatStatusListArgumentCaptor.capture());
    Mockito.verify(kafkaProducer, Mockito.times(2))
        .send(Mockito.eq(kafkaTopicProperties.getProductRecatProcess()),productCodeAndCategoryCodeEventArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productRecatStatusListArgumentCaptor.getAllValues().get(0).get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_CODE_1,
        productRecatStatusListArgumentCaptor.getAllValues().get(1).get(0).getProductCode());
    Assertions.assertEquals(NEW_CATEGORY_CODE,
        productRecatStatusListArgumentCaptor.getAllValues().get(0).get(0).getNewCategoryCode());
    Assertions.assertEquals(CATEGORY_CODE_1,
        productRecatStatusListArgumentCaptor.getAllValues().get(1).get(0).getNewCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME,
        productRecatStatusListArgumentCaptor.getAllValues().get(0).get(0).getCategoryName());
    Assertions.assertEquals(CATEGORY_NAME_1,
        productRecatStatusListArgumentCaptor.getAllValues().get(1).get(0).getCategoryName());
    Assertions.assertEquals(RecatConstants.PUBLISHED,
        productRecatStatusListArgumentCaptor.getAllValues().get(0).get(0).getStatus());
    Assertions.assertEquals(RecatConstants.PUBLISHED,
        productRecatStatusListArgumentCaptor.getAllValues().get(1).get(0).getStatus());
    Assertions.assertEquals(ID_1,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(0).getProductCodeAndCategoryCodeEventList()
            .get(0).getId());
    Assertions.assertEquals(PRODUCT_CODE,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(0).getProductCodeAndCategoryCodeEventList()
            .get(0).getProductCode());
    Assertions.assertEquals(RECAT_REQUEST_CODE,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(0).getProductCodeAndCategoryCodeEventList()
            .get(0).getRecatRequestCode());
    Assertions.assertEquals(NEW_CATEGORY_CODE,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(0).getProductCodeAndCategoryCodeEventList()
            .get(0).getCategoryCode());
    Assertions.assertEquals(ID_2,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(1).getProductCodeAndCategoryCodeEventList()
            .get(0).getId());
    Assertions.assertEquals(PRODUCT_CODE_1,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(1).getProductCodeAndCategoryCodeEventList()
            .get(0).getProductCode());
    Assertions.assertEquals(RECAT_REQUEST_CODE,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(1).getProductCodeAndCategoryCodeEventList()
            .get(0).getRecatRequestCode());
    Assertions.assertEquals(CATEGORY_CODE_1,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(1).getProductCodeAndCategoryCodeEventList()
            .get(0).getCategoryCode());
    Mockito.verify(kafkaTopicProperties, Mockito.times(3)).getProductRecatProcess();
  }

  @Test
  public void publishPendingProductsBatchedSaveAndPublishTest() {
    saveRecatProcessDbBatchSizeConfig.setValue(String.valueOf(1));
    pendingProductsPublishBatchSize.setValue(String.valueOf(5));
    publishPendingSystemParameterConfig.setValue(String.valueOf(Boolean.TRUE));
    Mockito.when(
        systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PUBLISH_PENDING_PRODUCTS))
        .thenReturn(publishPendingSystemParameterConfig);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PENDING_PRODUCTS_FETCH_LIMIT))
        .thenReturn(pendingProductsFetchLimitConfig);
    Mockito.when(productRecatStatusService.findProductRecatStatusByStoreIdAndAndStatus(STORE_ID, RecatConstants.PENDING,
        Integer.parseInt(pendingProductsFetchLimitConfig.getValue()))).thenReturn(productRecatStatusList);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE))
        .thenReturn(saveRecatProcessDbBatchSizeConfig);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PENDING_PRODUCTS_PUBLISH_BATCH_SIZE))
        .thenReturn(pendingProductsPublishBatchSize);
    recatProcessServiceWrapper.publishPendingProducts(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PUBLISH_PENDING_PRODUCTS);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PENDING_PRODUCTS_FETCH_LIMIT);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PENDING_PRODUCTS_PUBLISH_BATCH_SIZE);
    Mockito.verify(productRecatStatusService)
        .findProductRecatStatusByStoreIdAndAndStatus(STORE_ID, RecatConstants.PENDING,
            Integer.parseInt(pendingProductsFetchLimitConfig.getValue()));
    Mockito.verify(productRecatStatusService, Mockito.times(2))
        .saveProductRecatStatusList(productRecatStatusListArgumentCaptor.capture());
    Mockito.verify(kafkaProducer)
        .send(Mockito.eq(kafkaTopicProperties.getProductRecatProcess()), productCodeAndCategoryCodeEventArgumentCaptor.capture());
    Assertions.assertEquals(PRODUCT_CODE, productRecatStatusListArgumentCaptor.getAllValues().get(0).get(0).getProductCode());
    Assertions.assertEquals(PRODUCT_CODE_1,
        productRecatStatusListArgumentCaptor.getAllValues().get(1).get(0).getProductCode());
    Assertions.assertEquals(NEW_CATEGORY_CODE,
        productRecatStatusListArgumentCaptor.getAllValues().get(0).get(0).getNewCategoryCode());
    Assertions.assertEquals(CATEGORY_CODE_1,
        productRecatStatusListArgumentCaptor.getAllValues().get(1).get(0).getNewCategoryCode());
    Assertions.assertEquals(CATEGORY_NAME,
        productRecatStatusListArgumentCaptor.getAllValues().get(0).get(0).getCategoryName());
    Assertions.assertEquals(CATEGORY_NAME_1,
        productRecatStatusListArgumentCaptor.getAllValues().get(1).get(0).getCategoryName());
    Assertions.assertEquals(RecatConstants.PUBLISHED,
        productRecatStatusListArgumentCaptor.getAllValues().get(0).get(0).getStatus());
    Assertions.assertEquals(RecatConstants.PUBLISHED,
        productRecatStatusListArgumentCaptor.getAllValues().get(1).get(0).getStatus());
    Assertions.assertEquals(ID_1,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(0).getProductCodeAndCategoryCodeEventList()
            .get(0).getId());
    Assertions.assertEquals(PRODUCT_CODE,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(0).getProductCodeAndCategoryCodeEventList()
            .get(0).getProductCode());
    Assertions.assertEquals(RECAT_REQUEST_CODE,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(0).getProductCodeAndCategoryCodeEventList()
            .get(0).getRecatRequestCode());
    Assertions.assertEquals(NEW_CATEGORY_CODE,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(0).getProductCodeAndCategoryCodeEventList()
            .get(0).getCategoryCode());
    Assertions.assertEquals(ID_2,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(0).getProductCodeAndCategoryCodeEventList()
            .get(1).getId());
    Assertions.assertEquals(PRODUCT_CODE_1,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(0).getProductCodeAndCategoryCodeEventList()
            .get(1).getProductCode());
    Assertions.assertEquals(RECAT_REQUEST_CODE,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(0).getProductCodeAndCategoryCodeEventList()
            .get(1).getRecatRequestCode());
    Assertions.assertEquals(CATEGORY_CODE_1,
        productCodeAndCategoryCodeEventArgumentCaptor.getAllValues().get(0).getProductCodeAndCategoryCodeEventList()
            .get(1).getCategoryCode());
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getProductRecatProcess();
  }

  @Test
  public void updateProductCategoryTest() {
    Mockito.when(productRecatStatusService.findByIdAndStatus(ID_1, RecatConstants.PUBLISHED))
        .thenReturn(productRecatStatusList.get(0));
    Mockito.when(productRecatStatusService.updateProductCategory(productRecatStatusList.get(0)))
        .thenReturn(StringUtils.EMPTY);
    recatProcessServiceWrapper.updateProductCategory(productCodeAndCategoryCodeListEvent);
    Mockito.verify(productRecatStatusService).findByIdAndStatus(ID_1, RecatConstants.PUBLISHED);
    Mockito.verify(productRecatStatusService).updateProductCategory(productRecatStatusList.get(0));
    Mockito.verify(productRecatStatusService).validateResponseAndSave(productRecatStatusList.get(0), StringUtils.EMPTY);
  }

  @Test
  public void updateProductCategoryNullTest() {
    recatProcessServiceWrapper.updateProductCategory(productCodeAndCategoryCodeListEvent);
    Mockito.verify(productRecatStatusService).findByIdAndStatus(ID_1, RecatConstants.PUBLISHED);
  }

  @Test
  public void updateProductCategoryExceptionTest() {
    Mockito.when(productRecatStatusService.findByIdAndStatus(ID_1, RecatConstants.PUBLISHED))
        .thenReturn(productRecatStatusList.get(0));
    Mockito.doThrow(ApplicationRuntimeException.class).when(productRecatStatusService)
        .updateProductCategory(productRecatStatusList.get(0));
    recatProcessServiceWrapper.updateProductCategory(productCodeAndCategoryCodeListEvent);
    Mockito.verify(productRecatStatusService).findByIdAndStatus(ID_1, RecatConstants.PUBLISHED);
    Mockito.verify(productRecatStatusService).updateProductCategory(productRecatStatusList.get(0));
    Mockito.verify(productRecatStatusService)
        .validateResponseAndSave(productRecatStatusList.get(0), Constant.SYSTEM_ERROR);
  }

  @Test
  public void updateProductCategoryValidationErrorTest() {
    ProductRecatStatus productRecatStatus1 = getProductRecatStatus(ID_1);
    productRecatStatus1.setProductName(StringUtils.EMPTY);
    Mockito.when(productRecatStatusService.findByIdAndStatus(ID_1, RecatConstants.PUBLISHED))
        .thenReturn(productRecatStatus1);
    productRecatStatus1 = getProductRecatStatus(ID_2);
    productRecatStatus1.setProductCode(StringUtils.EMPTY);
    Mockito.when(productRecatStatusService.findByIdAndStatus(ID_2, RecatConstants.PUBLISHED))
        .thenReturn(productRecatStatus1);
    productRecatStatus1 = getProductRecatStatus(ID_3);
    productRecatStatus1.setCategoryCode(StringUtils.EMPTY);
    Mockito.when(productRecatStatusService.findByIdAndStatus(ID_3, RecatConstants.PUBLISHED))
        .thenReturn(productRecatStatus1);
    productRecatStatus1 = getProductRecatStatus(ID_4);
    productRecatStatus1.setCategoryName(StringUtils.EMPTY);
    Mockito.when(productRecatStatusService.findByIdAndStatus(ID_4, RecatConstants.PUBLISHED))
        .thenReturn(productRecatStatus1);
    productRecatStatus1 = getProductRecatStatus(ID_5);
    productRecatStatus1.setNewCategoryName(StringUtils.EMPTY);
    Mockito.when(productRecatStatusService.findByIdAndStatus(ID_5, RecatConstants.PUBLISHED))
        .thenReturn(productRecatStatus1);
    productRecatStatus1 = getProductRecatStatus(ID_6);
    productRecatStatus1.setNewCategoryCode(StringUtils.EMPTY);
    Mockito.when(productRecatStatusService.findByIdAndStatus(ID_6, RecatConstants.PUBLISHED))
        .thenReturn(productRecatStatus1);
    Mockito.doThrow(ApplicationRuntimeException.class).when(productRecatStatusService)
        .updateProductCategory(productRecatStatusList.get(0));
    productCodeAndCategoryCodeListEvent.setProductCodeAndCategoryCodeEventList(productCodeAndCategoryCodeEventList);
    recatProcessServiceWrapper.updateProductCategory(productCodeAndCategoryCodeListEvent);
    Mockito.verify(productRecatStatusService).findByIdAndStatus(ID_1, RecatConstants.PUBLISHED);
    Mockito.verify(productRecatStatusService).findByIdAndStatus(ID_2, RecatConstants.PUBLISHED);
    Mockito.verify(productRecatStatusService).findByIdAndStatus(ID_3, RecatConstants.PUBLISHED);
    Mockito.verify(productRecatStatusService).findByIdAndStatus(ID_3, RecatConstants.PUBLISHED);
    Mockito.verify(productRecatStatusService).findByIdAndStatus(ID_4, RecatConstants.PUBLISHED);
    Mockito.verify(productRecatStatusService).findByIdAndStatus(ID_5, RecatConstants.PUBLISHED);
    Mockito.verify(productRecatStatusService).findByIdAndStatus(ID_6, RecatConstants.PUBLISHED);
    Mockito.verify(productRecatStatusService, Mockito.times(6))
        .validateResponseAndSave(productRecatStatusArgumentCaptor.capture(), stringArgumentCaptor.capture());
    List<ProductRecatStatus> productRecatStatuses = productRecatStatusArgumentCaptor.getAllValues();
    List<String> errorMessage = stringArgumentCaptor.getAllValues();
    Assertions.assertEquals(ID_1, productRecatStatuses.get(0).getId());
    Assertions.assertEquals(ID_2, productRecatStatuses.get(1).getId());
    Assertions.assertEquals(ID_3, productRecatStatuses.get(2).getId());
    Assertions.assertEquals(ID_4, productRecatStatuses.get(3).getId());
    Assertions.assertEquals(ID_5, productRecatStatuses.get(4).getId());
    Assertions.assertEquals(ID_6, productRecatStatuses.get(5).getId());
    Assertions.assertEquals(RecatConstants.PRODUCT_NAME_EMPTY, errorMessage.get(0));
    Assertions.assertEquals(RecatConstants.PRODUCT_CODE_EMPTY, errorMessage.get(1));
    Assertions.assertEquals(RecatConstants.CATEGORY_CODE_EMPTY, errorMessage.get(2));
    Assertions.assertEquals(RecatConstants.CATEGORY_NAME_EMPTY, errorMessage.get(3));
    Assertions.assertEquals(RecatConstants.NEW_CATEGORY_NAME_EMPTY, errorMessage.get(4));
    Assertions.assertEquals(RecatConstants.NEW_CATEGORY_CODE_EMPTY, errorMessage.get(5));
  }

  @Test
  public void getRecatProcessSummaryTest() {
    Mockito.when(this.recatProcessService
        .getRecatProcessSummary(STORE_ID, recatProcessSummaryRequest, PAGE, SIZE))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    Page<RecatProcessSummaryResponse> response = recatProcessServiceWrapper
        .getRecatProcessSummary(STORE_ID, recatProcessSummaryRequest, PAGE, SIZE);
    Mockito.verify(this.recatProcessService)
        .getRecatProcessSummary(STORE_ID, recatProcessSummaryRequest, PAGE, SIZE);
  }

  private ProductRecatStatus getProductRecatStatus(String id) {
    ProductRecatStatus productRecatStatus1 = new ProductRecatStatus();
    BeanUtils.copyProperties(productRecatStatus, productRecatStatus1);
    productRecatStatus1.setId(id);
    return productRecatStatus1;
  }

  @Test
  public void uploadNewRecatRequestTest() throws Exception {
    mockFile(ProcessorUtils.BULK_RECAT_DIR + RECAT_REQUEST_CODE + Constant.SLASH + FILE_NAME);
    multipartFile = new MockMultipartFile("request", FILE_NAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(
            ProcessorUtils.BULK_RECAT_DIR + RECAT_REQUEST_CODE + Constant.SLASH + FILE_NAME)));
    BulkInternalUploadRequestDTO uploadRequestDTO = BulkInternalUploadRequestDTO.builder().fileName(FILE_NAME)
      .bulkInternalProcessType(BulkInternalProcessType.RECATEGORISATION)
      .internalProcessRequestCode(RECAT_REQUEST_CODE).build();
    Mockito.when(fileStorageService.isFileExists(uploadRequestDTO)).thenReturn(Boolean.TRUE);
    recatProcessServiceWrapper
        .uploadNewRecatRequest(STORE_ID, RECAT_REQUEST_CODE, FILE_NAME, SCHEDULED_TIME);
    Mockito.verify(this.recatProcessService).saveRecatProcess(Mockito.any(RecatProcess.class));
    Mockito.verify(this.recatProcessService)
        .sendMailForNewRecatProcess(Mockito.any(RecatProcess.class));
    Mockito.verify(fileStorageService).isFileExists(uploadRequestDTO);
  }

  private void mockFile(String filePath) throws IOException {
    File file = new File(filePath);
    FileUtils.writeByteArrayToFile(file, fileContent);
  }

  @Test
  public void uploadNewRecatRequest_fileNotExistTest() {
    BulkInternalUploadRequestDTO uploadRequestDTO =
      BulkInternalUploadRequestDTO.builder().fileName(FILE_NAME_NOT_EXIST)
        .bulkInternalProcessType(BulkInternalProcessType.RECATEGORISATION)
        .internalProcessRequestCode(RECAT_REQUEST_CODE).build();
    Mockito.when(fileStorageService.isFileExists(uploadRequestDTO)).thenReturn(Boolean.FALSE);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> recatProcessServiceWrapper.uploadNewRecatRequest(STORE_ID, RECAT_REQUEST_CODE,
              FILE_NAME_NOT_EXIST, SCHEDULED_TIME));
    }
    finally {
      Mockito.verify(fileStorageService).isFileExists(uploadRequestDTO);
    }
  }

  @Test
  public void uploadNewRecatRequest_validScheduleTimeTest() throws Exception {
    mockFile(ProcessorUtils.BULK_RECAT_DIR + RECAT_REQUEST_CODE + Constant.SLASH + FILE_NAME);
    multipartFile = new MockMultipartFile("request", FILE_NAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(
            ProcessorUtils.BULK_RECAT_DIR + RECAT_REQUEST_CODE + Constant.SLASH + FILE_NAME)));
    BulkInternalUploadRequestDTO uploadRequestDTO = BulkInternalUploadRequestDTO.builder().fileName(FILE_NAME)
      .bulkInternalProcessType(BulkInternalProcessType.RECATEGORISATION)
      .internalProcessRequestCode(RECAT_REQUEST_CODE).build();
    Mockito.when(fileStorageService.isFileExists(uploadRequestDTO)).thenReturn(Boolean.TRUE);
    recatProcessServiceWrapper
        .uploadNewRecatRequest(STORE_ID, RECAT_REQUEST_CODE, FILE_NAME, VALID_SCHEDULED_TIME);
    Mockito.verify(this.recatProcessService).saveRecatProcess(Mockito.any(RecatProcess.class));
    Mockito.verify(this.recatProcessService)
        .sendMailForNewRecatProcess(Mockito.any(RecatProcess.class));
    Mockito.verify(fileStorageService).isFileExists(uploadRequestDTO);
  }

  @Test
  public void uploadNewRecatRequest_emptyScheduleTimeTest() throws Exception {
    BulkInternalUploadRequestDTO uploadRequestDTO = BulkInternalUploadRequestDTO.builder().fileName(FILE_NAME)
      .bulkInternalProcessType(BulkInternalProcessType.RECATEGORISATION)
      .internalProcessRequestCode(RECAT_REQUEST_CODE).build();
    Mockito.when(fileStorageService.isFileExists(uploadRequestDTO)).thenReturn(Boolean.TRUE);
    mockFile(ProcessorUtils.BULK_RECAT_DIR + RECAT_REQUEST_CODE + Constant.SLASH + FILE_NAME);
    multipartFile = new MockMultipartFile("request", FILE_NAME, "application/vnd.ms-excel",
        FileUtils.readFileToByteArray(new File(
            ProcessorUtils.BULK_RECAT_DIR + RECAT_REQUEST_CODE + Constant.SLASH + FILE_NAME)));
    recatProcessServiceWrapper
        .uploadNewRecatRequest(STORE_ID, RECAT_REQUEST_CODE, FILE_NAME, StringUtils.EMPTY);
    Mockito.verify(this.recatProcessService).saveRecatProcess(Mockito.any(RecatProcess.class));
    Mockito.verify(this.recatProcessService)
        .sendMailForNewRecatProcess(Mockito.any(RecatProcess.class));
    Mockito.verify(fileStorageService).isFileExists(uploadRequestDTO);
  }

  @Test
  public void updateRecatProcessFinalStatusFalseTest() {
    Mockito.when(
        systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.CHECK_PENDING_PRODUCTS))
        .thenReturn(updateRecatProcessFinalStatus);
    recatProcessServiceWrapper.updateRecatProcessFinalStatus(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.CHECK_PENDING_PRODUCTS);
  }

  @Test
  public void updateRecatProcessFinalStatusFalseExceptionTest() {
    Mockito.doThrow(ApplicationRuntimeException.class).when(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.CHECK_PENDING_PRODUCTS);
    recatProcessServiceWrapper.updateRecatProcessFinalStatus(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.CHECK_PENDING_PRODUCTS);
  }

  @Test
  public void updateRecatProcessFinalStatusEmptyInProgressTest() {
    updateRecatProcessFinalStatus.setValue(String.valueOf(true));
    Mockito.when(
        systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.CHECK_PENDING_PRODUCTS))
        .thenReturn(updateRecatProcessFinalStatus);
    recatProcessServiceWrapper.updateRecatProcessFinalStatus(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.CHECK_PENDING_PRODUCTS);
    Mockito.verify(recatProcessService).getRecatProcessByStoreIdAndStatus(STORE_ID, RecatConstants.IN_PROGRESS);
  }

  @Test
  public void updateRecatProcessFinalStatusPendingProductsTest() {
    updateRecatProcessFinalStatus.setValue(String.valueOf(true));
    Mockito.when(
        systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.CHECK_PENDING_PRODUCTS))
        .thenReturn(updateRecatProcessFinalStatus);
    Mockito.when(recatProcessService.getRecatProcessByStoreIdAndStatus(STORE_ID, RecatConstants.IN_PROGRESS))
        .thenReturn(Collections.singletonList(recatProcess));
    Mockito.when(productRecatStatusService
        .findCountByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PENDING,
            recatProcess.getRecatRequestCode())).thenReturn(10);
    recatProcessServiceWrapper.updateRecatProcessFinalStatus(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.CHECK_PENDING_PRODUCTS);
    Mockito.verify(recatProcessService).getRecatProcessByStoreIdAndStatus(STORE_ID, RecatConstants.IN_PROGRESS);
    Mockito.verify(productRecatStatusService)
        .findCountByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PENDING,
            recatProcess.getRecatRequestCode());
  }

  @Test
  public void updateRecatProcessFinalStatusInProgressErrorTest() {
    updateRecatProcessFinalStatus.setValue(String.valueOf(true));
    Mockito.when(
        systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.CHECK_PENDING_PRODUCTS))
        .thenReturn(updateRecatProcessFinalStatus);
    Mockito.when(recatProcessService.getRecatProcessByStoreIdAndStatus(STORE_ID, RecatConstants.IN_PROGRESS))
        .thenReturn(Collections.singletonList(recatProcess));
    Mockito.doThrow(ApplicationRuntimeException.class).when(productRecatStatusService)
        .findCountByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PENDING,
            recatProcess.getRecatRequestCode());
    recatProcessServiceWrapper.updateRecatProcessFinalStatus(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.CHECK_PENDING_PRODUCTS);
    Mockito.verify(recatProcessService).getRecatProcessByStoreIdAndStatus(STORE_ID, RecatConstants.IN_PROGRESS);
    Mockito.verify(productRecatStatusService)
        .findCountByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PENDING,
            recatProcess.getRecatRequestCode());
  }

  @Test
  public void updateRecatProcessFinalStatusPublishedProductsTest() {
    updateRecatProcessFinalStatus.setValue(String.valueOf(true));
    Mockito.when(
        systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.CHECK_PENDING_PRODUCTS))
        .thenReturn(updateRecatProcessFinalStatus);
    Mockito.when(recatProcessService.getRecatProcessByStoreIdAndStatus(STORE_ID, RecatConstants.IN_PROGRESS))
        .thenReturn(Collections.singletonList(recatProcess));
    Mockito.when(productRecatStatusService
        .findCountByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PENDING,
            recatProcess.getRecatRequestCode())).thenReturn(0);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PUBLISHED_PRODUCTS_UPDATED_TIME_THRESHOLD))
        .thenReturn(changedToFailedThresholdConfig);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE))
        .thenReturn(saveRecatProcessDbBatchSizeConfig);
    Mockito.when(productRecatStatusService.findByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PUBLISHED,
        recatProcess.getRecatRequestCode())).thenReturn(Collections.singletonList(productRecatStatus));
    recatProcessServiceWrapper.updateRecatProcessFinalStatus(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.CHECK_PENDING_PRODUCTS);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PUBLISHED_PRODUCTS_UPDATED_TIME_THRESHOLD);
    Mockito.verify(recatProcessService).getRecatProcessByStoreIdAndStatus(STORE_ID, RecatConstants.IN_PROGRESS);
    Mockito.verify(productRecatStatusService)
        .findCountByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PENDING,
            recatProcess.getRecatRequestCode());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE);
    Mockito.verify(productRecatStatusService)
        .findByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PUBLISHED,
            recatProcess.getRecatRequestCode());
  }

  @Test
  public void updateRecatProcessFinalStatusPublished2ProductsTest() {
    updateRecatProcessFinalStatus.setValue(String.valueOf(true));
    Mockito.when(
        systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.CHECK_PENDING_PRODUCTS))
        .thenReturn(updateRecatProcessFinalStatus);
    Mockito.when(recatProcessService.getRecatProcessByStoreIdAndStatus(STORE_ID, RecatConstants.IN_PROGRESS))
        .thenReturn(Collections.singletonList(recatProcess));
    Mockito.when(productRecatStatusService
        .findCountByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PENDING,
            recatProcess.getRecatRequestCode())).thenReturn(0);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PUBLISHED_PRODUCTS_UPDATED_TIME_THRESHOLD))
        .thenReturn(changedToFailedThresholdConfig);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE))
        .thenReturn(saveRecatProcessDbBatchSizeConfig);
    Mockito.when(productRecatStatusService.findByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PUBLISHED,
        recatProcess.getRecatRequestCode())).thenReturn(productRecatStatusList);
    recatProcessServiceWrapper.updateRecatProcessFinalStatus(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.CHECK_PENDING_PRODUCTS);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PUBLISHED_PRODUCTS_UPDATED_TIME_THRESHOLD);
    Mockito.verify(recatProcessService).getRecatProcessByStoreIdAndStatus(STORE_ID, RecatConstants.IN_PROGRESS);
    Mockito.verify(productRecatStatusService)
        .findCountByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PENDING,
            recatProcess.getRecatRequestCode());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE);
    Mockito.verify(productRecatStatusService)
        .findByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PUBLISHED,
            recatProcess.getRecatRequestCode());
    Mockito.verify(productRecatStatusService)
        .saveProductRecatStatusList(productRecatStatusListArgumentCaptor.capture());
    Assertions.assertEquals(RecatConstants.FAILED, productRecatStatusListArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertEquals(1, productRecatStatusListArgumentCaptor.getValue().size());
    Assertions.assertTrue(productRecatStatusListArgumentCaptor.getValue().get(0).isSystemError());
    Assertions.assertEquals(Constant.SYSTEM_ERROR, productRecatStatusListArgumentCaptor.getValue().get(0).getErrorMessage());
  }

  @Test
  public void updateRecatProcessFinalStatusUpdateStatusTest() {
    updateRecatProcessFinalStatus.setValue(String.valueOf(true));
    Mockito.when(
        systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.CHECK_PENDING_PRODUCTS))
        .thenReturn(updateRecatProcessFinalStatus);
    Mockito.when(recatProcessService.getRecatProcessByStoreIdAndStatus(STORE_ID, RecatConstants.IN_PROGRESS))
        .thenReturn(Collections.singletonList(recatProcess));
    Mockito.when(productRecatStatusService
        .findCountByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PENDING,
            recatProcess.getRecatRequestCode())).thenReturn(0);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PUBLISHED_PRODUCTS_UPDATED_TIME_THRESHOLD))
        .thenReturn(changedToFailedThresholdConfig);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE))
        .thenReturn(saveRecatProcessDbBatchSizeConfig);
    recatProcessServiceWrapper.updateRecatProcessFinalStatus(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.CHECK_PENDING_PRODUCTS);
    Mockito.verify(recatProcessService).getRecatProcessByStoreIdAndStatus(STORE_ID, RecatConstants.IN_PROGRESS);
    Mockito.verify(productRecatStatusService)
        .findCountByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PENDING,
            recatProcess.getRecatRequestCode());
    Mockito.verify(productRecatStatusService)
        .findByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PUBLISHED,
            recatProcess.getRecatRequestCode());
    Mockito.verify(productRecatStatusService)
        .getStatusCountByStoreIdAndRecatRequestCount(STORE_ID, recatProcess.getRecatRequestCode());
    Mockito.verify(recatProcessService).updateFinalStatus(recatProcess, new HashMap<>());
    Mockito.verify(recatProcessService).deleteRecatFile(
        ProcessorUtils.BULK_RECAT_DIR + recatProcess.getRecatRequestCode() + Constant.SLASH + recatProcess
            .getFileName());
  }

  @Test
  public void updateRecatProcessFinalStatusPublishedMovedToFailedProductsTest() {
    updateRecatProcessFinalStatus.setValue(String.valueOf(true));
    Mockito.when(
        systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.CHECK_PENDING_PRODUCTS))
        .thenReturn(updateRecatProcessFinalStatus);
    Mockito.when(recatProcessService.getRecatProcessByStoreIdAndStatus(STORE_ID, RecatConstants.IN_PROGRESS))
        .thenReturn(Collections.singletonList(recatProcess));
    Mockito.when(productRecatStatusService
        .findCountByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PENDING,
            recatProcess.getRecatRequestCode())).thenReturn(0);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PUBLISHED_PRODUCTS_UPDATED_TIME_THRESHOLD))
        .thenReturn(changedToFailedThresholdConfig);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE))
        .thenReturn(saveRecatProcessDbBatchSizeConfig);
    Mockito.when(productRecatStatusService.findByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PUBLISHED,
        recatProcess.getRecatRequestCode())).thenReturn(Collections.singletonList(productRecatStatusList.get(1)));
    recatProcessServiceWrapper.updateRecatProcessFinalStatus(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.CHECK_PENDING_PRODUCTS);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PUBLISHED_PRODUCTS_UPDATED_TIME_THRESHOLD);
    Mockito.verify(recatProcessService).getRecatProcessByStoreIdAndStatus(STORE_ID, RecatConstants.IN_PROGRESS);
    Mockito.verify(productRecatStatusService)
        .findCountByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PENDING,
            recatProcess.getRecatRequestCode());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE);
    Mockito.verify(productRecatStatusService)
        .findByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PUBLISHED,
            recatProcess.getRecatRequestCode());
    Mockito.verify(productRecatStatusService)
        .saveProductRecatStatusList(productRecatStatusListArgumentCaptor.capture());
    Mockito.verify(productRecatStatusService)
        .getStatusCountByStoreIdAndRecatRequestCount(STORE_ID, recatProcess.getRecatRequestCode());
    Mockito.verify(recatProcessService).updateFinalStatus(recatProcess, new HashMap<>());
    Mockito.verify(recatProcessService).deleteRecatFile(
        ProcessorUtils.BULK_RECAT_DIR + recatProcess.getRecatRequestCode() + Constant.SLASH + recatProcess
            .getFileName());
    Assertions.assertEquals(RecatConstants.FAILED, productRecatStatusListArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertEquals(1, productRecatStatusListArgumentCaptor.getValue().size());
    Assertions.assertTrue(productRecatStatusListArgumentCaptor.getValue().get(0).isSystemError());
    Assertions.assertEquals(Constant.SYSTEM_ERROR, productRecatStatusListArgumentCaptor.getValue().get(0).getErrorMessage());
  }

  @Test
  public void updateRecatProcessFinalStatusSendMailTest() throws Exception {
    updateRecatProcessFinalStatus.setValue(String.valueOf(true));
    Mockito.when(
        systemParameterConfigService.findValueByStoreIdAndVariable(STORE_ID, RecatConstants.CHECK_PENDING_PRODUCTS))
        .thenReturn(updateRecatProcessFinalStatus);
    Mockito.when(recatProcessService.getRecatProcessByStoreIdAndStatus(STORE_ID, RecatConstants.IN_PROGRESS))
        .thenReturn(Collections.singletonList(recatProcess));
    Mockito.when(productRecatStatusService
        .findCountByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PENDING,
            recatProcess.getRecatRequestCode())).thenReturn(0);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PUBLISHED_PRODUCTS_UPDATED_TIME_THRESHOLD))
        .thenReturn(changedToFailedThresholdConfig);
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE))
        .thenReturn(saveRecatProcessDbBatchSizeConfig);
    Mockito.when(productRecatStatusService.findByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PUBLISHED,
        recatProcess.getRecatRequestCode())).thenReturn(Collections.singletonList(productRecatStatusList.get(1)));
    Mockito.when(recatProcessService.updateFinalStatus(recatProcess, new HashMap<>())).thenReturn(recatProcess);
    recatProcessServiceWrapper.updateRecatProcessFinalStatus(STORE_ID);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.CHECK_PENDING_PRODUCTS);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.PUBLISHED_PRODUCTS_UPDATED_TIME_THRESHOLD);
    Mockito.verify(recatProcessService).getRecatProcessByStoreIdAndStatus(STORE_ID, RecatConstants.IN_PROGRESS);
    Mockito.verify(productRecatStatusService)
        .findCountByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PENDING,
            recatProcess.getRecatRequestCode());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE);
    Mockito.verify(productRecatStatusService)
        .findByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PUBLISHED,
            recatProcess.getRecatRequestCode());
    Mockito.verify(productRecatStatusService)
        .saveProductRecatStatusList(productRecatStatusListArgumentCaptor.capture());
    Mockito.verify(productRecatStatusService)
        .getStatusCountByStoreIdAndRecatRequestCount(STORE_ID, recatProcess.getRecatRequestCode());
    Mockito.verify(recatProcessService).updateFinalStatus(recatProcess, new HashMap<>());
    Mockito.verify(recatProcessService).sendMailForFinishedRecatProcess(recatProcessListArgumentCaptor.capture());
    Mockito.verify(recatProcessService).deleteRecatFile(
        ProcessorUtils.BULK_RECAT_DIR + recatProcess.getRecatRequestCode() + Constant.SLASH + recatProcess
            .getFileName());
    Assertions.assertEquals(RecatConstants.FAILED, productRecatStatusListArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertEquals(1, productRecatStatusListArgumentCaptor.getValue().size());
    Assertions.assertTrue(productRecatStatusListArgumentCaptor.getValue().get(0).isSystemError());
    Assertions.assertEquals(Constant.SYSTEM_ERROR, productRecatStatusListArgumentCaptor.getValue().get(0).getErrorMessage());
  }

  @Test
  public void getFailedProductsMailTest() throws Exception{
    Mockito.when(recatProcessService.getRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE))
        .thenReturn(this.recatProcess);
    recatProcessServiceWrapper.getFailedProductsMail(STORE_ID, RECAT_REQUEST_CODE, USERNAME, REQUEST_ID);
    Mockito.verify(recatProcessService).getRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
  }

  @Test
  public void getFailedProductsMailStatusFailedTest() throws Exception{
    this.recatProcess.setStatus(RecatConstants.FAILED);
    Mockito.when(recatProcessService.getRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE))
        .thenReturn(this.recatProcess);
    Mockito.when(bulkDownloadServiceBeanUtil.getRecatFailedProductsDownloadRequest(RECAT_REQUEST_CODE, USERNAME, REQUEST_ID))
        .thenReturn(null);
    recatProcessServiceWrapper.getFailedProductsMail(STORE_ID, RECAT_REQUEST_CODE, USERNAME, REQUEST_ID);
    Mockito.verify(recatProcessService).getRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Mockito.verify(bulkDownloadServiceBeanUtil).getRecatFailedProductsDownloadRequest(RECAT_REQUEST_CODE, USERNAME, REQUEST_ID);
    Mockito.verify(bulkProcessDownloadService).downloadExcelFile(null, BulkProcessPath.RECAT.getValue());
    Mockito.verify(recatProcessService).sendMailForRecatFailedProducts(recatProcess, USERNAME);
  }

  @Test
  public void getFailedProductsMailStatusPartialSuccessTest() throws Exception{
    this.recatProcess.setStatus(RecatConstants.PARTIAL_SUCCESS);
    Mockito.when(recatProcessService.getRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE))
        .thenReturn(this.recatProcess);
    Mockito.when(bulkDownloadServiceBeanUtil.getRecatFailedProductsDownloadRequest(RECAT_REQUEST_CODE, USERNAME, REQUEST_ID))
        .thenReturn(null);
    recatProcessServiceWrapper.getFailedProductsMail(STORE_ID, RECAT_REQUEST_CODE, USERNAME, REQUEST_ID);
    Mockito.verify(recatProcessService).getRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Mockito.verify(bulkDownloadServiceBeanUtil).getRecatFailedProductsDownloadRequest(RECAT_REQUEST_CODE, USERNAME, REQUEST_ID);
    Mockito.verify(bulkProcessDownloadService).downloadExcelFile(null, BulkProcessPath.RECAT.getValue());
    Mockito.verify(recatProcessService).sendMailForRecatFailedProducts(recatProcess, USERNAME);
  }

  @Test
  public void cancelRecatRequestNotFoundTest() {
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> recatProcessServiceWrapper.cancelRecatRequest(STORE_ID, RECAT_REQUEST_CODE, false,
              USER_NAME));
    } finally {
      Mockito.verify(recatProcessService).findRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    }
  }

  @Test
  public void cancelRecatRequestInValidTest() {
    recatProcess.setStatus(RecatConstants.IN_PROGRESS);
    Mockito.when(recatProcessService.findRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE))
        .thenReturn(recatProcess);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> recatProcessServiceWrapper.cancelRecatRequest(STORE_ID, RECAT_REQUEST_CODE, false,
              USER_NAME));
    } finally {
      Mockito.verify(recatProcessService).findRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    }
  }

  @Test
  public void cancelRecatRequestTest() {
    recatProcess.setStatus(RecatConstants.NEW);
    Mockito.when(recatProcessService.findRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE))
        .thenReturn(recatProcess);
    recatProcessServiceWrapper.cancelRecatRequest(STORE_ID, RECAT_REQUEST_CODE, false, USER_NAME);
    Mockito.verify(recatProcessService).findRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Mockito.verify(recatProcessService).saveRecatProcess(recatProcessArgumentCaptor.capture());
    Assertions.assertEquals(RecatConstants.CANCELLED, recatProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(RecatConstants.CANCELLED_BY_USER + USER_NAME, recatProcessArgumentCaptor.getValue().getNotes());
  }

  @Test
  public void cancelRecatRequestForceUpdateEmptyListTest() {
    recatProcess.setStatus(RecatConstants.IN_PROGRESS);
    Mockito.when(recatProcessService.findRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE))
        .thenReturn(recatProcess);
    recatProcessServiceWrapper.cancelRecatRequest(STORE_ID, RECAT_REQUEST_CODE, true, USER_NAME);
    Mockito.verify(recatProcessService).findRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Mockito.verify(recatProcessService).saveRecatProcess(recatProcessArgumentCaptor.capture());
    Mockito.verify(productRecatStatusService)
        .findByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PENDING, RECAT_REQUEST_CODE);
    Assertions.assertEquals(RecatConstants.CANCELLED, recatProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(RecatConstants.CANCELLED_BY_USER + USER_NAME, recatProcessArgumentCaptor.getValue().getNotes());
  }

  @Test
  public void cancelRecatRequestForceUpdateTest() {
    recatProcess.setStatus(RecatConstants.IN_PROGRESS);
    Mockito.when(recatProcessService.findRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE))
        .thenReturn(recatProcess);
    Mockito.when(productRecatStatusService
        .findByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PENDING, RECAT_REQUEST_CODE))
        .thenReturn(Collections.singletonList(productRecatStatus));
    Mockito.when(systemParameterConfigService
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE))
        .thenReturn(saveRecatProcessDbBatchSizeConfig);
    recatProcessServiceWrapper.cancelRecatRequest(STORE_ID, RECAT_REQUEST_CODE, true, USER_NAME);
    Mockito.verify(recatProcessService).findRecatProcessByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
    Mockito.verify(recatProcessService).saveRecatProcess(recatProcessArgumentCaptor.capture());
    Mockito.verify(productRecatStatusService)
        .findByStoreIdAndStatusAndRecatRequestCode(STORE_ID, RecatConstants.PENDING, RECAT_REQUEST_CODE);
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(STORE_ID, RecatConstants.SAVE_RECAT_PROCESS_BATCH_SIZE);
    Mockito.verify(productRecatStatusService)
        .saveProductRecatStatusList(productRecatStatusListArgumentCaptor.capture());
    Assertions.assertEquals(RecatConstants.CANCELLED, recatProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(RecatConstants.CANCELLED_BY_USER + USER_NAME, recatProcessArgumentCaptor.getValue().getNotes());
    Assertions.assertEquals(RecatConstants.FAILED, productRecatStatusListArgumentCaptor.getValue().get(0).getStatus());
    Assertions.assertEquals(RecatConstants.CANCELLED_BY_SYSTEM,
        productRecatStatusListArgumentCaptor.getValue().get(0).getErrorMessage());
  }

  @Test
  public void processNewRecatProcessInvalidSheetTypeTest() throws IOException {
    Sheet invalidSheet = Mockito.mock(Sheet.class);
    Mockito.when(
            fileStorageService.getFileDataWithInternalUploadRequest(Mockito.any(BulkInternalUploadRequestDTO.class)))
        .thenReturn(invalidSheet);
    RecatProcess recatProcess = new RecatProcess();
    recatProcess.setFileName("invalid-sheet.xlsx");
    recatProcess.setRecatRequestCode("test-request-code");

    recatProcessServiceWrapper.processRecatFile(STORE_ID, recatProcess);
    Mockito.verify(recatProcessService).saveRecatProcess(recatProcessArgumentCaptor.capture());
    Assertions.assertEquals(RecatConstants.CANCELLED, recatProcessArgumentCaptor.getValue().getStatus());
    Mockito.verify(fileStorageService).getFileDataWithInternalUploadRequest(
        BulkInternalUploadRequestDTO.builder().fileName(recatProcess.getFileName()).relativePath(RECAT)
            .bulkInternalProcessType(BulkInternalProcessType.RECATEGORISATION)
            .internalProcessRequestCode(recatProcess.getRecatRequestCode()).build());
  }

  @Test
  public void processRecatFileWorkbookNotNullTest() throws IOException {
    XSSFSheet mockSheet = Mockito.mock(XSSFSheet.class);
    XSSFWorkbook mockWorkbook = Mockito.mock(XSSFWorkbook.class);
    Mockito.when(mockSheet.getWorkbook()).thenReturn(mockWorkbook);
    Mockito.when(
            fileStorageService.getFileDataWithInternalUploadRequest(Mockito.any(BulkInternalUploadRequestDTO.class)))
        .thenReturn(mockSheet);
    RecatProcess recatProcess = new RecatProcess();
    recatProcess.setFileName("test-file.xlsx");
    recatProcess.setRecatRequestCode("test-request-code");
    Mockito.doThrow(new RuntimeException("Test Exception")).when(mockWorkbook).close();
    recatProcessServiceWrapper.processRecatFile(STORE_ID, recatProcess);
    Mockito.verify(fileStorageService).getFileDataWithInternalUploadRequest(
        BulkInternalUploadRequestDTO.builder().fileName(recatProcess.getFileName()).relativePath(RECAT)
            .bulkInternalProcessType(BulkInternalProcessType.RECATEGORISATION)
            .internalProcessRequestCode(recatProcess.getRecatRequestCode()).build());
    Mockito.verify(mockWorkbook).close();
    Mockito.verify(recatProcessService).saveRecatProcess(recatProcessArgumentCaptor.capture());
    Assertions.assertEquals(RecatConstants.CANCELLED, recatProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(Constant.SYSTEM_ERROR, recatProcessArgumentCaptor.getValue().getNotes());
  }

  @Test
  public void closeWorkbook() throws IOException {
    recatProcessServiceWrapper.closeWorkbook(null);
  }

}