package com.gdn.mta.bulk.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

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
import org.springframework.data.domain.PageImpl;

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.SuspensionProductRequest;
import com.gda.mta.product.dto.response.SuspensionProductResponse;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.config.SystemParameter;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.dto.BulkInternalUploadRequestDTO;
import com.gdn.mta.bulk.dto.product.BulkProductSuspensionRequest;
import com.gdn.mta.bulk.entity.BulkInternalEventModel;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.repository.ProductLevel3Repository;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.BulkProductSuspensionParameters;

public class
BulkProductSuspensionServiceBeanTest {
  private static final String BULK_UPDATE_FOLDER = "BulkUpdate";
  private static final String FILE_NAME = "ProductSuspension.xlsx";
  private static final String PARTIAL_SUCCESS_FILE_NAME = "ProductSuspensionPartialSuccess.xlsx";
  private static final String SUSPENSION_FAILED_FILE_NAME = "ProductSuspensionSuspensionActionFailed.xlsx";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String STORE_ID = "storeId";
  private static final String UPDATED_BY = "updatedBy";
  private static final String DEFAULT_BULK_PROCESS_TYPE = "ProductLevel3";
  private static final String REQUEST_ID = "requestId";
  private static final int PRODUCT_SUSPENSION_ASSIGNMENT_BATCH_SIZE = 10;
  private static final String ACTION_TYPE = "SUSPEND";
  private static final String REACTIVATE = "REACTIVATE";
  private static final String PRODUCT_CODE = "MTA-0520153";
  private static final String MERCHANT_CODE = "TOQ-15131";
  private static final String ERROR_MESSGAGE = "ERROR_MESSAGE";
  private static final String SELLER_CODE = "seller_code";
  private static String partialSuccessFilePath;
  private static String productSuspensionSuspensionActionFailedFilePath;
  private static final String SUSPENSION_FAILED_FULLY_FILE_NAME = "ProductSuspensionSuspensionActionFullyFailed.xlsx";
  private static String productSuspensionSuspensionActionFullFailedFilePath;

  private BulkProductSuspensionRequest bulkProductSuspensionRequest;
  private BulkInternalProcess bulkInternalProcess;
  private SuspensionProductResponse suspensionProductResponse;
  private SystemParameterConfig systemParameterConfig;
  private BulkDownloadRequest bulkDownloadRequest;
  private BulkInternalEventModel bulkInternalEventModel;
  private BulkInternalUploadRequestDTO bulkInternalUploadRequestDTO;

  @InjectMocks
  private BulkProductSuspensionServiceBean bulkProductSuspensionServiceBean;

  @Mock
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Mock
  private SystemParameter systemParameter;

  @Mock
  private ProductLevel3Repository productLevel3Repository;

  @Captor
  private ArgumentCaptor<List<SuspensionProductRequest>> bulkProductSuspensionRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<BulkInternalProcess> bulkProcessArgumentCaptor;

  @Mock
  private MailDeliveryService mailDeliveryService;

  @Mock
  private InternalProcessService internalProcessService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private File partialSuccessFile;
  private File partialSuspensionFailedFile;
  private File file;
  private File fullSuspensionFailedFile;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    ClassLoader classLoader = getClass().getClassLoader();
    file =
      new File(classLoader.getResource(BULK_UPDATE_FOLDER + File.separator + FILE_NAME).getFile());
    String filePath = file.getAbsolutePath();

     partialSuccessFile =
        new File(classLoader.getResource(BULK_UPDATE_FOLDER + File.separator + PARTIAL_SUCCESS_FILE_NAME).getFile());
    partialSuccessFilePath = partialSuccessFile.getAbsolutePath();

    partialSuspensionFailedFile =
        new File(classLoader.getResource(BULK_UPDATE_FOLDER + File.separator + SUSPENSION_FAILED_FILE_NAME).getFile());
    productSuspensionSuspensionActionFailedFilePath = partialSuspensionFailedFile.getAbsolutePath();

    fullSuspensionFailedFile = new File(
        classLoader.getResource(BULK_UPDATE_FOLDER + File.separator + SUSPENSION_FAILED_FULLY_FILE_NAME).getFile());
    productSuspensionSuspensionActionFullFailedFilePath = fullSuspensionFailedFile.getAbsolutePath();

    bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setStartTime(Calendar.getInstance().getTime());
    bulkInternalProcess.setInternalProcessRequestCode(BULK_PROCESS_CODE);
    bulkInternalProcess.setStoreId(STORE_ID);
    bulkInternalProcess.setStatus(BulkProcess.STATUS_PENDING);
    bulkInternalProcess.setProcessType("SUSPEND");
    bulkInternalProcess.setSuccessCount(0);
    bulkInternalProcess.setErrorCount(0);
    bulkInternalProcess.setFileName(FILE_NAME);
    bulkInternalProcess.setUpdatedBy(UPDATED_BY);
    bulkInternalProcess.setCreatedBy(UPDATED_BY);
    bulkInternalProcess.setSellerCode(SELLER_CODE);

    bulkProductSuspensionRequest = new BulkProductSuspensionRequest();
    bulkProductSuspensionRequest.setRequestId(REQUEST_ID);
    bulkProductSuspensionRequest.setStoreId(STORE_ID);
    bulkProductSuspensionRequest.setActionType(ACTION_TYPE);
    bulkProductSuspensionRequest.setBulkProcessType(DEFAULT_BULK_PROCESS_TYPE);
    bulkProductSuspensionRequest.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkProductSuspensionRequest.setFilePath(filePath);
    bulkProductSuspensionRequest.setUpdatedBy(UPDATED_BY);

    suspensionProductResponse = new SuspensionProductResponse();
    suspensionProductResponse.setBusinessPartnerCode(SELLER_CODE);
    suspensionProductResponse.setProductCode(PRODUCT_CODE);
    suspensionProductResponse.setErrorMessage(ERROR_MESSGAGE);
    when(internalProcessService.findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(null);
    when(internalProcessService.saveInternalProcess(Mockito.any(BulkInternalProcess.class)))
        .thenReturn(bulkInternalProcess);
    systemParameterConfig = new SystemParameterConfig(Constant.BULK_SUSPENSION_SWITCH, "false", "");
    when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_SUSPENSION_SWITCH))
        .thenReturn(systemParameterConfig);
    when(objectMapper.writeValueAsString(any())).thenReturn("Data");
    when(internalProcessService.saveInternalProcessData(anyList())).thenReturn(new ArrayList());
    when(internalProcessService.saveInternalProcess(any(BulkInternalProcess.class)))
        .thenReturn(new BulkInternalProcess());

    bulkInternalEventModel = BulkInternalEventModel.builder().internalProcessRequestCode(BULK_PROCESS_CODE)
        .processCode(Arrays.asList(PRODUCT_CODE)).requestId(REQUEST_ID).storeId(STORE_ID).build();
    File file1 = new File(
      classLoader.getResource(BULK_UPDATE_FOLDER + File.separator + FILE_NAME).getFile());
    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(file1);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class)))
      .thenReturn(sheet);
  }

  @Test
  public void processTest() throws Exception {
    when(systemParameter.getProductSuspensionBulkUploadBatchSize())
        .thenReturn(PRODUCT_SUSPENSION_ASSIGNMENT_BATCH_SIZE);
    when(bulkUpdateServiceUtil.getBulkProcess(bulkProductSuspensionRequest, 0, 0)).thenReturn(bulkInternalProcess);
    when(internalProcessService.saveInternalProcess(bulkInternalProcess)).thenReturn(bulkInternalProcess);
    when(productLevel3Repository.doBulkSuspensionProductsActions(anyList(), anyString(), anyString()))
        .thenReturn(new ArrayList());
    doNothing().when(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    bulkProductSuspensionServiceBean.process(bulkProductSuspensionRequest);

    verify(systemParameter).getProductSuspensionBulkUploadBatchSize();
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(bulkUpdateServiceUtil).getBulkProcess(bulkProductSuspensionRequest, 0, 0);
    verify(internalProcessService, times(3)).saveInternalProcess(bulkInternalProcess);
    verify(productLevel3Repository).doBulkSuspensionProductsActions(Mockito.anyList(), Mockito.eq(REQUEST_ID),
        Mockito.eq(bulkProductSuspensionRequest.getUpdatedBy()));
    verify(internalProcessService, times(3)).saveInternalProcess(bulkProcessArgumentCaptor.capture());
    verify(mailDeliveryService).sendEmail(any(BulkDownloadRequest.class), anyMap());
    verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
    verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_SUSPENSION_SWITCH);
    verify(fileStorageService).getFilePrefix(Mockito.anyString());
    verify(fileStorageService).getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class));
    Assertions.assertEquals(Integer.valueOf(6), bulkProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(Integer.valueOf(6), bulkProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(ProcessStatus.COMPLETED.name(), bulkProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void processTest_AlreadyExist() throws Exception {
    when(internalProcessService.findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkInternalProcess);
    bulkProductSuspensionServiceBean.process(bulkProductSuspensionRequest);
    verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
  }

  @Test
  public void processTestForPartialValidation() throws Exception {
    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(partialSuccessFile);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class)))
      .thenReturn(sheet);
    bulkInternalUploadRequestDTO = new BulkInternalUploadRequestDTO();
    bulkInternalUploadRequestDTO.setRelativePath(partialSuccessFilePath);
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.SUSPEND);
    bulkProductSuspensionRequest.setFilePath(partialSuccessFilePath);
    when(systemParameter.getProductSuspensionBulkUploadBatchSize())
        .thenReturn(PRODUCT_SUSPENSION_ASSIGNMENT_BATCH_SIZE);
    doNothing().when(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    when(bulkUpdateServiceUtil.getBulkProcess(bulkProductSuspensionRequest, 0, 0)).thenReturn(bulkInternalProcess);
    when(internalProcessService.saveInternalProcess(bulkInternalProcess)).thenReturn(bulkInternalProcess);
    when(productLevel3Repository.doBulkSuspensionProductsActions(anyList(), anyString(), anyString()))
        .thenReturn(new ArrayList());

    bulkProductSuspensionServiceBean.process(bulkProductSuspensionRequest);

    verify(systemParameter).getProductSuspensionBulkUploadBatchSize();
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_SUSPENSION_SWITCH);
    verify(bulkUpdateServiceUtil).getBulkProcess(bulkProductSuspensionRequest, 0, 0);
    verify(internalProcessService, times(3)).saveInternalProcess(bulkInternalProcess);
    verify(productLevel3Repository).doBulkSuspensionProductsActions(Mockito.anyList(), Mockito.eq(REQUEST_ID),
        Mockito.eq(bulkProductSuspensionRequest.getUpdatedBy()));
    verify(internalProcessService, times(3)).saveInternalProcess(bulkProcessArgumentCaptor.capture());
    verify(mailDeliveryService).sendEmail(any(BulkDownloadRequest.class), anyMap());
    verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
    verify(fileStorageService).getFilePrefix(Mockito.anyString());
    Assertions.assertEquals(Integer.valueOf(4), bulkProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(Integer.valueOf(5), bulkProcessArgumentCaptor.getValue().getErrorCount());
    Assertions.assertEquals(Integer.valueOf(9), bulkProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(ProcessStatus.PARTIAL_COMPLETED.name(), bulkProcessArgumentCaptor.getValue().getStatus());
    verify(fileStorageService).getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class));
  }

  @Test
  public void processTestForPartialValidationWithEmptySellerCode() throws Exception {
    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(partialSuccessFile);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class)))
      .thenReturn(sheet);
    bulkInternalUploadRequestDTO = new BulkInternalUploadRequestDTO();
    bulkInternalUploadRequestDTO.setRelativePath(partialSuccessFilePath);
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.SUSPEND);
    bulkProductSuspensionRequest.setFilePath(partialSuccessFilePath);
    when(systemParameter.getProductSuspensionBulkUploadBatchSize())
      .thenReturn(PRODUCT_SUSPENSION_ASSIGNMENT_BATCH_SIZE);
    doNothing().when(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    when(bulkUpdateServiceUtil.getBulkProcess(bulkProductSuspensionRequest, 0, 0)).thenReturn(bulkInternalProcess);
    when(internalProcessService.saveInternalProcess(bulkInternalProcess)).thenReturn(bulkInternalProcess);
    when(productLevel3Repository.doBulkSuspensionProductsActions(anyList(), anyString(), anyString()))
      .thenReturn(new ArrayList());

    bulkProductSuspensionServiceBean.process(bulkProductSuspensionRequest);

    verify(systemParameter).getProductSuspensionBulkUploadBatchSize();
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(this.systemParameterConfigService)
      .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_SUSPENSION_SWITCH);
    verify(bulkUpdateServiceUtil).getBulkProcess(bulkProductSuspensionRequest, 0, 0);
    verify(internalProcessService, times(3)).saveInternalProcess(bulkInternalProcess);
    verify(productLevel3Repository).doBulkSuspensionProductsActions(Mockito.anyList(), Mockito.eq(REQUEST_ID),
      Mockito.eq(bulkProductSuspensionRequest.getUpdatedBy()));
    verify(internalProcessService, times(3)).saveInternalProcess(bulkProcessArgumentCaptor.capture());
    verify(mailDeliveryService).sendEmail(any(BulkDownloadRequest.class), anyMap());
    verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
    verify(fileStorageService).getFilePrefix(Mockito.anyString());
    Assertions.assertEquals(Integer.valueOf(4), bulkProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(Integer.valueOf(5), bulkProcessArgumentCaptor.getValue().getErrorCount());
    Assertions.assertEquals(Integer.valueOf(9), bulkProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(ProcessStatus.PARTIAL_COMPLETED.name(), bulkProcessArgumentCaptor.getValue().getStatus());
    verify(fileStorageService).getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class));
  }

  @Test
  public void processTestForFailedReasonValidation() throws Exception {
    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(partialSuspensionFailedFile);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class)))
      .thenReturn(sheet);
    bulkInternalUploadRequestDTO = new BulkInternalUploadRequestDTO();
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.SUSPEND);
    bulkInternalUploadRequestDTO.setRelativePath(productSuspensionSuspensionActionFailedFilePath);
    bulkProductSuspensionRequest.setFilePath(productSuspensionSuspensionActionFailedFilePath);
    when(systemParameter.getProductSuspensionBulkUploadBatchSize())
        .thenReturn(PRODUCT_SUSPENSION_ASSIGNMENT_BATCH_SIZE);
    when(bulkUpdateServiceUtil.getBulkProcess(bulkProductSuspensionRequest, 0, 0)).thenReturn(bulkInternalProcess);
    when(internalProcessService.saveInternalProcess(bulkInternalProcess)).thenReturn(bulkInternalProcess);
    when(productLevel3Repository.doBulkSuspensionProductsActions(anyList(), anyString(), anyString()))
        .thenReturn(Arrays.asList(suspensionProductResponse));
    doNothing().when(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    bulkProductSuspensionServiceBean.process(bulkProductSuspensionRequest);

    verify(systemParameter).getProductSuspensionBulkUploadBatchSize();
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_SUSPENSION_SWITCH);
    verify(bulkUpdateServiceUtil).getBulkProcess(bulkProductSuspensionRequest, 0, 0);
    verify(internalProcessService, times(3)).saveInternalProcess(bulkInternalProcess);
    verify(productLevel3Repository).doBulkSuspensionProductsActions(Mockito.anyList(), Mockito.eq(REQUEST_ID),
        Mockito.eq(bulkProductSuspensionRequest.getUpdatedBy()));
    verify(internalProcessService, times(3)).saveInternalProcess(bulkProcessArgumentCaptor.capture());
    verify(mailDeliveryService).sendEmail(any(BulkDownloadRequest.class), anyMap());
    verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
    verify(fileStorageService).getFilePrefix(Mockito.anyString());
    verify(fileStorageService).getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    Assertions.assertEquals(Integer.valueOf(4), bulkProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(Integer.valueOf(2), bulkProcessArgumentCaptor.getValue().getErrorCount());
    Assertions.assertEquals(Integer.valueOf(6), bulkProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(ProcessStatus.PARTIAL_COMPLETED.name(), bulkProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void processTestForFullyFailedReasonValidation() throws Exception {
    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(fullSuspensionFailedFile);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class)))
      .thenReturn(sheet);
    doNothing().when(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    bulkInternalUploadRequestDTO = new BulkInternalUploadRequestDTO();
    bulkInternalUploadRequestDTO.setBulkInternalProcessType(BulkInternalProcessType.SUSPEND);
    bulkInternalUploadRequestDTO.setRelativePath(productSuspensionSuspensionActionFullFailedFilePath);
    bulkProductSuspensionRequest.setFilePath(productSuspensionSuspensionActionFullFailedFilePath);
    when(systemParameter.getProductSuspensionBulkUploadBatchSize())
        .thenReturn(PRODUCT_SUSPENSION_ASSIGNMENT_BATCH_SIZE);
    when(bulkUpdateServiceUtil.getBulkProcess(bulkProductSuspensionRequest, 0, 0)).thenReturn(bulkInternalProcess);
    when(internalProcessService.saveInternalProcess(bulkInternalProcess)).thenReturn(bulkInternalProcess);
    when(productLevel3Repository.doBulkSuspensionProductsActions(anyList(), anyString(), anyString()))
        .thenReturn(Arrays.asList(suspensionProductResponse));

    bulkProductSuspensionServiceBean.process(bulkProductSuspensionRequest);

    verify(systemParameter).getProductSuspensionBulkUploadBatchSize();
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_SUSPENSION_SWITCH);
    verify(bulkUpdateServiceUtil).getBulkProcess(bulkProductSuspensionRequest, 0, 0);
    verify(internalProcessService, times(3)).saveInternalProcess(bulkInternalProcess);
    verify(internalProcessService, times(3)).saveInternalProcess(bulkProcessArgumentCaptor.capture());
    verify(mailDeliveryService).sendEmail(any(BulkDownloadRequest.class), anyMap());
    verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
    verify(fileStorageService).getFilePrefix(Mockito.anyString());
   verify(fileStorageService).getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    Assertions.assertEquals(Integer.valueOf(0), bulkProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(Integer.valueOf(3), bulkProcessArgumentCaptor.getValue().getErrorCount());
    Assertions.assertEquals(Integer.valueOf(3), bulkProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void processReactivateTest() throws Exception {
    bulkProductSuspensionRequest.setActionType(REACTIVATE);
    Map<String, Object> email = new HashMap<>();
    when(systemParameter.getProductSuspensionBulkUploadBatchSize())
        .thenReturn(PRODUCT_SUSPENSION_ASSIGNMENT_BATCH_SIZE);
    when(bulkUpdateServiceUtil.getBulkProcess(bulkProductSuspensionRequest, 0, 0)).thenReturn(bulkInternalProcess);
    when(internalProcessService.saveInternalProcess(bulkInternalProcess)).thenReturn(bulkInternalProcess);
    when(productLevel3Repository.doBulkSuspensionProductsActions(anyList(), anyString(), anyString()))
        .thenReturn(new ArrayList());
    doNothing().when(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    bulkProductSuspensionServiceBean.process(bulkProductSuspensionRequest);

    verify(systemParameter).getProductSuspensionBulkUploadBatchSize();
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_SUSPENSION_SWITCH);
    verify(bulkUpdateServiceUtil).getBulkProcess(bulkProductSuspensionRequest, 0, 0);
    verify(internalProcessService, times(3)).saveInternalProcess(bulkInternalProcess);
    verify(productLevel3Repository).doBulkSuspensionProductsActions(Mockito.anyList(), Mockito.eq(REQUEST_ID),
        Mockito.eq(bulkProductSuspensionRequest.getUpdatedBy()));
    verify(internalProcessService, times(3)).saveInternalProcess(bulkProcessArgumentCaptor.capture());
    verify(mailDeliveryService).sendEmail(any(BulkDownloadRequest.class), anyMap());
    verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
    verify(fileStorageService).getFilePrefix(Mockito.anyString());
    verify(fileStorageService).getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class));
    Assertions.assertEquals(Integer.valueOf(6), bulkProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(Integer.valueOf(6), bulkProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(ProcessStatus.COMPLETED.name(), bulkProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void processReactivateFailedTest() throws Exception {
    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(partialSuspensionFailedFile);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class)))
      .thenReturn(sheet);
    bulkInternalUploadRequestDTO = new BulkInternalUploadRequestDTO();
    bulkInternalUploadRequestDTO.setRelativePath(productSuspensionSuspensionActionFailedFilePath);
    bulkProductSuspensionRequest.setActionType(REACTIVATE);
    bulkProductSuspensionRequest.setFilePath(productSuspensionSuspensionActionFailedFilePath);
    when(systemParameter.getProductSuspensionBulkUploadBatchSize())
        .thenReturn(PRODUCT_SUSPENSION_ASSIGNMENT_BATCH_SIZE);
    doNothing().when(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    when(bulkUpdateServiceUtil.getBulkProcess(bulkProductSuspensionRequest, 0, 0)).thenReturn(bulkInternalProcess);
    when(internalProcessService.saveInternalProcess(bulkInternalProcess)).thenReturn(bulkInternalProcess);
    when(productLevel3Repository.doBulkSuspensionProductsActions(anyList(), anyString(), anyString()))
        .thenReturn(Arrays.asList(suspensionProductResponse));

    bulkProductSuspensionServiceBean.process(bulkProductSuspensionRequest);

    verify(systemParameter).getProductSuspensionBulkUploadBatchSize();
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_SUSPENSION_SWITCH);
    verify(bulkUpdateServiceUtil).getBulkProcess(bulkProductSuspensionRequest, 0, 0);
    verify(internalProcessService, times(3)).saveInternalProcess(bulkInternalProcess);
    verify(productLevel3Repository).doBulkSuspensionProductsActions(Mockito.anyList(), Mockito.eq(REQUEST_ID),
        Mockito.eq(bulkProductSuspensionRequest.getUpdatedBy()));
    verify(internalProcessService, times(3)).saveInternalProcess(bulkProcessArgumentCaptor.capture());
    verify(mailDeliveryService).sendEmail(any(BulkDownloadRequest.class), anyMap());
    verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
    verify(fileStorageService).getFilePrefix(Mockito.anyString());
    verify(fileStorageService).getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class));
    Assertions.assertEquals(Integer.valueOf(4), bulkProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(Integer.valueOf(2), bulkProcessArgumentCaptor.getValue().getErrorCount());
    Assertions.assertEquals(Integer.valueOf(6), bulkProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(ProcessStatus.PARTIAL_COMPLETED.name(), bulkProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void processActionNullFailedTest() throws Exception {
    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(partialSuspensionFailedFile);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class)))
      .thenReturn(sheet);
    bulkInternalUploadRequestDTO = new BulkInternalUploadRequestDTO();
    bulkInternalUploadRequestDTO.setRelativePath(productSuspensionSuspensionActionFailedFilePath);
    bulkProductSuspensionRequest.setActionType(null);
    bulkInternalProcess.setProcessType(null);
    when(internalProcessService.saveInternalProcess(Mockito.any(BulkInternalProcess.class)))
        .thenReturn(bulkInternalProcess);
    bulkProductSuspensionRequest.setFilePath(productSuspensionSuspensionActionFailedFilePath);
    when(systemParameter.getProductSuspensionBulkUploadBatchSize())
        .thenReturn(PRODUCT_SUSPENSION_ASSIGNMENT_BATCH_SIZE);
    when(bulkUpdateServiceUtil.getBulkProcess(bulkProductSuspensionRequest, 0, 0)).thenReturn(bulkInternalProcess);
    when(internalProcessService.saveInternalProcess(bulkInternalProcess)).thenReturn(bulkInternalProcess);
    when(productLevel3Repository.doBulkSuspensionProductsActions(anyList(), anyString(), anyString()))
        .thenReturn(Arrays.asList(suspensionProductResponse));
    doNothing().when(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());

    bulkProductSuspensionServiceBean.process(bulkProductSuspensionRequest);

    verify(systemParameter).getProductSuspensionBulkUploadBatchSize();
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_SUSPENSION_SWITCH);
    verify(bulkUpdateServiceUtil).getBulkProcess(bulkProductSuspensionRequest, 0, 0);
    verify(internalProcessService, times(3)).saveInternalProcess(bulkInternalProcess);
    verify(productLevel3Repository).doBulkSuspensionProductsActions(Mockito.anyList(), Mockito.eq(REQUEST_ID),
        Mockito.eq(bulkProductSuspensionRequest.getUpdatedBy()));
    verify(internalProcessService, times(3)).saveInternalProcess(bulkProcessArgumentCaptor.capture());
    verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
    verify(fileStorageService).getFilePrefix(Mockito.anyString());
    verify(fileStorageService).getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class));
    Assertions.assertEquals(Integer.valueOf(4), bulkProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(Integer.valueOf(2), bulkProcessArgumentCaptor.getValue().getErrorCount());
    Assertions.assertEquals(Integer.valueOf(6), bulkProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(ProcessStatus.PARTIAL_COMPLETED.name(), bulkProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void processTest_New() throws Exception {

    systemParameterConfig.setValue("true");
    when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_SUSPENSION_SWITCH))
        .thenReturn(systemParameterConfig);
    when(systemParameter.getProductSuspensionBulkUploadBatchSize())
        .thenReturn(PRODUCT_SUSPENSION_ASSIGNMENT_BATCH_SIZE);
    when(bulkUpdateServiceUtil.getBulkProcess(bulkProductSuspensionRequest, 0, 0)).thenReturn(bulkInternalProcess);
    when(internalProcessService.saveInternalProcess(bulkInternalProcess)).thenReturn(bulkInternalProcess);
    when(productLevel3Repository.doBulkSuspensionProductsActions(anyList(), anyString(), anyString()))
        .thenReturn(new ArrayList());

    bulkProductSuspensionServiceBean.process(bulkProductSuspensionRequest);

    verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_SUSPENSION_SWITCH);
    verify(bulkUpdateServiceUtil).getBulkProcess(bulkProductSuspensionRequest, 0, 0);
    verify(internalProcessService, times(2)).saveInternalProcess(bulkInternalProcess);
    verify(internalProcessService, times(2)).saveInternalProcess(bulkProcessArgumentCaptor.capture());
    verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
    verify(internalProcessService).saveInternalProcessData(anyList());
    verify(objectMapper, times(6)).writeValueAsString(any());
    verify(fileStorageService).getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class));
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(bulkUpdateServiceUtil);
    Mockito.verifyNoMoreInteractions(systemParameter);
    Mockito.verifyNoMoreInteractions(productLevel3Repository);
    Mockito.verifyNoMoreInteractions(internalProcessService);
    Mockito.verifyNoMoreInteractions(systemParameterConfigService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(fileStorageService);
    verifyNoMoreInteractions(mailDeliveryService);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void processSuspensionEvent() throws Exception {
    when(internalProcessService.findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkInternalProcess);
    when(internalProcessService
        .findByStoreIdAndBulkProcessCodeAndRowNumberIn(STORE_ID, BULK_PROCESS_CODE, Arrays.asList(PRODUCT_CODE)))
        .thenReturn(new ArrayList<>());
    when(systemParameter.getProductSuspensionBulkUploadBatchSize())
        .thenReturn(PRODUCT_SUSPENSION_ASSIGNMENT_BATCH_SIZE);
    when(bulkUpdateServiceUtil.getBulkProcess(bulkProductSuspensionRequest, 0, 0)).thenReturn(bulkInternalProcess);
    when(internalProcessService.saveInternalProcess(bulkInternalProcess)).thenReturn(bulkInternalProcess);
    when(productLevel3Repository.doBulkSuspensionProductsActions(anyList(), anyString(), anyString()))
        .thenReturn(new ArrayList());

    bulkProductSuspensionServiceBean.processSuspensionEvent(bulkInternalEventModel);

    verify(systemParameter).getProductSuspensionBulkUploadBatchSize();
    verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
    verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
    verify(internalProcessService)
        .findByStoreIdAndBulkProcessCodeAndRowNumberIn(STORE_ID, BULK_PROCESS_CODE, Arrays.asList(PRODUCT_CODE));
    verify(bulkUpdateServiceUtil).getRowDataToProcessSuspension(anyList());
    verify(bulkUpdateServiceUtil).setBlpFinalDataForSuspension(anyList(), anyList());
    verify(internalProcessService, times(2)).saveInternalProcessData(anyList());
  }

  @Test
  public void setFinalStatusAndNotificationOnSuspension() throws Exception {
    bulkInternalProcess.setId("");
    BulkInternalProcessData bulkInternalProcessData =
        BulkInternalProcessData.builder().data("{\"data\":\"data\"}").build();
    BulkInternalProcessData bulkInternalProcessData1 =
        BulkInternalProcessData.builder().data("{\"data\":\"data\"}").errorMessage(ERROR_MESSGAGE).build();
    when(bulkUpdateServiceUtil.getBulkProcess(bulkProductSuspensionRequest, 0, 0)).thenReturn(bulkInternalProcess);
    when(internalProcessService
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(anyString(), anyString()))
        .thenReturn(Arrays.asList(bulkInternalProcessData, bulkInternalProcessData1));
    doNothing().when(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    when(productLevel3Repository.doBulkSuspensionProductsActions(anyList(), anyString(), anyString()))
        .thenReturn(new ArrayList());
    bulkProductSuspensionServiceBean.setFinalStatusAndNotificationOnSuspension(bulkInternalProcess, STORE_ID);
    verify(internalProcessService).saveInternalProcess(bulkInternalProcess);
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(internalProcessService).saveInternalProcess(bulkProcessArgumentCaptor.capture());
    verify(mailDeliveryService).sendEmail(any(BulkDownloadRequest.class), anyMap());
    verify(fileStorageService).getFilePrefix(Mockito.anyString());
    verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(anyString(), anyString());
    Assertions.assertEquals(Integer.valueOf(1), bulkProcessArgumentCaptor.getValue().getSuccessCount());
  }

  @Test
  public void setFinalStatusAndNotificationOnReactivation() throws Exception {
    bulkInternalProcess.setId("");
    bulkInternalProcess.setNotes("REACTIVATE");
    BulkInternalProcessData bulkInternalProcessData =
        BulkInternalProcessData.builder().data("{\"data\":\"data\"}").build();
    BulkInternalProcessData bulkInternalProcessData1 =
        BulkInternalProcessData.builder().data("{\"data\":\"data\"}").errorMessage(ERROR_MESSGAGE).build();
    when(bulkUpdateServiceUtil.getBulkProcess(bulkProductSuspensionRequest, 0, 0)).thenReturn(bulkInternalProcess);
    when(internalProcessService
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(anyString(), anyString()))
        .thenReturn(Arrays.asList(bulkInternalProcessData, bulkInternalProcessData1));
    doNothing().when(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    when(productLevel3Repository.doBulkSuspensionProductsActions(anyList(), anyString(), anyString()))
        .thenReturn(new ArrayList());
    bulkProductSuspensionServiceBean.setFinalStatusAndNotificationOnSuspension(bulkInternalProcess, STORE_ID);
    verify(internalProcessService).saveInternalProcess(bulkInternalProcess);
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(internalProcessService).saveInternalProcess(bulkProcessArgumentCaptor.capture());
    verify(mailDeliveryService).sendEmail(any(BulkDownloadRequest.class), anyMap());
    verify(fileStorageService).getFilePrefix(Mockito.anyString());
    verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(anyString(), anyString());
    Assertions.assertEquals(Integer.valueOf(1), bulkProcessArgumentCaptor.getValue().getSuccessCount());
  }

  @Test
  public void processToPublishForSuspension() {
    BulkInternalProcessData internalProcessData = BulkInternalProcessData.builder().parentCode(PRODUCT_CODE).build();
    SystemParameterConfig systemParameterConfig =
        new SystemParameterConfig(SystemParameterConfigNames.PROCESS_DATA_SUSPENSION_BATCH_SIZE, "2", "desc");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(STORE_ID, BULK_PROCESS_CODE,
            ProcessStatus.PENDING.name())).thenReturn(Arrays.asList(internalProcessData));
    bulkProductSuspensionServiceBean
        .processToPublishForSuspension(STORE_ID, REQUEST_ID, new PageImpl<>(Arrays.asList(bulkInternalProcess)));
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(STORE_ID, BULK_PROCESS_CODE,
            ProcessStatus.PENDING.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(kafkaProducer)
        .send(eq(kafkaTopicProperties.getBulkProductSuspensionEvent()), any(BulkInternalEventModel.class));
    verify(internalProcessService).saveInternalProcessData(anyList());
    verify(kafkaTopicProperties, times(4)).getBulkProductSuspensionEvent();
  }

  @Test
  public void test_processSuspensionForWrongFailedReason() throws Exception {
    List<Map<String, String>> inputRowData = new ArrayList<>();
    LinkedHashMap<String, String> rowDataJson = new LinkedHashMap<>();
    rowDataJson.put("wrong_data", "");
    inputRowData.add(rowDataJson);
      when(internalProcessService.findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE))
        .thenReturn(bulkInternalProcess);
      when(internalProcessService
        .findByStoreIdAndBulkProcessCodeAndRowNumberIn(STORE_ID, BULK_PROCESS_CODE, Arrays.asList(PRODUCT_CODE)))
        .thenReturn(new ArrayList<>());
      when(systemParameter.getProductSuspensionBulkUploadBatchSize())
        .thenReturn(PRODUCT_SUSPENSION_ASSIGNMENT_BATCH_SIZE);
      when(bulkUpdateServiceUtil.getBulkProcess(bulkProductSuspensionRequest, 0, 0)).thenReturn(bulkInternalProcess);
      when(internalProcessService.saveInternalProcess(bulkInternalProcess)).thenReturn(bulkInternalProcess);
      when(productLevel3Repository.doBulkSuspensionProductsActions(anyList(), anyString(), anyString()))
        .thenReturn(new ArrayList());
      bulkProductSuspensionServiceBean.processSuspensionEvent(bulkInternalEventModel);
      when(bulkUpdateServiceUtil.getRowDataToProcessSuspension(anyList())).thenReturn(inputRowData);
      verify(systemParameter).getProductSuspensionBulkUploadBatchSize();
      verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
      verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
      verify(internalProcessService)
        .findByStoreIdAndBulkProcessCodeAndRowNumberIn(STORE_ID, BULK_PROCESS_CODE, Arrays.asList(PRODUCT_CODE));
      verify(bulkUpdateServiceUtil).getRowDataToProcessSuspension(anyList());
      verify(bulkUpdateServiceUtil).setBlpFinalDataForSuspension(anyList(), anyList());
      verify(internalProcessService, times(2)).saveInternalProcessData(anyList());
    }

  @Test
  public void testRemoveFailedDataFromSuccessData() throws Exception {
    List<Map<String, String>> successData = new ArrayList<>();
    List<Map<String, String>> failureData = new ArrayList<>();
    Map<String, String> successItem1 = new HashMap<>();
    successItem1.put(BulkProductSuspensionParameters.PRODUCT_CODE, PRODUCT_CODE);
    successItem1.put(BulkProductSuspensionParameters.SELLER_CODE, SELLER_CODE);
    successItem1.put(FILE_NAME, DEFAULT_BULK_PROCESS_TYPE);
    successData.add(successItem1);
    
    // Add another success item with different product/seller key
    Map<String, String> successItem2 = new HashMap<>();
    successItem2.put(BulkProductSuspensionParameters.PRODUCT_CODE, PRODUCT_CODE.concat(PRODUCT_CODE));
    successItem2.put(BulkProductSuspensionParameters.SELLER_CODE, SELLER_CODE.concat(SELLER_CODE));
    successItem2.put(FILE_NAME.concat(FILE_NAME), DEFAULT_BULK_PROCESS_TYPE.concat(DEFAULT_BULK_PROCESS_TYPE));
    successData.add(successItem2);
    Map<String, String> failureItem = new HashMap<>();
    failureItem.put(BulkProductSuspensionParameters.PRODUCT_CODE,PRODUCT_CODE.concat(PRODUCT_CODE));
    failureItem.put(BulkProductSuspensionParameters.SELLER_CODE, SELLER_CODE.concat(SELLER_CODE));
    failureItem.put(BulkProductSuspensionParameters.FAILURE_REASON, ERROR_MESSGAGE);
    failureData.add(failureItem);
    Method removeFailedDataMethod = BulkProductSuspensionServiceBean.class.getDeclaredMethod(
        "removeFailedDataFromSuccessData", List.class, List.class);
    removeFailedDataMethod.setAccessible(true);
    List<HashMap<String, String>> result = (List<HashMap<String, String>>) removeFailedDataMethod.invoke(
        bulkProductSuspensionServiceBean, successData, failureData);
    Assertions.assertEquals(1, successData.size());
    Assertions.assertEquals(PRODUCT_CODE,
      successData.get(0).get(BulkProductSuspensionParameters.PRODUCT_CODE));
    Assertions.assertEquals(SELLER_CODE,
      successData.get(0).get(BulkProductSuspensionParameters.SELLER_CODE));
    Assertions.assertEquals(1, result.size());
    Assertions.assertTrue(result.get(0).containsKey(BulkProductSuspensionParameters.PRODUCT_CODE));
    Assertions.assertTrue(result.get(0).containsKey(BulkProductSuspensionParameters.SELLER_CODE));
    Assertions.assertTrue(result.get(0).containsKey(BulkProductSuspensionParameters.FAILURE_REASON));
  }
}