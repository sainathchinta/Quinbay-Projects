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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.List;

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
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.config.SystemParameter;
import com.gdn.mta.bulk.SystemParameterConfigNames;
import com.gdn.mta.bulk.dto.BulkInternalUploadRequestDTO;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.product.BulkConfigurationUpdateRequest;
import com.gdn.mta.bulk.entity.BulkInternalEventModel;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.SystemParameterConfig;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.partners.bulk.util.BulkConfigurationUpdateParameters;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.dto.response.BulkCategoryConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.BulkMerchantConfigUploadResponse;

public class BulkConfigurationUpdateServiceBeanTest {

  private static final String BULK_UPDATE_FOLDER = "BulkUpdate";
  private static final String FILE_NAME = "ConfigurationUpdate.xlsx";
  private static final String CORRUPT_DATA_FILE_NAME = "ConfigurationUpdate_corruptDataFile.xlsx";
  private static final String CATEGORY_CORRUPT_DATA_FILE_NAME = "CategoryDataTestFile.xlsx";
  private static final String CONFIG_DATA_VALIDATION_BY_TYPE = "ConfigDataValidationByType.xlsx";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String STORE_ID = "storeId";
  private static final String UPDATED_BY = "updatedBy";
  private static final String REQUEST_ID = "requestId";
  private static final String DEFAULT_BULK_PROCESS_TYPE = "ProductLevel3";
  private static final String ACTION_TYPE_MERCHANT = "Merchant";
  private static final String ACTION_TYPE_CATEGORY = "Category";
  private static final int CONFIGURATION_UPDATE_ASSIGNMENT_BATCH_SIZE = 10;
  private static final String MERCHANT_CODE_1 = "merchantCode1";
  private static final String MERCHANT_NAME_1 = "merchantName1";
  private static final String CATEGORY_CODE_1 = "categoryCode1";
  private static final String CATEGORY_NAME_1 = "categoryName1";
  private static final String MERCHANT_CODE_2 = "merchantCode2";
  private static final String MERCHANT_NAME_2 = "merchantName2";
  private static final String MERCHANT_CODE_3 = "merchantCode3";
  private static final String MERCHANT_NAME_3 = "merchantName3";
  private static final String ERROR_MESSAGE = "ERROR_MESSAGE";
  private static final String SUCCESS = "SUCCESS";
  private static final String lang = "in";
  private static String corruptDataFile;
  private static String categoryDataTestFile;
  private static String dataFailedFile;

  private BulkInternalProcess bulkProcess;
  private BulkInternalEventModel bulkInternalEventModel;
  private BulkConfigurationUpdateRequest bulkConfigurationUpdateRequest;
  private BulkMerchantConfigUploadResponse bulkMerchantConfigUploadResponse1;
  private BulkMerchantConfigUploadResponse bulkMerchantConfigUploadResponse2;
  private BulkMerchantConfigUploadResponse bulkMerchantConfigUploadResponse3;
  private List<BulkMerchantConfigUploadResponse> bulkMerchantConfigUploadResponseList;
  private BulkCategoryConfigUploadResponse bulkCategoryConfigUploadResponse1;
  private List<BulkCategoryConfigUploadResponse> categoryConfigUploadResponseList;

  @Mock
  private MailDeliveryService mailDeliveryService;

  @Mock
  private InternalProcessService internalProcessService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Mock
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Mock
  private SystemParameterConfigService systemParameterConfigService;

  @Mock
  private SystemParameter systemParameter;

  @Mock
  private BulkConfigurationService bulkConfigurationService;

  @InjectMocks
  private BulkConfigurationUpdateServiceBean bulkConfigurationUpdateServiceBean;

  @Captor
  private ArgumentCaptor<BulkInternalProcess> bulkProcessArgumentCaptor;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private FileStorageServiceBean fileStorageServiceBean;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private SystemParameterConfig systemParameterConfig;

  private BulkInternalUploadRequestDTO bulkInternalUploadRequestDTO;

  @BeforeEach
  public void init() throws Exception {
    initMocks(this);
    ClassLoader classLoader = getClass().getClassLoader();
    File file = new File(classLoader.getResource(BULK_UPDATE_FOLDER + File.separator + FILE_NAME).getFile());
    String filePath = file.getAbsolutePath();
    corruptDataFile =
        new File(classLoader.getResource(BULK_UPDATE_FOLDER + File.separator + CORRUPT_DATA_FILE_NAME).getFile())
            .getAbsolutePath();
    categoryDataTestFile = new File(
        classLoader.getResource(BULK_UPDATE_FOLDER + File.separator + CATEGORY_CORRUPT_DATA_FILE_NAME).getFile())
        .getAbsolutePath();

    dataFailedFile = new File(
        classLoader.getResource(BULK_UPDATE_FOLDER + File.separator + CONFIG_DATA_VALIDATION_BY_TYPE).getFile())
        .getAbsolutePath();

    Mockito.when(fileStorageServiceBean.getFilePrefix(Mockito.anyString())).thenReturn(REQUEST_ID);

    bulkProcess = new BulkInternalProcess();
    bulkProcess.setStartTime(Calendar.getInstance().getTime());
    bulkProcess.setInternalProcessRequestCode(BULK_PROCESS_CODE);
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setStatus(BulkProcess.STATUS_PENDING);
    bulkProcess.setSuccessCount(0);
    bulkProcess.setErrorCount(0);
    bulkProcess.setFileName(filePath);
    bulkProcess.setCreatedBy(UPDATED_BY);
    bulkProcess.setNotes(BulkConfigurationUpdateParameters.MERCHANT);

    bulkConfigurationUpdateRequest = new BulkConfigurationUpdateRequest();
    bulkConfigurationUpdateRequest.setRequestId(REQUEST_ID);
    bulkConfigurationUpdateRequest.setStoreId(STORE_ID);
    bulkConfigurationUpdateRequest.setActionType(ACTION_TYPE_MERCHANT);
    bulkConfigurationUpdateRequest.setBulkProcessType(DEFAULT_BULK_PROCESS_TYPE);
    bulkConfigurationUpdateRequest.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkConfigurationUpdateRequest.setFilePath(filePath);
    bulkConfigurationUpdateRequest.setUpdatedBy(UPDATED_BY);

    bulkMerchantConfigUploadResponse1 = new BulkMerchantConfigUploadResponse();
    bulkMerchantConfigUploadResponse1.setBusinessPartnerCode(MERCHANT_CODE_1);
    bulkMerchantConfigUploadResponse1.setBusinessPartnerName(MERCHANT_NAME_1);
    bulkMerchantConfigUploadResponse1.setErrorMessage(SUCCESS);

    bulkMerchantConfigUploadResponse2 = new BulkMerchantConfigUploadResponse();
    bulkMerchantConfigUploadResponse2.setBusinessPartnerCode(MERCHANT_CODE_2);
    bulkMerchantConfigUploadResponse2.setBusinessPartnerName(MERCHANT_NAME_2);
    bulkMerchantConfigUploadResponse2.setErrorMessage(SUCCESS);

    bulkMerchantConfigUploadResponse3 = new BulkMerchantConfigUploadResponse();
    bulkMerchantConfigUploadResponse3.setBusinessPartnerCode(MERCHANT_CODE_3);
    bulkMerchantConfigUploadResponse3.setBusinessPartnerName(MERCHANT_NAME_3);
    bulkMerchantConfigUploadResponse3.setErrorMessage(SUCCESS);

    bulkMerchantConfigUploadResponseList = new ArrayList<>();
    bulkMerchantConfigUploadResponseList.add(bulkMerchantConfigUploadResponse1);
    bulkMerchantConfigUploadResponseList.add(bulkMerchantConfigUploadResponse2);
    bulkMerchantConfigUploadResponseList.add(bulkMerchantConfigUploadResponse3);

    bulkCategoryConfigUploadResponse1 = new BulkCategoryConfigUploadResponse();
    bulkCategoryConfigUploadResponse1.setCategoryCode(CATEGORY_CODE_1);
    bulkCategoryConfigUploadResponse1.setCategoryName(CATEGORY_NAME_1);
    bulkCategoryConfigUploadResponse1.setErrorMessage(SUCCESS);

    categoryConfigUploadResponseList = new ArrayList<>();
    categoryConfigUploadResponseList.add(bulkCategoryConfigUploadResponse1);
    when(internalProcessService.findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(null);
    when(internalProcessService.saveInternalProcess(Mockito.any(BulkInternalProcess.class))).thenReturn(bulkProcess);

    systemParameterConfig = new SystemParameterConfig(Constant.BULK_SUSPENSION_SWITCH, "false", "");
    when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_CONFIG_UPDATE_SWITCH))
        .thenReturn(systemParameterConfig);
    bulkInternalEventModel = BulkInternalEventModel.builder().internalProcessRequestCode(BULK_PROCESS_CODE)
        .processCode(Arrays.asList(ACTION_TYPE_CATEGORY)).requestId(REQUEST_ID).storeId(STORE_ID).build();

    File file1 = new File(
      classLoader.getResource(BULK_UPDATE_FOLDER + File.separator + FILE_NAME).getFile());

    bulkConfigurationUpdateRequest.setFilePath(file1.getAbsolutePath());
    bulkInternalUploadRequestDTO =
      BulkInternalUploadRequestDTO.builder().relativePath(file1.getAbsolutePath())
        .bulkInternalProcessType(BulkInternalProcessType.CONFIGURATION).build();
    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(file1);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class)))
      .thenReturn(sheet);
  }

  @Test
  public void processConfigurationUpdateMerchantTest() throws Exception {
    when(systemParameter.getConfigurationBulkUploadBatchSize()).thenReturn(CONFIGURATION_UPDATE_ASSIGNMENT_BATCH_SIZE);
    when(internalProcessService.saveInternalProcess(bulkProcess)).thenReturn(bulkProcess);
    when(bulkConfigurationService.bulkMerchantConfigUpload(Mockito.anyList(), Mockito.any()))
        .thenReturn(bulkMerchantConfigUploadResponseList);
    bulkConfigurationUpdateServiceBean.processConfigurationUpdate(bulkConfigurationUpdateRequest);
    verify(systemParameter).getConfigurationBulkUploadBatchSize();
    verify(bulkConfigurationService).bulkMerchantConfigUpload(Mockito.anyList(), Mockito.any());
    verify(internalProcessService, times(3)).saveInternalProcess(bulkProcessArgumentCaptor.capture());
    verify(mailDeliveryService).sendEmail(any(BulkDownloadRequest.class), anyMap());
    verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
    verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_CONFIG_UPDATE_SWITCH);
    Assertions.assertEquals(Integer.valueOf(3), bulkProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(Integer.valueOf(3), bulkProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(BulkProcess.STATUS_FINISHED, bulkProcessArgumentCaptor.getValue().getStatus());
    verify(fileStorageServiceBean).getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    verify(fileStorageServiceBean).getFilePrefix(BulkProcessType.CONFIGURATION.getValue());
  }

  @Test
  public void processConfigurationUpdateMerchantTest_AlreadyExist() throws Exception {
    when(internalProcessService.findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    bulkConfigurationUpdateServiceBean.processConfigurationUpdate(bulkConfigurationUpdateRequest);
    verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
  }

  @Test
  public void processConfigurationUpdateCorruptDataTest() throws Exception {
    bulkConfigurationUpdateRequest.setFilePath(corruptDataFile);
    bulkInternalUploadRequestDTO.setRelativePath(corruptDataFile);
    when(systemParameter.getConfigurationBulkUploadBatchSize()).thenReturn(CONFIGURATION_UPDATE_ASSIGNMENT_BATCH_SIZE);
    when(internalProcessService.saveInternalProcess(bulkProcess)).thenReturn(bulkProcess);
    when(bulkConfigurationService.bulkMerchantConfigUpload(Mockito.anyList(), Mockito.any()))
        .thenReturn(bulkMerchantConfigUploadResponseList);
    bulkConfigurationUpdateServiceBean.processConfigurationUpdate(bulkConfigurationUpdateRequest);
    verify(systemParameter).getConfigurationBulkUploadBatchSize();
    verify(internalProcessService, times(3)).saveInternalProcess(bulkProcessArgumentCaptor.capture());
    verify(mailDeliveryService).sendEmail(any(BulkDownloadRequest.class), anyMap());
    verify(bulkConfigurationService).bulkMerchantConfigUpload(Mockito.anyList(), Mockito.any());
    verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
    verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_CONFIG_UPDATE_SWITCH);
    Assertions.assertEquals(Integer.valueOf(3), bulkProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(Integer.valueOf(3), bulkProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcessArgumentCaptor.getValue().getErrorCount());
    Assertions.assertEquals(BulkProcess.STATUS_FINISHED, bulkProcessArgumentCaptor.getValue().getStatus());
    verify(fileStorageServiceBean).getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    verify(fileStorageServiceBean).getFilePrefix(BulkProcessType.CONFIGURATION.getValue());
  }

  @Test
  public void processConfigurationUpdateCategoryTest() throws Exception {
    bulkConfigurationUpdateRequest.setActionType(ACTION_TYPE_CATEGORY);
    bulkConfigurationUpdateRequest.setFilePath(categoryDataTestFile);
    bulkInternalUploadRequestDTO.setRelativePath(categoryDataTestFile);
    when(systemParameter.getConfigurationBulkUploadBatchSize()).thenReturn(CONFIGURATION_UPDATE_ASSIGNMENT_BATCH_SIZE);
    when(internalProcessService.saveInternalProcess(bulkProcess)).thenReturn(bulkProcess);
    when(bulkConfigurationService.bulkCategoryConfigUpload(Mockito.anyList(), Mockito.any()))
        .thenReturn(categoryConfigUploadResponseList);
    doNothing().when(fileStorageServiceBean).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    bulkConfigurationUpdateServiceBean.processConfigurationUpdate(bulkConfigurationUpdateRequest);
    verify(internalProcessService, times(3)).saveInternalProcess(bulkProcessArgumentCaptor.capture());
    verify(mailDeliveryService).sendEmail(any(BulkDownloadRequest.class), anyMap());
    verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
    verify(fileStorageServiceBean).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_CONFIG_UPDATE_SWITCH);
    Assertions.assertEquals(Integer.valueOf(0), bulkProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(Integer.valueOf(3), bulkProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
    verify(fileStorageServiceBean).getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    verify(fileStorageServiceBean).getFilePrefix(BulkProcessType.CONFIGURATION.getValue());
  }

  @Test
  public void processConfigurationUpdateCategoryNoPassedTest() throws Exception {
    bulkCategoryConfigUploadResponse1.setErrorMessage(ERROR_MESSAGE);
    bulkConfigurationUpdateRequest.setActionType(ACTION_TYPE_CATEGORY);
    bulkConfigurationUpdateRequest.setFilePath(categoryDataTestFile);
    bulkInternalUploadRequestDTO.setRelativePath(categoryDataTestFile);
    when(systemParameter.getConfigurationBulkUploadBatchSize()).thenReturn(CONFIGURATION_UPDATE_ASSIGNMENT_BATCH_SIZE);
    when(internalProcessService.saveInternalProcess(bulkProcess)).thenReturn(bulkProcess);
    when(bulkConfigurationService.bulkCategoryConfigUpload(Mockito.anyList(), Mockito.any()))
        .thenReturn(categoryConfigUploadResponseList);
    doNothing().when(fileStorageServiceBean).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    bulkConfigurationUpdateServiceBean.processConfigurationUpdate(bulkConfigurationUpdateRequest);
    verify(fileStorageServiceBean).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(internalProcessService, times(3)).saveInternalProcess(bulkProcessArgumentCaptor.capture());
    verify(mailDeliveryService).sendEmail(any(BulkDownloadRequest.class), anyMap());
    verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
    verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_CONFIG_UPDATE_SWITCH);
    Assertions.assertEquals(Integer.valueOf(0), bulkProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(Integer.valueOf(3), bulkProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
    verify(fileStorageServiceBean).getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    verify(fileStorageServiceBean).getFilePrefix(BulkProcessType.CONFIGURATION.getValue());
  }

  @Test
  public void processConfigurationUpdateNoPassedTest() throws Exception {
    bulkInternalUploadRequestDTO.setRelativePath(dataFailedFile);
    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(dataFailedFile);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    Mockito.when(fileStorageServiceBean.getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class)))
      .thenReturn(sheet);
    bulkConfigurationUpdateRequest.setActionType(ACTION_TYPE_MERCHANT);
    bulkConfigurationUpdateRequest.setFilePath(dataFailedFile);
    doNothing().when(fileStorageServiceBean).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    when(internalProcessService.saveInternalProcess(bulkProcess)).thenReturn(bulkProcess);
    bulkConfigurationUpdateServiceBean.processConfigurationUpdate(bulkConfigurationUpdateRequest);
    verify(internalProcessService, times(3)).saveInternalProcess(bulkProcessArgumentCaptor.capture());
    verify(mailDeliveryService).sendEmail(any(BulkDownloadRequest.class), anyMap());
    verify(fileStorageServiceBean).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
    verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_CONFIG_UPDATE_SWITCH);
    Assertions.assertEquals(Integer.valueOf(0), bulkProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(Integer.valueOf(1), bulkProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
    verify(fileStorageServiceBean).getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    verify(fileStorageServiceBean).getFilePrefix(BulkProcessType.CONFIGURATION.getValue());
  }

  @Test
  public void processTest_New() throws Exception {
    systemParameterConfig.setValue("true");
    when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_CONFIG_UPDATE_SWITCH))
        .thenReturn(systemParameterConfig);
    when(internalProcessService.saveInternalProcess(any())).thenReturn(bulkProcess);
    bulkConfigurationUpdateServiceBean.processConfigurationUpdate(bulkConfigurationUpdateRequest);

    verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_CONFIG_UPDATE_SWITCH);
    verify(internalProcessService, times(2)).saveInternalProcess(any());
    verify(internalProcessService).saveInternalProcessData(anyList());
    verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
    verify(objectMapper, times(3)).writeValueAsString(any());
    verify(fileStorageServiceBean).getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
  }

  @Test
  public void processTest_New_CategoryType() throws Exception {
    systemParameterConfig.setValue("true");
    when(this.systemParameterConfigService
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_CONFIG_UPDATE_SWITCH))
        .thenReturn(systemParameterConfig);
    when(internalProcessService.saveInternalProcess(any())).thenReturn(bulkProcess);
    bulkConfigurationUpdateRequest.setActionType(BulkConfigurationUpdateParameters.CATEGORY);
    bulkConfigurationUpdateServiceBean.processConfigurationUpdate(bulkConfigurationUpdateRequest);

    verify(this.systemParameterConfigService)
        .findValueByStoreIdAndVariable(Constant.STORE_ID, Constant.BULK_CONFIG_UPDATE_SWITCH);
    verify(internalProcessService, times(2)).saveInternalProcess(any());
    verify(internalProcessService).saveInternalProcessData(anyList());
    verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
    verify(objectMapper, times(3)).writeValueAsString(any());
    verify(fileStorageServiceBean).getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);

  }

  @Test
  public void processConfigUpdateEventTest() throws Exception {
    when(internalProcessService.findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    when(internalProcessService.findByStoreIdAndBulkProcessCodeAndRowNumberIn(STORE_ID, BULK_PROCESS_CODE,
        Arrays.asList(ACTION_TYPE_CATEGORY))).thenReturn(new ArrayList<>());
    when(internalProcessService.saveInternalProcess(bulkProcess)).thenReturn(bulkProcess);
    bulkConfigurationUpdateServiceBean.processConfigUpdateEvent(bulkInternalEventModel);
    verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
    verify(internalProcessService).findByInternalProcessRequestCode(STORE_ID, BULK_PROCESS_CODE);
    verify(internalProcessService).findByStoreIdAndBulkProcessCodeAndRowNumberIn(STORE_ID, BULK_PROCESS_CODE,
        Arrays.asList(ACTION_TYPE_CATEGORY));
    verify(bulkUpdateServiceUtil).getRowDataToProcessSuspension(anyList());
    verify(bulkUpdateServiceUtil).setBlpFinalDataForConfigUpdate(anyList(), anyList());
    verify(internalProcessService, times(2)).saveInternalProcessData(anyList());
  }

  @Test
  public void setFinalStatusAndNotificationOnConfigUpdate() throws Exception {
    bulkProcess.setFileName(FILE_NAME);
    bulkProcess.setId("id");
    BulkInternalProcessData bulkInternalProcessData =
        BulkInternalProcessData.builder().data("{\"data\":\"data\"}").build();
    BulkInternalProcessData bulkInternalProcessData1 =
        BulkInternalProcessData.builder().data("{\"data\":\"data\"}").errorMessage("ERROR_MESSGAGE").build();
    when(internalProcessService
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(anyString(), anyString()))
        .thenReturn(Arrays.asList(bulkInternalProcessData, bulkInternalProcessData1));
    doNothing().when(fileStorageServiceBean).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    bulkConfigurationUpdateServiceBean.setFinalStatusAndNotificationOnConfigUpdate(bulkProcess, STORE_ID);
    verify(internalProcessService).saveInternalProcess(bulkProcess);
    verify(fileStorageServiceBean).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    verify(internalProcessService).saveInternalProcess(bulkProcessArgumentCaptor.capture());
    verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(anyString(), anyString());
    verify(mailDeliveryService).sendEmail(any(BulkDownloadRequest.class), anyMap());
    Assertions.assertEquals(Integer.valueOf(1), bulkProcessArgumentCaptor.getValue().getSuccessCount());
    verify(fileStorageServiceBean).getFilePrefix(BulkProcessType.CONFIGURATION.getValue());
  }

  @Test
  public void processToPublishForConfigUpdate() {
    BulkInternalProcessData internalProcessData =
        BulkInternalProcessData.builder().parentCode(BulkConfigurationUpdateParameters.CATEGORY_CODE).build();
    SystemParameterConfig systemParameterConfig =
        new SystemParameterConfig(SystemParameterConfigNames.PROCESS_DATA_CONFIG_UPDATE_BATCH_SIZE, "2", "desc");
    Mockito.when(systemParameterConfigService.findValueByStoreIdAndVariable(Mockito.anyString(), Mockito.anyString()))
        .thenReturn(systemParameterConfig);
    Mockito.when(internalProcessService
        .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(STORE_ID, BULK_PROCESS_CODE,
            ProcessStatus.PENDING.name())).thenReturn(Arrays.asList(internalProcessData));
    bulkConfigurationUpdateServiceBean
        .processToPublishForConfigUpdate(STORE_ID, REQUEST_ID, new PageImpl<>(Arrays.asList(bulkProcess)));
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataByStoreIdAndInternalProcessRequestCodeAndStatus(STORE_ID, BULK_PROCESS_CODE,
            ProcessStatus.PENDING.name());
    Mockito.verify(systemParameterConfigService)
        .findValueByStoreIdAndVariable(Mockito.anyString(), Mockito.anyString());
    Mockito.verify(kafkaProducer)
        .send(eq(kafkaTopicProperties.getBulkConfigurationUpdateRows()),
            any(BulkInternalEventModel.class));
    verify(internalProcessService).saveInternalProcessData(anyList());
    verify(kafkaTopicProperties, times(4)).getBulkConfigurationUpdateRows();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(systemParameter);
    Mockito.verifyNoMoreInteractions(bulkConfigurationService);
    Mockito.verifyNoMoreInteractions(internalProcessService);
    verifyNoMoreInteractions(mailDeliveryService);
    verifyNoMoreInteractions(systemParameterConfigService);
    verifyNoMoreInteractions(kafkaProducer);
    verifyNoMoreInteractions(bulkUpdateServiceUtil);
    verifyNoMoreInteractions(fileStorageServiceBean);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }
}
