package com.gdn.mta.bulk.service;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.List;

import com.gdn.mta.bulk.dto.BulkInternalUploadRequestDTO;
import org.apache.commons.io.FileUtils;
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

import com.gda.mta.product.dto.BulkMasterProductUpdateRequest;
import com.gda.mta.product.dto.BulkMasterProductUpdateResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.config.SystemParameter;
import com.gdn.mta.bulk.dto.MasterDataBulkUpdateRequest;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.repository.ProductRepository;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.dto.request.SimpleMasterProductUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.SimpleMasterProductUpdateResponse;
import org.mockito.Mockito;

import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.FAILED;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.HYPHEN;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.MASTER_PRODUCT_BULK_UPDATE;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.MASTER_PRODUCT_UPDATE;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

public class MasterDataBulkUpdateServiceBeanTest {

  @Mock
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Mock
  private TrackerService trackerService;

  @Mock
  private SystemParameter systemParameter;

  @Mock
  private ProductRepository productRepository;

  @Mock
  private BulkProcessService bulkProcessService;

  @InjectMocks
  private MasterDataBulkUpdateServiceBean masterDataBulkUpdateServiceBean;

  @Captor
  private ArgumentCaptor<BulkInternalProcessData> bulkInternalProcessDataArgumentCaptor;

  @Captor
  private ArgumentCaptor<BulkProcess> bulkProcessArgumentCaptor;

  @Captor
  private ArgumentCaptor<BulkInternalProcess> bulkInternalProcessArgumentCaptor;

  @Captor
  private ArgumentCaptor<BulkMasterProductUpdateRequest> bulkMasterProductUpdateRequestArgumentCaptor;

  @Mock
  private InternalProcessService internalProcessService;

  @Mock
  private FileStorageService fileStorageService;

  private MasterDataBulkUpdateRequest masterDataBulkUpdateRequest;
  private MasterDataBulkUpdateRequest masterDataBulkUpdateSuccessRequest;
  private BulkInternalProcess bulkInternalProcess;
  private BulkInternalProcessData bulkInternalProcessDataSuccess;
  private BulkInternalProcessData bulkInternalProcessDataFailed;
  private BulkInternalUploadRequestDTO bulkInternalUploadRequestDTO;

  private BulkProcess bulkProcess;

  private static final String STORE_ID = "storeId";
  private static final String REQUEST_ID = "requestId";
  private static final String CLIENT_HOST = "clientHost";
  private static final String UPDATED_BY = "updatedBy";
  private static final String EMAIL_TO = "emailTo";
  private static final String EMAIL_CC = "emailCC";
  private static final String BULK_UPDATE_FOLDER = "BulkUpdate";
  private static final String FILE_NAME = "InternalBulkUpdate.xlsx";
  private static final String FILE_NAME_SUCCESS = "InternalBulkUpdateSuccess.xlsx";
  private static final String FILE_NAME_SUCCESS_DUMMY = "InternalBulkUpdateSuccessDummy.xlsx";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final int BULK_UPDATE_BATCH_SIZE = 10;
  private static final String ERROR_MESSAGE = "errorMessage";
  private static final String INTERNAL_BULK_UPLOAD_DATA =
      "{\"productCode\":\"PRODUCT_CODE\",\"productName\":\"NAME\",\"brand\":\"BRAND\",\"length\":\"1.2\","
          + "\"width\":\"3.4\",\"height\":\"5.6\",\"weight\":\"7.8\",\"dangerousGoodsLevel\":\"1\"}";
  private static final Integer TWO_INT = 2;
  private static final Integer ONE_INT = 1;
  private static final Integer ZERO_INT = 0;
  private static final String PRODUCT_SKU = "productSku";
  private String filePathPrefix;
  private File fileSuccess;
  private File file;

  @BeforeEach
  public void setUp() throws Exception{
    initMocks(this);
    ClassLoader classLoader = getClass().getClassLoader();
    file = new File(classLoader
        .getResource(BULK_UPDATE_FOLDER + File.separator + FILE_NAME).getFile());
    String filePath = file.getAbsolutePath();
    masterDataBulkUpdateRequest = new MasterDataBulkUpdateRequest();
    masterDataBulkUpdateRequest.setRequestId(REQUEST_ID);
    masterDataBulkUpdateRequest.setClientHost(CLIENT_HOST);
    masterDataBulkUpdateRequest.setUpdatedBy(UPDATED_BY);
    masterDataBulkUpdateRequest.setFilePath(filePath);
    masterDataBulkUpdateRequest.setStoreId(STORE_ID);
    masterDataBulkUpdateRequest.setEmailTo(EMAIL_TO);
    masterDataBulkUpdateRequest.setEmailCC(EMAIL_CC);
    masterDataBulkUpdateRequest.setBulkProcessCode(BULK_PROCESS_CODE);

    filePathPrefix = classLoader.getResource(BULK_UPDATE_FOLDER).getPath();

    FileUtils.copyFile(new File(filePathPrefix+ File.separator + FILE_NAME_SUCCESS),
        new File( filePathPrefix+ File.separator + FILE_NAME_SUCCESS_DUMMY));
    fileSuccess = new File(filePathPrefix + File.separator + FILE_NAME_SUCCESS_DUMMY);
    String filePathSuccess = fileSuccess.getAbsolutePath();
    masterDataBulkUpdateSuccessRequest = new MasterDataBulkUpdateRequest();
    masterDataBulkUpdateSuccessRequest.setRequestId(REQUEST_ID);
    masterDataBulkUpdateSuccessRequest.setClientHost(CLIENT_HOST);
    masterDataBulkUpdateSuccessRequest.setUpdatedBy(UPDATED_BY);
    masterDataBulkUpdateSuccessRequest.setFilePath(filePathSuccess);
    masterDataBulkUpdateSuccessRequest.setStoreId(STORE_ID);
    masterDataBulkUpdateSuccessRequest.setEmailTo(EMAIL_TO);
    masterDataBulkUpdateSuccessRequest.setEmailCC(EMAIL_CC);
    masterDataBulkUpdateSuccessRequest.setBulkProcessCode(BULK_PROCESS_CODE);

    bulkProcess = new BulkProcess();
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkProcess.setBulkUpdate(true);
    bulkProcess.setStartDate(Calendar.getInstance().getTime());
    bulkProcess.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkProcess.setStoreId(STORE_ID);
    bulkProcess.setStatus(BulkProcess.STATUS_PENDING);
    bulkProcess.setRequestId(REQUEST_ID);
    bulkProcess.setSuccessCount(0);
    bulkProcess.setErrorCount(0);
    bulkProcess.setInputErrorCount(0);
    bulkProcess.setSystemErrorCount(0);

    when(this.bulkProcessService.saveOperation(Mockito.any(BulkProcess.class)))
        .thenReturn(bulkProcess);

    bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setFileName(filePath);
    bulkInternalProcess.setTotalCount(1);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name());

    bulkInternalProcessDataSuccess = new BulkInternalProcessData();
    bulkInternalProcessDataSuccess.setData(INTERNAL_BULK_UPLOAD_DATA);
    bulkInternalProcessDataSuccess.setId(REQUEST_ID);

    bulkInternalProcessDataFailed = new BulkInternalProcessData();
    bulkInternalProcessDataFailed.setData(INTERNAL_BULK_UPLOAD_DATA);
    bulkInternalProcessDataFailed.setId(REQUEST_ID);
    bulkInternalProcessDataFailed.setErrorMessage(ERROR_MESSAGE);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(bulkUpdateServiceUtil);
    verifyNoMoreInteractions(trackerService);
    verifyNoMoreInteractions(systemParameter);
    verifyNoMoreInteractions(productRepository);
    verifyNoMoreInteractions(bulkProcessService);
    verifyNoMoreInteractions(internalProcessService);
    verifyNoMoreInteractions(fileStorageService);
  }

  @Test
  public void processBulkUpdateStatusSuccessTest() throws Exception{
    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(fileSuccess);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    bulkInternalUploadRequestDTO =
      BulkInternalUploadRequestDTO.builder().relativePath(masterDataBulkUpdateSuccessRequest.getFilePath())
        .bulkInternalProcessType(BulkInternalProcessType.INTERNAL_BULK_UPLOAD).build();
    Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class)))
      .thenReturn(sheet);
    when(bulkUpdateServiceUtil.getBulkProcess(eq(masterDataBulkUpdateSuccessRequest), anyInt(), anyInt()))
        .thenReturn(bulkProcess);
    when(bulkUpdateServiceUtil.savePreProcessBulkData(any(BulkProcess.class))).thenReturn(bulkProcess);
    when(systemParameter.getMasterDataBulkUpdateBatchSize()).thenReturn(BULK_UPDATE_BATCH_SIZE);
    SimpleMasterProductUpdateResponse simpleMasterProductUpdateResponse =
        new SimpleMasterProductUpdateResponse();
    simpleMasterProductUpdateResponse.setUpdateSuccess(true);
    BulkMasterProductUpdateResponse bulkMasterProductUpdateResponse =
        new BulkMasterProductUpdateResponse();
    bulkMasterProductUpdateResponse.setSimpleMasterProductUpdateResponses(
        Collections.singletonList(simpleMasterProductUpdateResponse));
    when(productRepository.updateMasterProducts(any(BulkMasterProductUpdateRequest.class)))
        .thenReturn(bulkMasterProductUpdateResponse);
    doNothing().when(trackerService).sendTracker(anyString(), anyString(), anyString(), anyString(), anyString());
    masterDataBulkUpdateServiceBean.processBulkUpdate(masterDataBulkUpdateSuccessRequest);
    verify(bulkUpdateServiceUtil).getBulkProcess(masterDataBulkUpdateSuccessRequest, 0, 0);
    verify(bulkUpdateServiceUtil).savePreProcessBulkData(bulkProcessArgumentCaptor.capture());
    verify(bulkProcessService, times(2)).saveOperation(bulkProcessArgumentCaptor.capture());
    verify(systemParameter).getMasterDataBulkUpdateBatchSize();
    verify(productRepository).updateMasterProducts(bulkMasterProductUpdateRequestArgumentCaptor.capture());
    Assertions.assertEquals(BULK_PROCESS_CODE, bulkProcessArgumentCaptor.getAllValues().get(0).getBulkProcessCode());
    verify(fileStorageService).getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    Assertions.assertEquals(BULK_PROCESS_CODE, bulkProcessArgumentCaptor.getAllValues().get(1).getBulkProcessCode());
    Assertions.assertEquals(3,
        bulkMasterProductUpdateRequestArgumentCaptor.getValue().getSimpleMasterProductUpdateRequests().size());
    SimpleMasterProductUpdateRequest firstSimpleMasterProductUpdateRequest =
        bulkMasterProductUpdateRequestArgumentCaptor.getValue().getSimpleMasterProductUpdateRequests().get(0);
    Assertions.assertTrue(1 == firstSimpleMasterProductUpdateRequest.getDangerousGoodsLevel());

  }

  @Test
  public void processBulkUpdateTest() throws Exception{
    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(file);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    bulkInternalUploadRequestDTO =
      BulkInternalUploadRequestDTO.builder().relativePath(masterDataBulkUpdateRequest.getFilePath())
        .bulkInternalProcessType(BulkInternalProcessType.INTERNAL_BULK_UPLOAD).build();
    Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class)))
      .thenReturn(sheet);
    when(bulkUpdateServiceUtil.getBulkProcess(eq(masterDataBulkUpdateRequest), anyInt(), anyInt()))
        .thenReturn(bulkProcess);
    when(bulkUpdateServiceUtil.savePreProcessBulkData(any(BulkProcess.class))).thenReturn(bulkProcess);
    when(systemParameter.getMasterDataBulkUpdateBatchSize()).thenReturn(BULK_UPDATE_BATCH_SIZE);
    SimpleMasterProductUpdateResponse simpleMasterProductUpdateResponse =
        new SimpleMasterProductUpdateResponse();
    simpleMasterProductUpdateResponse.setUpdateSuccess(true);
    BulkMasterProductUpdateResponse bulkMasterProductUpdateResponse =
        new BulkMasterProductUpdateResponse();
    bulkMasterProductUpdateResponse.setSimpleMasterProductUpdateResponses(
        Collections.singletonList(simpleMasterProductUpdateResponse));
    when(productRepository.updateMasterProducts(any(BulkMasterProductUpdateRequest.class)))
        .thenReturn(bulkMasterProductUpdateResponse);
    doNothing().when(trackerService).sendTracker(anyString(), anyString(), anyString(), anyString(), anyString());
    masterDataBulkUpdateServiceBean.processBulkUpdate(masterDataBulkUpdateRequest);
    verify(bulkUpdateServiceUtil).getBulkProcess(masterDataBulkUpdateRequest, 0, 0);
    verify(bulkUpdateServiceUtil).savePreProcessBulkData(bulkProcessArgumentCaptor.capture());
    verify(bulkProcessService, times(2)).saveOperation(bulkProcessArgumentCaptor.capture());
    verify(systemParameter).getMasterDataBulkUpdateBatchSize();
    verify(productRepository).updateMasterProducts(bulkMasterProductUpdateRequestArgumentCaptor.capture());
    verify(trackerService, times(7))
        .sendTracker(MASTER_PRODUCT_BULK_UPDATE, MASTER_PRODUCT_UPDATE, HYPHEN,
            FAILED, masterDataBulkUpdateRequest.getUpdatedBy());
    verify(fileStorageService).getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
    Assertions.assertEquals(BULK_PROCESS_CODE, bulkProcessArgumentCaptor.getAllValues().get(0).getBulkProcessCode());
    Assertions.assertEquals(BULK_PROCESS_CODE, bulkProcessArgumentCaptor.getAllValues().get(1).getBulkProcessCode());
    Assertions.assertEquals(3,
        bulkMasterProductUpdateRequestArgumentCaptor.getValue().getSimpleMasterProductUpdateRequests().size());
    SimpleMasterProductUpdateRequest firstSimpleMasterProductUpdateRequest =
        bulkMasterProductUpdateRequestArgumentCaptor.getValue().getSimpleMasterProductUpdateRequests().get(0);
    Assertions.assertTrue(1 == firstSimpleMasterProductUpdateRequest.getDangerousGoodsLevel());

  }

  @Test
  public void processBulkUpdate_FailedToSaveProductsTest() throws Exception{
    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(file);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    bulkInternalUploadRequestDTO =
      BulkInternalUploadRequestDTO.builder().relativePath(masterDataBulkUpdateRequest.getFilePath())
        .bulkInternalProcessType(BulkInternalProcessType.INTERNAL_BULK_UPLOAD).build();
    Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class)))
      .thenReturn(sheet);
    when(bulkUpdateServiceUtil.getBulkProcess(eq(masterDataBulkUpdateRequest), anyInt(), anyInt()))
        .thenReturn(bulkProcess);
    when(bulkUpdateServiceUtil.savePreProcessBulkData(any(BulkProcess.class))).thenReturn(bulkProcess);
    when(systemParameter.getMasterDataBulkUpdateBatchSize()).thenReturn(BULK_UPDATE_BATCH_SIZE);
    SimpleMasterProductUpdateResponse simpleMasterProductUpdateResponse =
        new SimpleMasterProductUpdateResponse();
    simpleMasterProductUpdateResponse.setUpdateSuccess(false);
    BulkMasterProductUpdateResponse bulkMasterProductUpdateResponse =
        new BulkMasterProductUpdateResponse();
    bulkMasterProductUpdateResponse.setSimpleMasterProductUpdateResponses(
        Collections.singletonList(simpleMasterProductUpdateResponse));
    when(productRepository.updateMasterProducts(any(BulkMasterProductUpdateRequest.class)))
        .thenReturn(bulkMasterProductUpdateResponse);
    doNothing().when(trackerService).sendTracker(anyString(), anyString(), anyString(), anyString(), anyString());
    masterDataBulkUpdateServiceBean.processBulkUpdate(masterDataBulkUpdateRequest);
    verify(bulkUpdateServiceUtil).getBulkProcess(masterDataBulkUpdateRequest, 0, 0);
    verify(bulkUpdateServiceUtil).savePreProcessBulkData(bulkProcessArgumentCaptor.capture());
    verify(bulkProcessService, times(2)).saveOperation(bulkProcessArgumentCaptor.capture());
    verify(systemParameter).getMasterDataBulkUpdateBatchSize();
    verify(productRepository).updateMasterProducts(bulkMasterProductUpdateRequestArgumentCaptor.capture());
    verify(trackerService, times(7))
        .sendTracker(MASTER_PRODUCT_BULK_UPDATE, MASTER_PRODUCT_UPDATE, HYPHEN,
            FAILED, masterDataBulkUpdateRequest.getUpdatedBy());
    verify(fileStorageService).getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
  }

  @Test
  public void processBulkUpdate_ExceptionOnSaveProductsTest() throws Exception{
    XSSFWorkbook workBook;
    InputStream is = new FileInputStream(file);
    workBook = new XSSFWorkbook(is);
    XSSFSheet sheet = workBook.getSheetAt(0);
    bulkInternalUploadRequestDTO =
      BulkInternalUploadRequestDTO.builder().relativePath(masterDataBulkUpdateRequest.getFilePath())
        .bulkInternalProcessType(BulkInternalProcessType.INTERNAL_BULK_UPLOAD).build();
    Mockito.when(fileStorageService.getFileDataWithInternalUploadRequest(any(BulkInternalUploadRequestDTO.class)))
      .thenReturn(sheet);
    when(bulkUpdateServiceUtil.getBulkProcess(eq(masterDataBulkUpdateRequest), anyInt(), anyInt()))
        .thenReturn(bulkProcess);
    when(bulkUpdateServiceUtil.savePreProcessBulkData(any(BulkProcess.class))).thenReturn(bulkProcess);
    when(systemParameter.getMasterDataBulkUpdateBatchSize()).thenReturn(BULK_UPDATE_BATCH_SIZE);
    when(productRepository.updateMasterProducts(any(BulkMasterProductUpdateRequest.class)))
        .thenThrow(new ApplicationException(ErrorCategory.UNSPECIFIED, ERROR_MESSAGE));
    doNothing().when(trackerService).sendTracker(anyString(), anyString(), anyString(), anyString(), anyString());
    masterDataBulkUpdateServiceBean.processBulkUpdate(masterDataBulkUpdateRequest);
    verify(bulkUpdateServiceUtil).getBulkProcess(masterDataBulkUpdateRequest, 0, 0);
    verify(bulkUpdateServiceUtil).savePreProcessBulkData(bulkProcessArgumentCaptor.capture());
    verify(bulkProcessService, times(2)).saveOperation(bulkProcessArgumentCaptor.capture());
    verify(systemParameter).getMasterDataBulkUpdateBatchSize();
    verify(productRepository).updateMasterProducts(bulkMasterProductUpdateRequestArgumentCaptor.capture());
    verify(trackerService, times(10))
        .sendTracker(MASTER_PRODUCT_BULK_UPDATE, MASTER_PRODUCT_UPDATE, HYPHEN,
            FAILED, masterDataBulkUpdateRequest.getUpdatedBy());
    verify(fileStorageService).getFileDataWithInternalUploadRequest(bulkInternalUploadRequestDTO);
  }

  @Test
  public void processInternalBulkUploadEventTest() throws Exception {
    BulkInternalProcessData bulkInternalProcessData1 = new BulkInternalProcessData();
    bulkInternalProcessData1.setParentCode(PRODUCT_SKU);
    bulkInternalProcessData1.setData(INTERNAL_BULK_UPLOAD_DATA);
    Mockito.when(internalProcessService.getBulkInternalProcessDataById(STORE_ID, REQUEST_ID))
        .thenReturn(bulkInternalProcessData1);
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData1);
    Mockito.when(internalProcessService.saveInternalProcessData(bulkInternalProcessDataList))
        .thenReturn(bulkInternalProcessDataList);
    masterDataBulkUpdateServiceBean
        .processInternalBulkUploadEvent(STORE_ID, UPDATED_BY, BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name(),
            REQUEST_ID);
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataById(STORE_ID, REQUEST_ID);
    Mockito.verify(productRepository).updateMasterProducts(any(BulkMasterProductUpdateRequest.class));
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(bulkInternalProcessDataArgumentCaptor.capture());
    Assertions.assertEquals(ProcessStatus.COMPLETED.name(), bulkInternalProcessDataArgumentCaptor.getValue().getStatus());
    Mockito.verify(internalProcessService).saveInternalProcessData(bulkInternalProcessDataList);
  }

  @Test
  public void processInternalBulkUploadEventExceptionTest() throws Exception {
    BulkInternalProcessData bulkInternalProcessData1 = new BulkInternalProcessData();
    bulkInternalProcessData1.setParentCode(PRODUCT_SKU);
    bulkInternalProcessData1.setData(INTERNAL_BULK_UPLOAD_DATA);
    Mockito.when(internalProcessService.getBulkInternalProcessDataById(STORE_ID, REQUEST_ID))
        .thenReturn(bulkInternalProcessData1);
    Mockito.doThrow(ApplicationRuntimeException.class).when(productRepository)
        .updateMasterProducts(any(BulkMasterProductUpdateRequest.class));
    List<BulkInternalProcessData> bulkInternalProcessDataList = new ArrayList<>();
    bulkInternalProcessDataList.add(bulkInternalProcessData1);
    Mockito.when(internalProcessService.saveInternalProcessData(bulkInternalProcessDataList))
        .thenReturn(bulkInternalProcessDataList);
    masterDataBulkUpdateServiceBean
        .processInternalBulkUploadEvent(STORE_ID, UPDATED_BY, BulkInternalProcessType.INTERNAL_BULK_UPLOAD.name(),
            REQUEST_ID);
    Mockito.verify(internalProcessService)
        .getBulkInternalProcessDataById(STORE_ID, REQUEST_ID);
    Mockito.verify(productRepository)
        .updateMasterProducts(any(BulkMasterProductUpdateRequest.class));
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(bulkInternalProcessDataArgumentCaptor.capture());
    Mockito.verify(trackerService).sendTracker(MASTER_PRODUCT_BULK_UPDATE, MASTER_PRODUCT_UPDATE, HYPHEN, FAILED, UPDATED_BY);
    Mockito.verify(internalProcessService).saveInternalProcessData(bulkInternalProcessDataList);
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(Constant.SYSTEM_ERROR, bulkInternalProcessDataArgumentCaptor.getValue().getErrorMessage());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelTest() throws Exception {
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        STORE_ID, bulkInternalProcess.getId())).thenReturn(Arrays.asList(bulkInternalProcessDataSuccess));
    masterDataBulkUpdateServiceBean.setFinalStatusAndGenerateFailedExcel(bulkInternalProcess, STORE_ID);
    verify(internalProcessService).getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        STORE_ID, bulkInternalProcess.getId());
    verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Assertions.assertEquals(ONE_INT, bulkInternalProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(ONE_INT, bulkInternalProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(ZERO_INT, bulkInternalProcessArgumentCaptor.getValue().getErrorCount());
  }

  @Test
  public void setFinalStatusAndGeneratePendingTest() throws Exception {
    bulkInternalProcess.setTotalCount(TWO_INT);
    bulkInternalProcessDataSuccess.setStatus(ProcessStatus.PENDING.name());
    doNothing().when(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        STORE_ID, bulkInternalProcess.getId()))
      .thenReturn(Arrays.asList(bulkInternalProcessDataSuccess, bulkInternalProcessDataFailed));
    masterDataBulkUpdateServiceBean.setFinalStatusAndGenerateFailedExcel(bulkInternalProcess, STORE_ID);
    verify(internalProcessService).getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
      STORE_ID, bulkInternalProcess.getId());
  }

  @Test
  public void setFinalStatusAndGenerateinProgressTest() throws Exception {
    bulkInternalProcess.setTotalCount(TWO_INT);
    bulkInternalProcessDataSuccess.setStatus(ProcessStatus.IN_PROGRESS.name());
    doNothing().when(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        STORE_ID, bulkInternalProcess.getId()))
      .thenReturn(Arrays.asList(bulkInternalProcessDataSuccess, bulkInternalProcessDataFailed));
    masterDataBulkUpdateServiceBean.setFinalStatusAndGenerateFailedExcel(bulkInternalProcess, STORE_ID);
    verify(internalProcessService).getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
      STORE_ID, bulkInternalProcess.getId());
  }

  @Test
  public void setFinalStatusAndGenerateinEmptyTest() throws Exception {
    bulkInternalProcess.setTotalCount(TWO_INT);
    bulkInternalProcessDataSuccess.setStatus(ProcessStatus.IN_PROGRESS.name());
    doNothing().when(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        STORE_ID, bulkInternalProcess.getId()))
      .thenReturn(Collections.emptyList());
    masterDataBulkUpdateServiceBean.setFinalStatusAndGenerateFailedExcel(bulkInternalProcess, STORE_ID);
    verify(internalProcessService).getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
      STORE_ID, bulkInternalProcess.getId());
  }


  @Test
  public void setFinalStatusAndGenerateFailedExcelFailedTest() throws Exception {
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        STORE_ID, bulkInternalProcess.getId())).thenReturn(Arrays.asList(bulkInternalProcessDataFailed));
    doNothing().when(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    masterDataBulkUpdateServiceBean.setFinalStatusAndGenerateFailedExcel(bulkInternalProcess, STORE_ID);
    verify(internalProcessService).getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        STORE_ID, bulkInternalProcess.getId());
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.any(), Mockito.anyString());
    verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Assertions.assertEquals(ONE_INT, bulkInternalProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(ZERO_INT, bulkInternalProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(ONE_INT, bulkInternalProcessArgumentCaptor.getValue().getErrorCount());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelPartialCompTest() throws Exception {
    bulkInternalProcess.setTotalCount(TWO_INT);
    doNothing().when(fileStorageService).createBulkFile(Mockito.any(), Mockito.anyString(), Mockito.anyString());
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        STORE_ID, bulkInternalProcess.getId()))
        .thenReturn(Arrays.asList(bulkInternalProcessDataSuccess, bulkInternalProcessDataFailed));
    masterDataBulkUpdateServiceBean.setFinalStatusAndGenerateFailedExcel(bulkInternalProcess, STORE_ID);
    verify(internalProcessService).getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        STORE_ID, bulkInternalProcess.getId());
    verify(fileStorageService).createBulkFile(Mockito.any(), Mockito.any(), Mockito.anyString());
    verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Assertions.assertEquals(TWO_INT, bulkInternalProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(ONE_INT, bulkInternalProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(ONE_INT, bulkInternalProcessArgumentCaptor.getValue().getErrorCount());
  }

}