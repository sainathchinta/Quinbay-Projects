package com.gdn.mta.bulk.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.config.SystemParameter;
import com.gdn.mta.bulk.dto.BulkVendorProductAssignRequest;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.models.VendorBulkAssignmentRequest;
import com.gdn.mta.bulk.models.download.VendorAutoAssignmentRequest;
import com.gdn.mta.bulk.repository.download.ProductDistributionTaskRepository;
import com.gdn.mta.bulk.util.BulkUpdateServiceUtil;
import com.gdn.mta.bulk.util.ProcessorUtils;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.bulk.util.VendorProductDataBulkParameters;
import com.gdn.x.mta.distributiontask.rest.model.request.BulkVendorProductActionsRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.BulkVendorProductActionsResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.VendorProductActionsResponse;

public class VendorProductBulkAssignServiceBeanTest {

  private static final String BULK_UPDATE_FOLDER = "BulkUpdate";
  private static final String FILE_NAME = "VendorBulkAssignment.xlsx";
  private static final String FILE_NAME_INVALID_PRODUCT_CODE = "VendorBulkAssignmentInvalidProductCode.xlsx";
  private static final String FILE_NAME_SUCCESS = "VendorBulkAssignmentSuccess.xlsx";
  private static final String DUMMY_FILE_NAME_SUCCESS = "VendorBulkAssignmentDummySuccess.xlsx";
  private static final String EXCEPTION_FILE_NAME = "VendorBulkAssignmentException.xlsx";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String ID = "id";
  private static final String CONTENT_ASSIGN = "content";
  private static final String IMAGE_ASSIGN = "image";
  private static final String SYSTEM = "System";
  private static final String REVIEWERS = "REVIEWERS";
  private static final String STORE_ID = "storeId";
  private static final String UPDATED_BY = "updatedBy";
  private static final String DEFAULT_BULK_PROCESS_TYPE = "ProductLevel3";
  private static final String REQUEST_ID = "requestId";
  private static final int VENDOR_BULK_ASSIGNMENT_BATCH_SIZE = 10;
  private static final String ACTION_TYPE = "assign";
  private static final String ASSIGNED_TO = "assignedTo";
  private static final String ASSIGNED_BY = "assignedBy";
  private static final String PRODUCT_CODE = "code";

  private BulkVendorProductAssignRequest bulkVendorProductAssignRequest;
  private BulkVendorProductAssignRequest bulkVendorProductAssignRequestInvalidProductCode;
  private BulkVendorProductAssignRequest bulkVendorProductAssignRequestSuccess;
  private Map<String, List<String>> validUserList;
  private BulkProcess bulkProcess;
  private BulkVendorProductActionsRequest bulkVendorProductActionsRequest;
  private BulkVendorProductActionsResponse bulkVendorProductActionsResponse;
  private VendorProductActionsResponse vendorProductActionsResponse;
  private List<String> contentSuccessProductList = new ArrayList<>();
  private List<String> imageSuccessProductList = new ArrayList<>();
  private BulkInternalProcess bulkInternalProcess;
  private List<BulkInternalProcessData> bulkInternalProcessDataList;
  private File exceptionFile;
  private ObjectMapper mapper = new ObjectMapper();
  VendorBulkAssignmentRequest vendorBulkAssignmentRequest ;
  private BulkInternalProcessData bulkInternalProcessData1 = new BulkInternalProcessData();


  @InjectMocks
  private VendorProductBulkAssignServiceBean vendorProductBulkAssignServiceBean;

  @Mock
  private BulkUpdateServiceUtil bulkUpdateServiceUtil;

  @Mock
  private SystemParameter systemParameter;

  @Mock
  private ProductDistributionTaskRepository productDistributionTaskRepository;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private InternalProcessService internalProcessService;

  @Mock
  private NotificationService notificationService;

  @Captor
  private ArgumentCaptor<BulkVendorProductActionsRequest> bulkVendorProductActionsRequestArgumentCaptor;

  @Captor
  private ArgumentCaptor<BulkProcess> bulkProcessArgumentCaptor;

  @Captor
  private ArgumentCaptor<BulkInternalProcess> bulkInternalProcessArgumentCaptor;

  @Captor
  private ArgumentCaptor<BulkInternalProcessData> bulkInternalProcessDataArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception{
    initMocks(this);
    ClassLoader classLoader = getClass().getClassLoader();
    File file = new File(classLoader
        .getResource(BULK_UPDATE_FOLDER + File.separator + FILE_NAME).getFile());
    exceptionFile
        = new File(classLoader
        .getResource(BULK_UPDATE_FOLDER + File.separator + EXCEPTION_FILE_NAME).getFile());
    String filePath = file.getAbsolutePath();
    validUserList = new HashMap<>();
    validUserList.put(REVIEWERS, Collections.singletonList(SYSTEM));
    bulkVendorProductAssignRequest = new BulkVendorProductAssignRequest();
    bulkVendorProductAssignRequest.setAssignmentType(CONTENT_ASSIGN);
    bulkVendorProductAssignRequest.setFilePath(filePath);
    bulkVendorProductAssignRequest.setValidUserRoleList(validUserList);
    bulkVendorProductAssignRequest.setStoreId(STORE_ID);
    bulkVendorProductAssignRequest.setUpdatedBy(UPDATED_BY);
    bulkVendorProductAssignRequest.setBulkProcessType(DEFAULT_BULK_PROCESS_TYPE);
    bulkVendorProductAssignRequest.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkVendorProductAssignRequest.setRequestId(REQUEST_ID);

    File fileInvalidProductCode = new File(classLoader
        .getResource(BULK_UPDATE_FOLDER + File.separator + FILE_NAME_INVALID_PRODUCT_CODE).getFile());
    exceptionFile
        = new File(classLoader
        .getResource(BULK_UPDATE_FOLDER + File.separator + EXCEPTION_FILE_NAME).getFile());
    String filePathInvalidProductCode = fileInvalidProductCode.getAbsolutePath();
    validUserList = new HashMap<>();
    validUserList.put(REVIEWERS, Collections.singletonList(SYSTEM));
    bulkVendorProductAssignRequestInvalidProductCode = new BulkVendorProductAssignRequest();
    bulkVendorProductAssignRequestInvalidProductCode.setAssignmentType(CONTENT_ASSIGN);
    bulkVendorProductAssignRequestInvalidProductCode.setFilePath(filePathInvalidProductCode);
    bulkVendorProductAssignRequestInvalidProductCode.setValidUserRoleList(validUserList);
    bulkVendorProductAssignRequestInvalidProductCode.setStoreId(STORE_ID);
    bulkVendorProductAssignRequestInvalidProductCode.setUpdatedBy(UPDATED_BY);
    bulkVendorProductAssignRequestInvalidProductCode.setBulkProcessType(DEFAULT_BULK_PROCESS_TYPE);
    bulkVendorProductAssignRequestInvalidProductCode.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkVendorProductAssignRequestInvalidProductCode.setRequestId(REQUEST_ID);

    File fileSuccess = new File(classLoader
        .getResource(BULK_UPDATE_FOLDER).getFile());
    String FOLDER_PATH = fileSuccess.getAbsolutePath();
    FileUtils.copyFile(new File( FOLDER_PATH + File.separator + FILE_NAME_SUCCESS),
        new File(FOLDER_PATH + File.separator + DUMMY_FILE_NAME_SUCCESS ));
    String filePathSuccess = FOLDER_PATH + File.separator + DUMMY_FILE_NAME_SUCCESS;
    bulkVendorProductAssignRequestSuccess = new BulkVendorProductAssignRequest();
    bulkVendorProductAssignRequestSuccess.setAssignmentType(CONTENT_ASSIGN);
    bulkVendorProductAssignRequestSuccess.setFilePath(filePathSuccess);
    bulkVendorProductAssignRequestSuccess.setValidUserRoleList(validUserList);
    bulkVendorProductAssignRequestSuccess.setStoreId(STORE_ID);
    bulkVendorProductAssignRequestSuccess.setUpdatedBy(UPDATED_BY);
    bulkVendorProductAssignRequestSuccess.setBulkProcessCode(DEFAULT_BULK_PROCESS_TYPE);
    bulkVendorProductAssignRequestSuccess.setBulkProcessType(BULK_PROCESS_CODE);
    bulkVendorProductAssignRequestSuccess.setRequestId(REQUEST_ID);

    bulkProcess = new BulkProcess();
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

    bulkVendorProductActionsRequest = new BulkVendorProductActionsRequest();
    bulkVendorProductActionsRequest.setRequestId(REQUEST_ID);
    bulkVendorProductActionsRequest.setStoreId(STORE_ID);
    bulkVendorProductActionsRequest.setActionType(ACTION_TYPE);
    bulkVendorProductActionsRequest.setAssignmentType(CONTENT_ASSIGN);

    bulkVendorProductActionsResponse = new BulkVendorProductActionsResponse();
    vendorProductActionsResponse = new VendorProductActionsResponse();

    contentSuccessProductList.addAll(Arrays.asList("MTA-0550829","MTA-0528834","MTA-0520153","MTA-0520157"));
    imageSuccessProductList.addAll(Arrays.asList("MTA-0527310", "MTA-0527330"));
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(null);
    when(bulkProcessService.saveOperation(bulkProcess)).thenReturn(bulkProcess);

    bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setId(ID);
    bulkInternalProcess.setInternalProcessRequestCode(BULK_PROCESS_CODE);
    bulkInternalProcess.setFileName(filePath);

    bulkInternalProcessDataList = new ArrayList<>();
    BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
    bulkInternalProcessData.setErrorMessage(BULK_PROCESS_CODE);
    vendorBulkAssignmentRequest = new VendorBulkAssignmentRequest();
    vendorBulkAssignmentRequest.setAssignmentType(VendorProductDataBulkParameters.CONTENT_ASSIGNMENT);
    vendorBulkAssignmentRequest.setProductCode(PRODUCT_CODE);
    vendorBulkAssignmentRequest.setAssignedTo(ASSIGNED_TO);
    vendorBulkAssignmentRequest.setAssignedBy(ASSIGNED_BY);
    vendorBulkAssignmentRequest.setAssignmentType(VendorProductDataBulkParameters.CONTENT_ASSIGNMENT);
    bulkInternalProcessData.setData(mapper.writeValueAsString(vendorBulkAssignmentRequest));

    BulkInternalProcessData bulkInternalProcessData1 = new BulkInternalProcessData();
    bulkInternalProcessData1.setData(mapper.writeValueAsString(vendorBulkAssignmentRequest));
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    bulkInternalProcessDataList.add(bulkInternalProcessData1);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name());
  }

  @Test
  public void processBulkUpdateContentInvalidProductCodeTest() throws Exception {
    when(systemParameter.getVendorProductAssigneeBulkUpdateBatchSize()).thenReturn(VENDOR_BULK_ASSIGNMENT_BATCH_SIZE);
    when(bulkUpdateServiceUtil.getBulkProcess(bulkVendorProductAssignRequestInvalidProductCode, 0, 0)).thenReturn(bulkProcess);
    when(bulkUpdateServiceUtil.savePreProcessBulkData(bulkProcess)).thenReturn(bulkProcess);
    when(productDistributionTaskRepository
        .bulkVendorProductActions(any(BulkVendorProductActionsRequest.class)))
        .thenReturn(bulkVendorProductActionsResponse);
    vendorProductBulkAssignServiceBean.processBulkUpdate(bulkVendorProductAssignRequestInvalidProductCode);
    verify(systemParameter).getVendorProductAssigneeBulkUpdateBatchSize();
    verify(bulkUpdateServiceUtil).getBulkProcess(bulkVendorProductAssignRequestInvalidProductCode, 0, 0);
    verify(bulkUpdateServiceUtil).savePreProcessBulkData(bulkProcess);
    verify(productDistributionTaskRepository)
        .bulkVendorProductActions(bulkVendorProductActionsRequestArgumentCaptor.capture());
    verify(bulkProcessService, times(2)).saveOperation(bulkProcessArgumentCaptor.capture());
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(notificationService).sendBulkAssignNotification(
        bulkProcess, bulkProcess.getSuccessCount(), bulkProcess.getTotalCount(),
      bulkVendorProductAssignRequestInvalidProductCode.getVendorCode());
    Assertions.assertEquals(contentSuccessProductList,
        bulkVendorProductActionsRequestArgumentCaptor.getAllValues().get(0).getBulkScreeningProductActionsRequests()
            .get(0).getProductCodes());
    Assertions.assertEquals(CONTENT_ASSIGN, bulkVendorProductActionsRequestArgumentCaptor.getValue().getAssignmentType());
    Assertions.assertEquals(UPDATED_BY, bulkVendorProductActionsRequestArgumentCaptor.getValue().getUserName());
    Assertions.assertEquals(STORE_ID, bulkVendorProductActionsRequestArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(ACTION_TYPE, bulkVendorProductActionsRequestArgumentCaptor.getValue().getActionType());
    Assertions.assertTrue(bulkProcessArgumentCaptor.getValue().getBulkUpdate());
    Assertions.assertEquals(Integer.valueOf(3), bulkProcessArgumentCaptor.getValue().getErrorCount());
    Assertions.assertEquals(Integer.valueOf(4), bulkProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(Integer.valueOf(7), bulkProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(Integer.valueOf(3), bulkProcessArgumentCaptor.getValue().getInputErrorCount());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcessArgumentCaptor.getValue().getSystemErrorCount());
    Assertions.assertEquals(BulkProcess.STATUS_PARTIALLY_DONE, bulkProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void processBulkUpdateContentTest() throws Exception {
    when(systemParameter.getVendorProductAssigneeBulkUpdateBatchSize()).thenReturn(VENDOR_BULK_ASSIGNMENT_BATCH_SIZE);
    when(bulkUpdateServiceUtil.getBulkProcess(bulkVendorProductAssignRequest, 0, 0)).thenReturn(bulkProcess);
    when(bulkUpdateServiceUtil.savePreProcessBulkData(bulkProcess)).thenReturn(bulkProcess);
    when(productDistributionTaskRepository
        .bulkVendorProductActions(any(BulkVendorProductActionsRequest.class)))
        .thenReturn(bulkVendorProductActionsResponse);
    vendorProductBulkAssignServiceBean.processBulkUpdate(bulkVendorProductAssignRequest);
    verify(systemParameter).getVendorProductAssigneeBulkUpdateBatchSize();
    verify(bulkUpdateServiceUtil).getBulkProcess(bulkVendorProductAssignRequest, 0, 0);
    verify(bulkUpdateServiceUtil).savePreProcessBulkData(bulkProcess);
    verify(productDistributionTaskRepository)
        .bulkVendorProductActions(bulkVendorProductActionsRequestArgumentCaptor.capture());
    verify(bulkProcessService, times(2)).saveOperation(bulkProcessArgumentCaptor.capture());
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(notificationService).sendBulkAssignNotification(
        bulkProcess, bulkProcess.getSuccessCount(), bulkProcess.getTotalCount(),
      bulkVendorProductAssignRequest.getVendorCode());
    Assertions.assertEquals(contentSuccessProductList,
        bulkVendorProductActionsRequestArgumentCaptor.getAllValues().get(0).getBulkScreeningProductActionsRequests()
            .get(0).getProductCodes());
    Assertions.assertEquals(CONTENT_ASSIGN, bulkVendorProductActionsRequestArgumentCaptor.getValue().getAssignmentType());
    Assertions.assertEquals(UPDATED_BY, bulkVendorProductActionsRequestArgumentCaptor.getValue().getUserName());
    Assertions.assertEquals(STORE_ID, bulkVendorProductActionsRequestArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(ACTION_TYPE, bulkVendorProductActionsRequestArgumentCaptor.getValue().getActionType());
    Assertions.assertTrue(bulkProcessArgumentCaptor.getValue().getBulkUpdate());
    Assertions.assertEquals(Integer.valueOf(2), bulkProcessArgumentCaptor.getValue().getErrorCount());
    Assertions.assertEquals(Integer.valueOf(4), bulkProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(Integer.valueOf(6), bulkProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(Integer.valueOf(2), bulkProcessArgumentCaptor.getValue().getInputErrorCount());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcessArgumentCaptor.getValue().getSystemErrorCount());
    Assertions.assertEquals(BulkProcess.STATUS_PARTIALLY_DONE, bulkProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void processBulkUpdate_ALreadyExist() throws Exception {
    when(bulkProcessService.findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE)).thenReturn(bulkProcess);
    vendorProductBulkAssignServiceBean.processBulkUpdate(bulkVendorProductAssignRequest);
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
  }

  @Test
  public void processBulkUpdateContentSuccessTest() throws Exception {
    when(systemParameter.getVendorProductAssigneeBulkUpdateBatchSize()).thenReturn(VENDOR_BULK_ASSIGNMENT_BATCH_SIZE);
    when(bulkUpdateServiceUtil.getBulkProcess(bulkVendorProductAssignRequestSuccess, 0, 0)).thenReturn(bulkProcess);
    when(bulkUpdateServiceUtil.savePreProcessBulkData(bulkProcess)).thenReturn(bulkProcess);
    when(productDistributionTaskRepository
        .bulkVendorProductActions(any(BulkVendorProductActionsRequest.class)))
        .thenReturn(bulkVendorProductActionsResponse);
    vendorProductBulkAssignServiceBean.processBulkUpdate(bulkVendorProductAssignRequestSuccess);
    verify(systemParameter).getVendorProductAssigneeBulkUpdateBatchSize();
    verify(bulkUpdateServiceUtil).getBulkProcess(bulkVendorProductAssignRequestSuccess, 0, 0);
    verify(bulkUpdateServiceUtil).savePreProcessBulkData(bulkProcess);
    verify(productDistributionTaskRepository)
        .bulkVendorProductActions(bulkVendorProductActionsRequestArgumentCaptor.capture());
    verify(bulkProcessService, times(2)).saveOperation(bulkProcessArgumentCaptor.capture());
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, DEFAULT_BULK_PROCESS_TYPE);
    verify(notificationService).sendBulkAssignNotification(
        bulkProcess, bulkProcess.getSuccessCount(), bulkProcess.getTotalCount(),
      bulkVendorProductAssignRequestSuccess.getVendorCode());
    Assertions.assertEquals(contentSuccessProductList,
        bulkVendorProductActionsRequestArgumentCaptor.getAllValues().get(0).getBulkScreeningProductActionsRequests()
            .get(0).getProductCodes());
    Assertions.assertEquals(CONTENT_ASSIGN, bulkVendorProductActionsRequestArgumentCaptor.getValue().getAssignmentType());
    Assertions.assertEquals(UPDATED_BY, bulkVendorProductActionsRequestArgumentCaptor.getValue().getUserName());
    Assertions.assertEquals(STORE_ID, bulkVendorProductActionsRequestArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(ACTION_TYPE, bulkVendorProductActionsRequestArgumentCaptor.getValue().getActionType());
    Assertions.assertTrue(bulkProcessArgumentCaptor.getValue().getBulkUpdate());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcessArgumentCaptor.getValue().getErrorCount());
    Assertions.assertEquals(Integer.valueOf(4), bulkProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(Integer.valueOf(4), bulkProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcessArgumentCaptor.getValue().getInputErrorCount());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcessArgumentCaptor.getValue().getSystemErrorCount());
    Assertions.assertEquals(BulkProcess.STATUS_FINISHED, bulkProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void processBulkUpdateContentExceptionTest() throws Exception {
    bulkVendorProductAssignRequest.setFilePath(exceptionFile.getAbsolutePath());
    when(systemParameter.getVendorProductAssigneeBulkUpdateBatchSize()).thenReturn(VENDOR_BULK_ASSIGNMENT_BATCH_SIZE);
    when(bulkUpdateServiceUtil.getBulkProcess(bulkVendorProductAssignRequest, 0, 0)).thenReturn(bulkProcess);
    when(bulkUpdateServiceUtil.savePreProcessBulkData(bulkProcess)).thenReturn(bulkProcess);
    vendorProductBulkAssignServiceBean.processBulkUpdate(bulkVendorProductAssignRequest);
    verify(systemParameter).getVendorProductAssigneeBulkUpdateBatchSize();
    verify(bulkUpdateServiceUtil).getBulkProcess(bulkVendorProductAssignRequest, 0, 0);
    verify(bulkUpdateServiceUtil).savePreProcessBulkData(bulkProcess);
    verify(bulkProcessService, times(2)).saveOperation(bulkProcessArgumentCaptor.capture());
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(notificationService).sendBulkAssignNotification(
        bulkProcess, bulkProcess.getSuccessCount(), bulkProcess.getTotalCount(),
      bulkVendorProductAssignRequest.getVendorCode());
    Assertions.assertTrue(bulkProcessArgumentCaptor.getValue().getBulkUpdate());
    Assertions.assertEquals(Integer.valueOf(6), bulkProcessArgumentCaptor.getValue().getErrorCount());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(Integer.valueOf(6), bulkProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(Integer.valueOf(6), bulkProcessArgumentCaptor.getValue().getInputErrorCount());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcessArgumentCaptor.getValue().getSystemErrorCount());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void processBulkUpdateEmptyImageAssigneeExceptionTest() throws Exception {
    bulkVendorProductAssignRequest.setFilePath(exceptionFile.getAbsolutePath());
    bulkVendorProductAssignRequest.setAssignmentType(IMAGE_ASSIGN);
    when(systemParameter.getVendorProductAssigneeBulkUpdateBatchSize()).thenReturn(VENDOR_BULK_ASSIGNMENT_BATCH_SIZE);
    when(bulkUpdateServiceUtil.getBulkProcess(bulkVendorProductAssignRequest, 0, 0)).thenReturn(bulkProcess);
    when(bulkUpdateServiceUtil.savePreProcessBulkData(bulkProcess)).thenReturn(bulkProcess);
    vendorProductBulkAssignServiceBean.processBulkUpdate(bulkVendorProductAssignRequest);
    verify(systemParameter).getVendorProductAssigneeBulkUpdateBatchSize();
    verify(bulkUpdateServiceUtil).getBulkProcess(bulkVendorProductAssignRequest, 0, 0);
    verify(bulkUpdateServiceUtil).savePreProcessBulkData(bulkProcess);
    verify(bulkProcessService, times(2)).saveOperation(bulkProcessArgumentCaptor.capture());
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(notificationService).sendBulkAssignNotification(
        bulkProcess, bulkProcess.getSuccessCount(), bulkProcess.getTotalCount(),
      bulkVendorProductAssignRequest.getVendorCode());
    Assertions.assertTrue(bulkProcessArgumentCaptor.getValue().getBulkUpdate());
    Assertions.assertEquals(Integer.valueOf(6), bulkProcessArgumentCaptor.getValue().getErrorCount());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(Integer.valueOf(6), bulkProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(Integer.valueOf(6), bulkProcessArgumentCaptor.getValue().getInputErrorCount());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcessArgumentCaptor.getValue().getSystemErrorCount());
    Assertions.assertEquals(BulkProcess.STATUS_ABORTED, bulkProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void processBulkUpdateImageTest() throws Exception {
    bulkVendorProductAssignRequest.setAssignmentType(IMAGE_ASSIGN);
    when(systemParameter.getVendorProductAssigneeBulkUpdateBatchSize()).thenReturn(VENDOR_BULK_ASSIGNMENT_BATCH_SIZE);
    when(bulkUpdateServiceUtil.getBulkProcess(bulkVendorProductAssignRequest, 0, 0)).thenReturn(bulkProcess);
    when(bulkUpdateServiceUtil.savePreProcessBulkData(bulkProcess)).thenReturn(bulkProcess);
    when(productDistributionTaskRepository
        .bulkVendorProductActions(any(BulkVendorProductActionsRequest.class)))
        .thenReturn(bulkVendorProductActionsResponse);
    vendorProductBulkAssignServiceBean.processBulkUpdate(bulkVendorProductAssignRequest);
    verify(systemParameter).getVendorProductAssigneeBulkUpdateBatchSize();
    verify(bulkUpdateServiceUtil).getBulkProcess(bulkVendorProductAssignRequest, 0, 0);
    verify(bulkUpdateServiceUtil).savePreProcessBulkData(bulkProcess);
    verify(productDistributionTaskRepository)
        .bulkVendorProductActions(bulkVendorProductActionsRequestArgumentCaptor.capture());
    verify(bulkProcessService, times(2)).saveOperation(bulkProcessArgumentCaptor.capture());
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(notificationService).sendBulkAssignNotification(
        bulkProcess, bulkProcess.getSuccessCount(), bulkProcess.getTotalCount(),
      bulkVendorProductAssignRequest.getVendorCode());
    Assertions.assertEquals(imageSuccessProductList,
        bulkVendorProductActionsRequestArgumentCaptor.getAllValues().get(0).getBulkScreeningProductActionsRequests()
            .get(0).getProductCodes());
    Assertions.assertEquals(IMAGE_ASSIGN, bulkVendorProductActionsRequestArgumentCaptor.getValue().getAssignmentType());
    Assertions.assertEquals(UPDATED_BY, bulkVendorProductActionsRequestArgumentCaptor.getValue().getUserName());
    Assertions.assertEquals(STORE_ID, bulkVendorProductActionsRequestArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(ACTION_TYPE, bulkVendorProductActionsRequestArgumentCaptor.getValue().getActionType());
    Assertions.assertTrue(bulkProcessArgumentCaptor.getValue().getBulkUpdate());
    Assertions.assertEquals(Integer.valueOf(4), bulkProcessArgumentCaptor.getValue().getErrorCount());
    Assertions.assertEquals(Integer.valueOf(2), bulkProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(Integer.valueOf(6), bulkProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(Integer.valueOf(4), bulkProcessArgumentCaptor.getValue().getInputErrorCount());
    Assertions.assertEquals(Integer.valueOf(0), bulkProcessArgumentCaptor.getValue().getSystemErrorCount());
    Assertions.assertEquals(BulkProcess.STATUS_PARTIALLY_DONE, bulkProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void processBulkUpdateImageExceptionTest() throws Exception {
    bulkVendorProductAssignRequest.setAssignmentType(IMAGE_ASSIGN);
    when(systemParameter.getVendorProductAssigneeBulkUpdateBatchSize()).thenReturn(VENDOR_BULK_ASSIGNMENT_BATCH_SIZE);
    when(bulkUpdateServiceUtil.getBulkProcess(bulkVendorProductAssignRequest, 0, 0)).thenReturn(bulkProcess);
    when(bulkUpdateServiceUtil.savePreProcessBulkData(bulkProcess)).thenReturn(bulkProcess);
    Mockito.doThrow(Exception.class).when(productDistributionTaskRepository).
    bulkVendorProductActions(any(BulkVendorProductActionsRequest.class));
    vendorProductBulkAssignServiceBean.processBulkUpdate(bulkVendorProductAssignRequest);
    verify(systemParameter).getVendorProductAssigneeBulkUpdateBatchSize();
    verify(bulkUpdateServiceUtil).getBulkProcess(bulkVendorProductAssignRequest, 0, 0);
    verify(bulkUpdateServiceUtil).savePreProcessBulkData(bulkProcess);
    verify(productDistributionTaskRepository)
        .bulkVendorProductActions(bulkVendorProductActionsRequestArgumentCaptor.capture());
    verify(bulkProcessService, times(2)).saveOperation(bulkProcessArgumentCaptor.capture());
    verify(bulkProcessService).findByBulkProcessCode(STORE_ID, BULK_PROCESS_CODE);
    verify(notificationService).sendBulkAssignNotification(
        bulkProcess, bulkProcess.getSuccessCount(), bulkProcess.getTotalCount(),
      bulkVendorProductAssignRequest.getVendorCode());
    Assertions.assertEquals(imageSuccessProductList,
        bulkVendorProductActionsRequestArgumentCaptor.getAllValues().get(0).getBulkScreeningProductActionsRequests()
            .get(0).getProductCodes());
    Assertions.assertEquals(IMAGE_ASSIGN, bulkVendorProductActionsRequestArgumentCaptor.getValue().getAssignmentType());
    Assertions.assertEquals(UPDATED_BY, bulkVendorProductActionsRequestArgumentCaptor.getValue().getUserName());
    Assertions.assertEquals(STORE_ID, bulkVendorProductActionsRequestArgumentCaptor.getValue().getStoreId());
    Assertions.assertEquals(ACTION_TYPE, bulkVendorProductActionsRequestArgumentCaptor.getValue().getActionType());
    Assertions.assertTrue(bulkProcessArgumentCaptor.getValue().getBulkUpdate());
    Assertions.assertEquals(Integer.valueOf(4), bulkProcessArgumentCaptor.getValue().getErrorCount());
    Assertions.assertEquals(Integer.valueOf(2), bulkProcessArgumentCaptor.getValue().getSuccessCount());
    Assertions.assertEquals(Integer.valueOf(6), bulkProcessArgumentCaptor.getValue().getTotalCount());
    Assertions.assertEquals(Integer.valueOf(4), bulkProcessArgumentCaptor.getValue().getInputErrorCount());
    Assertions.assertEquals(Integer.valueOf(2), bulkProcessArgumentCaptor.getValue().getSystemErrorCount());
    Assertions.assertEquals(BulkProcess.STATUS_PARTIALLY_DONE, bulkProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelTest() throws IOException {
    bulkInternalProcessDataList.forEach(
      bulkInternalProcessData -> bulkInternalProcessData.setProcessType(
        BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name()));
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
        .thenReturn(bulkInternalProcessDataList);
    Mockito
        .when(objectMapper.readValue(bulkInternalProcessDataList.get(0).getData(), VendorBulkAssignmentRequest.class))
        .thenReturn(vendorBulkAssignmentRequest);
    vendorProductBulkAssignServiceBean.setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    Mockito.verify(objectMapper)
        .readValue(bulkInternalProcessDataList.get(0).getData(), VendorBulkAssignmentRequest.class);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    verify(notificationService).sendBulkAssignNotification(Mockito.any(BulkProcess.class),
        Mockito.eq(1), Mockito.eq(2), Mockito.eq(bulkInternalProcess.getSellerCode()));
    Assertions.assertEquals(ProcessStatus.PARTIAL_COMPLETED.name(), bulkInternalProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(
        ProcessorUtils.BULK_VENDOR_ASSIGN + bulkInternalProcess.getInternalProcessRequestCode() + File.separator
            + bulkInternalProcess.getInternalProcessRequestCode() + ProcessorUtils
            .getFileFormat(bulkInternalProcess.getFileName()),
        bulkInternalProcessArgumentCaptor.getValue().getErrorFilePath());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelSuccessTest() throws IOException {
    bulkInternalProcessDataList.get(0).setErrorMessage(StringUtils.EMPTY);
    bulkInternalProcessDataList.get(1).setErrorMessage(StringUtils.EMPTY);
    bulkInternalProcessDataList.forEach(
      bulkInternalProcessData -> bulkInternalProcessData.setProcessType(
        BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name()));
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());

    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
        .thenReturn(bulkInternalProcessDataList);
    Mockito
        .when(objectMapper.readValue(bulkInternalProcessDataList.get(0).getData(), VendorBulkAssignmentRequest.class))
        .thenReturn(vendorBulkAssignmentRequest);
    vendorProductBulkAssignServiceBean.setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    Mockito.verify(objectMapper, times(2))
        .readValue(bulkInternalProcessDataList.get(0).getData(), VendorBulkAssignmentRequest.class);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    verify(notificationService).sendBulkAssignNotification(Mockito.any(BulkProcess.class),
        Mockito.eq(2), Mockito.eq(2), Mockito.eq(bulkInternalProcess.getSellerCode()));
    Assertions.assertEquals(ProcessStatus.COMPLETED.name(), bulkInternalProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(
        ProcessorUtils.BULK_VENDOR_ASSIGN + bulkInternalProcess.getInternalProcessRequestCode() + File.separator
            + bulkInternalProcess.getInternalProcessRequestCode() + ProcessorUtils
            .getFileFormat(bulkInternalProcess.getFileName()),
        bulkInternalProcessArgumentCaptor.getValue().getErrorFilePath());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelAutoAssignmentSuccessTest() throws IOException {
    bulkInternalProcessDataList.get(0).setErrorMessage(StringUtils.EMPTY);
    bulkInternalProcessDataList.get(1).setErrorMessage(StringUtils.EMPTY);
    bulkInternalProcessDataList.get(0).setStatus(ProcessStatus.COMPLETED.name());
    bulkInternalProcessDataList.get(1).setErrorMessage(ProcessStatus.COMPLETED.name());
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.toString());
    bulkInternalProcess.setErrorCount(1);
    bulkInternalProcess.setSuccessCount(2);
    bulkInternalProcess.setTotalCount(3);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
        .thenReturn(bulkInternalProcessDataList);
    Mockito
        .when(objectMapper.readValue(bulkInternalProcessDataList.get(0).getData(), VendorBulkAssignmentRequest.class))
        .thenReturn(vendorBulkAssignmentRequest);
    vendorProductBulkAssignServiceBean.setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    Mockito.verify(objectMapper)
        .readValue(bulkInternalProcessDataList.get(0).getData(), VendorBulkAssignmentRequest.class);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    verify(notificationService).sendBulkAssignNotification(Mockito.any(BulkProcess.class),
        Mockito.eq(1), Mockito.eq(3), Mockito.eq(bulkInternalProcess.getSellerCode()));
    Assertions.assertEquals(ProcessStatus.PARTIAL_COMPLETED.name(),
        bulkInternalProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertNull(bulkInternalProcessArgumentCaptor.getValue().getErrorFilePath());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelAutoAssignmentPartialTest() throws IOException {
    BulkInternalProcessData bulkInternalProcessData3 = BulkInternalProcessData.builder()
      .processType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name())
      .internalProcessRequestCode(BULK_PROCESS_CODE).status(ProcessStatus.FAILED.name())
      .data(mapper.writeValueAsString(vendorBulkAssignmentRequest)).build();
    bulkInternalProcessDataList.get(0).setErrorMessage(ProcessStatus.COMPLETED.name());
    bulkInternalProcessDataList.get(1).setErrorMessage(ProcessStatus.COMPLETED.name());
    bulkInternalProcessData3.setErrorMessage(BulkInternalProcessData.COLUMN_ERROR_MESSAGE);
    bulkInternalProcessDataList.get(0).setStatus(ProcessStatus.FAILED.name());
    bulkInternalProcessDataList.get(1).setStatus(ProcessStatus.FAILED.name());
    bulkInternalProcessDataList.get(1).setErrorMessage(ProcessStatus.COMPLETED.name());
    bulkInternalProcessDataList.add(bulkInternalProcessData3);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.toString());
    bulkInternalProcess.setErrorCount(3);
    bulkInternalProcess.setSuccessCount(0);
    bulkInternalProcess.setTotalCount(3);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
      .thenReturn(bulkInternalProcessDataList);
    vendorProductBulkAssignServiceBean.setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    verify(notificationService).sendBulkAssignNotification(Mockito.any(BulkProcess.class),
      Mockito.eq(0), Mockito.eq(3), Mockito.eq(bulkInternalProcess.getSellerCode()));
    Assertions.assertEquals(ProcessStatus.FAILED.name(),
      bulkInternalProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertNull(bulkInternalProcessArgumentCaptor.getValue().getErrorFilePath());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelAutoAssignmentCompleteSuccessTest() throws IOException {
    BulkInternalProcessData bulkInternalProcessData3 = BulkInternalProcessData.builder()
      .processType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name())
      .internalProcessRequestCode(BULK_PROCESS_CODE).status(ProcessStatus.COMPLETED.name())
      .data(mapper.writeValueAsString(vendorBulkAssignmentRequest)).build();
    bulkInternalProcessDataList.get(0).setErrorMessage(StringUtils.EMPTY);
    bulkInternalProcessDataList.get(1).setErrorMessage(StringUtils.EMPTY);
    bulkInternalProcessDataList.get(0).setStatus(ProcessStatus.COMPLETED.name());
    bulkInternalProcessDataList.get(1).setStatus(ProcessStatus.COMPLETED.name());
    bulkInternalProcessDataList.add(bulkInternalProcessData3);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.toString());
    bulkInternalProcess.setErrorCount(0);
    bulkInternalProcess.setSuccessCount(3);
    bulkInternalProcess.setTotalCount(3);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
      .thenReturn(bulkInternalProcessDataList);
    Mockito
      .when(objectMapper.readValue(bulkInternalProcessDataList.get(0).getData(), VendorBulkAssignmentRequest.class))
      .thenReturn(vendorBulkAssignmentRequest);
    vendorProductBulkAssignServiceBean.setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    Mockito.verify(objectMapper, times(3))
      .readValue(bulkInternalProcessDataList.get(0).getData(), VendorBulkAssignmentRequest.class);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    verify(notificationService).sendBulkAssignNotification(Mockito.any(BulkProcess.class),
      Mockito.eq(3), Mockito.eq(3), Mockito.eq(bulkInternalProcess.getSellerCode()));
    Assertions.assertEquals(ProcessStatus.COMPLETED.name(),
      bulkInternalProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertNull(bulkInternalProcessArgumentCaptor.getValue().getErrorFilePath());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelEmptyTest() throws IOException {
    BulkInternalProcessData bulkInternalProcessData3 = BulkInternalProcessData.builder()
      .processType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name())
      .internalProcessRequestCode(BULK_PROCESS_CODE).status(ProcessStatus.COMPLETED.name())
      .data(mapper.writeValueAsString(vendorBulkAssignmentRequest)).build();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.toString());
    bulkInternalProcess.setErrorCount(0);
    bulkInternalProcess.setSuccessCount(3);
    bulkInternalProcess.setTotalCount(3);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
      .thenReturn(Collections.EMPTY_LIST);
    vendorProductBulkAssignServiceBean.setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelAutoAssignmentPendingTest() throws IOException {
    BulkInternalProcessData bulkInternalProcessData3 = BulkInternalProcessData.builder()
      .processType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name())
      .internalProcessRequestCode(BULK_PROCESS_CODE).status(ProcessStatus.IN_PROGRESS.name())
      .data(mapper.writeValueAsString(vendorBulkAssignmentRequest)).build();
    bulkInternalProcessDataList.get(0).setErrorMessage(StringUtils.EMPTY);
    bulkInternalProcessDataList.get(1).setErrorMessage(StringUtils.EMPTY);
    bulkInternalProcessDataList.get(0).setStatus(ProcessStatus.PENDING.name());
    bulkInternalProcessDataList.get(1).setStatus(ProcessStatus.PENDING.name());
    bulkInternalProcessDataList.add(bulkInternalProcessData3);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.toString());
    bulkInternalProcess.setErrorCount(0);
    bulkInternalProcess.setSuccessCount(3);
    bulkInternalProcess.setTotalCount(3);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
      .thenReturn(bulkInternalProcessDataList);
    Mockito
      .when(objectMapper.readValue(bulkInternalProcessDataList.get(0).getData(), VendorBulkAssignmentRequest.class))
      .thenReturn(vendorBulkAssignmentRequest);
    vendorProductBulkAssignServiceBean.setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelAutoAssignmentInProgressTest() throws IOException {
    BulkInternalProcessData bulkInternalProcessData3 = BulkInternalProcessData.builder()
      .processType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.name())
      .internalProcessRequestCode(BULK_PROCESS_CODE).status(ProcessStatus.IN_PROGRESS.name())
      .data(mapper.writeValueAsString(vendorBulkAssignmentRequest)).build();
    bulkInternalProcessDataList.get(0).setErrorMessage(StringUtils.EMPTY);
    bulkInternalProcessDataList.get(1).setErrorMessage(StringUtils.EMPTY);
    bulkInternalProcessDataList.get(0).setStatus(ProcessStatus.IN_PROGRESS.name());
    bulkInternalProcessDataList.get(1).setStatus(ProcessStatus.IN_PROGRESS.name());
    bulkInternalProcessDataList.add(bulkInternalProcessData3);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.toString());
    bulkInternalProcess.setErrorCount(0);
    bulkInternalProcess.setSuccessCount(3);
    bulkInternalProcess.setTotalCount(3);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
      .thenReturn(bulkInternalProcessDataList);
    Mockito
      .when(objectMapper.readValue(bulkInternalProcessDataList.get(0).getData(), VendorBulkAssignmentRequest.class))
      .thenReturn(vendorBulkAssignmentRequest);
    vendorProductBulkAssignServiceBean.setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
  }


  @Test
  public void setFinalStatusAndGenerateFailedExcelAutoAssignmentSuccessCountTest() throws IOException {
    bulkInternalProcessDataList.get(0).setErrorMessage(StringUtils.EMPTY);
    bulkInternalProcessDataList.get(1).setErrorMessage(StringUtils.EMPTY);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.toString());
    bulkInternalProcess.setErrorCount(1);
    bulkInternalProcess.setTotalCount(2);
    bulkInternalProcess.setSuccessCount(1);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
        .thenReturn(bulkInternalProcessDataList);
    Mockito
        .when(objectMapper.readValue(bulkInternalProcessDataList.get(0).getData(), VendorBulkAssignmentRequest.class))
        .thenReturn(vendorBulkAssignmentRequest);
    vendorProductBulkAssignServiceBean.setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    Mockito.verify(objectMapper, times(2))
        .readValue(bulkInternalProcessDataList.get(0).getData(), VendorBulkAssignmentRequest.class);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    verify(notificationService).sendBulkAssignNotification(Mockito.any(BulkProcess.class),
        Mockito.eq(2), Mockito.eq(2), Mockito.eq(bulkInternalProcess.getSellerCode()));
    Assertions.assertEquals(ProcessStatus.PARTIAL_COMPLETED.name(),
      bulkInternalProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertNull(bulkInternalProcessArgumentCaptor.getValue().getErrorFilePath());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelAutoAssignmentSuccessFalseTest() throws IOException {
    bulkInternalProcessDataList.get(0).setErrorMessage(StringUtils.EMPTY);
    bulkInternalProcessDataList.get(1).setErrorMessage(StringUtils.EMPTY);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_AUTO_ASSIGNMENT.toString());
    bulkInternalProcess.setErrorCount(0);
    bulkInternalProcess.setSuccessCount(1);
    bulkInternalProcess.setTotalCount(1);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
        .thenReturn(bulkInternalProcessDataList);
    Mockito
        .when(objectMapper.readValue(bulkInternalProcessDataList.get(0).getData(), VendorBulkAssignmentRequest.class))
        .thenReturn(vendorBulkAssignmentRequest);
    vendorProductBulkAssignServiceBean.setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    Mockito.verify(objectMapper, times(2))
        .readValue(bulkInternalProcessDataList.get(0).getData(), VendorBulkAssignmentRequest.class);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    verify(notificationService).sendBulkAssignNotification(Mockito.any(BulkProcess.class),
        Mockito.eq(2), Mockito.eq(1), Mockito.eq(bulkInternalProcess.getSellerCode()));
    Assertions.assertNull(bulkInternalProcessArgumentCaptor.getValue().getErrorFilePath());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelFailedTest() throws IOException {
    bulkInternalProcessDataList.get(0).setErrorMessage(BULK_PROCESS_CODE);
    bulkInternalProcessDataList.get(1).setErrorMessage(BULK_PROCESS_CODE);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name());
    bulkInternalProcessDataList.forEach(
      bulkInternalProcessData -> bulkInternalProcessData.setProcessType(
        BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name()));
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
        .thenReturn(bulkInternalProcessDataList);
    Mockito
        .when(objectMapper.readValue(bulkInternalProcessDataList.get(0).getData(), VendorBulkAssignmentRequest.class))
        .thenReturn(vendorBulkAssignmentRequest);
    vendorProductBulkAssignServiceBean.setFinalStatusAndGenerateFailedExcel(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    verify(notificationService).sendBulkAssignNotification(Mockito.any(BulkProcess.class),
        Mockito.eq(0), Mockito.eq(2), Mockito.eq(bulkInternalProcess.getSellerCode()));
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(
        ProcessorUtils.BULK_VENDOR_ASSIGN + bulkInternalProcess.getInternalProcessRequestCode() + File.separator
            + bulkInternalProcess.getInternalProcessRequestCode() + ProcessorUtils
            .getFileFormat(bulkInternalProcess.getFileName()),
        bulkInternalProcessArgumentCaptor.getValue().getErrorFilePath());
  }

  @Test
  public void processVendorBulkAssignmentTest() throws Exception {
    bulkInternalProcessData1.setData(mapper.writeValueAsString(vendorBulkAssignmentRequest));
    bulkInternalProcessData1.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(internalProcessService.getBulkInternalProcessDataById(STORE_ID, REQUEST_ID))
        .thenReturn(bulkInternalProcessData1);
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData1))
        .thenReturn(bulkInternalProcessData1);
    vendorProductBulkAssignServiceBean
        .processVendorBulkAssignment(STORE_ID, UPDATED_BY, BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name(),
            REQUEST_ID);
    Mockito.verify(internalProcessService).getBulkInternalProcessDataById(STORE_ID, REQUEST_ID);
    Mockito.verify(productDistributionTaskRepository)
        .bulkVendorProductActions(bulkVendorProductActionsRequestArgumentCaptor.capture());
    Assertions.assertEquals(VendorProductDataBulkParameters.ASSIGN,
        bulkVendorProductActionsRequestArgumentCaptor.getValue().getActionType());
    Assertions.assertEquals(PRODUCT_CODE,
        bulkVendorProductActionsRequestArgumentCaptor.getValue().getBulkScreeningProductActionsRequests().get(0)
            .getProductCodes().get(0));
    Assertions.assertEquals(ASSIGNED_BY,
        bulkVendorProductActionsRequestArgumentCaptor.getValue().getBulkScreeningProductActionsRequests().get(0)
            .getAssignedBy());
    Assertions.assertEquals(ASSIGNED_TO,
        bulkVendorProductActionsRequestArgumentCaptor.getValue().getBulkScreeningProductActionsRequests().get(0)
            .getAssignTo());
    Mockito.verify(internalProcessService, times(2))
        .saveBulkInternalProcessData(bulkInternalProcessDataArgumentCaptor.capture());
    Assertions.assertEquals(ProcessStatus.COMPLETED.name(), bulkInternalProcessDataArgumentCaptor.getValue().getStatus());
    Assertions.assertTrue(StringUtils.isBlank(bulkInternalProcessDataArgumentCaptor.getValue().getErrorMessage()));
  }

  @Test
  public void processVendorBulkAssignmentNonNullTest() throws Exception {
    bulkInternalProcessData1.setData(mapper.writeValueAsString(vendorBulkAssignmentRequest));
    bulkInternalProcessData1.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(internalProcessService.getBulkInternalProcessDataById(STORE_ID, REQUEST_ID))
        .thenReturn(bulkInternalProcessData1);
    Mockito.when(productDistributionTaskRepository.bulkVendorProductActions(Mockito.any()))
        .thenReturn(new BulkVendorProductActionsResponse());
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData1))
        .thenReturn(bulkInternalProcessData1);
    vendorProductBulkAssignServiceBean
        .processVendorBulkAssignment(STORE_ID, UPDATED_BY, BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name(),
            REQUEST_ID);
    Mockito.verify(internalProcessService).getBulkInternalProcessDataById(STORE_ID, REQUEST_ID);
    Mockito.verify(productDistributionTaskRepository)
        .bulkVendorProductActions(bulkVendorProductActionsRequestArgumentCaptor.capture());
    Assertions.assertEquals(VendorProductDataBulkParameters.ASSIGN,
        bulkVendorProductActionsRequestArgumentCaptor.getValue().getActionType());
    Assertions.assertEquals(PRODUCT_CODE,
        bulkVendorProductActionsRequestArgumentCaptor.getValue().getBulkScreeningProductActionsRequests().get(0)
            .getProductCodes().get(0));
    Assertions.assertEquals(ASSIGNED_BY,
        bulkVendorProductActionsRequestArgumentCaptor.getValue().getBulkScreeningProductActionsRequests().get(0)
            .getAssignedBy());
    Assertions.assertEquals(ASSIGNED_TO,
        bulkVendorProductActionsRequestArgumentCaptor.getValue().getBulkScreeningProductActionsRequests().get(0)
            .getAssignTo());
    Mockito.verify(internalProcessService, times(2))
        .saveBulkInternalProcessData(bulkInternalProcessDataArgumentCaptor.capture());
    Assertions.assertEquals(ProcessStatus.COMPLETED.name(), bulkInternalProcessDataArgumentCaptor.getValue().getStatus());
    Assertions.assertTrue(StringUtils.isBlank(bulkInternalProcessDataArgumentCaptor.getValue().getErrorMessage()));
  }

  @Test
  public void processVendorBulkAssignmentPDTTest() throws Exception {
    bulkInternalProcessData1.setData(mapper.writeValueAsString(vendorBulkAssignmentRequest));
    bulkInternalProcessData1.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(internalProcessService.getBulkInternalProcessDataById(STORE_ID, REQUEST_ID))
        .thenReturn(bulkInternalProcessData1);
    VendorProductActionsResponse vendorProductActionsResponse = new VendorProductActionsResponse();
    vendorProductActionsResponse.setReasonOfFailure(Constant.ERROR_IN_PRODUCT_CODE);
    Mockito.when(productDistributionTaskRepository.bulkVendorProductActions(Mockito.any()))
        .thenReturn(new BulkVendorProductActionsResponse(Collections.singletonList(vendorProductActionsResponse)));
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData1))
        .thenReturn(bulkInternalProcessData1);
    vendorProductBulkAssignServiceBean
        .processVendorBulkAssignment(STORE_ID, UPDATED_BY, BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name(),
            REQUEST_ID);
    Mockito.verify(internalProcessService).getBulkInternalProcessDataById(STORE_ID, REQUEST_ID);
    Mockito.verify(productDistributionTaskRepository)
        .bulkVendorProductActions(bulkVendorProductActionsRequestArgumentCaptor.capture());
    Assertions.assertEquals(VendorProductDataBulkParameters.ASSIGN,
        bulkVendorProductActionsRequestArgumentCaptor.getValue().getActionType());
    Assertions.assertEquals(PRODUCT_CODE,
        bulkVendorProductActionsRequestArgumentCaptor.getValue().getBulkScreeningProductActionsRequests().get(0)
            .getProductCodes().get(0));
    Assertions.assertEquals(ASSIGNED_BY,
        bulkVendorProductActionsRequestArgumentCaptor.getValue().getBulkScreeningProductActionsRequests().get(0)
            .getAssignedBy());
    Assertions.assertEquals(ASSIGNED_TO,
        bulkVendorProductActionsRequestArgumentCaptor.getValue().getBulkScreeningProductActionsRequests().get(0)
            .getAssignTo());
    Mockito.verify(internalProcessService, times(2))
        .saveBulkInternalProcessData(bulkInternalProcessDataArgumentCaptor.capture());
    Assertions.assertEquals(ProcessStatus.COMPLETED.name(), bulkInternalProcessDataArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void processVendorBulkAssignmentPDTErrorTest() throws Exception {
    bulkInternalProcessData1.setData(mapper.writeValueAsString(vendorBulkAssignmentRequest));
    bulkInternalProcessData1.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(internalProcessService.getBulkInternalProcessDataById(STORE_ID, REQUEST_ID))
        .thenReturn(bulkInternalProcessData1);
    VendorProductActionsResponse vendorProductActionsResponse = new VendorProductActionsResponse();
    vendorProductActionsResponse.setReasonOfFailure(Constant.ERROR_IN_PRODUCT_CODE);
    vendorProductActionsResponse.setProductCode(Collections.singletonList(PRODUCT_CODE));
    Mockito.when(productDistributionTaskRepository.bulkVendorProductActions(Mockito.any()))
        .thenReturn(new BulkVendorProductActionsResponse(Collections.singletonList(vendorProductActionsResponse)));
    Mockito.when(internalProcessService.saveBulkInternalProcessData(bulkInternalProcessData1))
        .thenReturn(bulkInternalProcessData1);
    vendorProductBulkAssignServiceBean
        .processVendorBulkAssignment(STORE_ID, UPDATED_BY, BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name(),
            REQUEST_ID);
    Mockito.verify(internalProcessService).getBulkInternalProcessDataById(STORE_ID, REQUEST_ID);
    Mockito.verify(productDistributionTaskRepository)
        .bulkVendorProductActions(bulkVendorProductActionsRequestArgumentCaptor.capture());
    Assertions.assertEquals(VendorProductDataBulkParameters.ASSIGN,
        bulkVendorProductActionsRequestArgumentCaptor.getValue().getActionType());
    Assertions.assertEquals(PRODUCT_CODE,
        bulkVendorProductActionsRequestArgumentCaptor.getValue().getBulkScreeningProductActionsRequests().get(0)
            .getProductCodes().get(0));
    Assertions.assertEquals(ASSIGNED_BY,
        bulkVendorProductActionsRequestArgumentCaptor.getValue().getBulkScreeningProductActionsRequests().get(0)
            .getAssignedBy());
    Assertions.assertEquals(ASSIGNED_TO,
        bulkVendorProductActionsRequestArgumentCaptor.getValue().getBulkScreeningProductActionsRequests().get(0)
            .getAssignTo());
    Mockito.verify(internalProcessService, times(2))
        .saveBulkInternalProcessData(bulkInternalProcessDataArgumentCaptor.capture());
    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(Constant.ERROR_IN_PRODUCT_CODE,
        bulkInternalProcessDataArgumentCaptor.getValue().getErrorMessage());
  }

  @Test
  public void processVendorBulkAssignmentFailedTest() throws Exception {
    bulkInternalProcessData1.setData(mapper.writeValueAsString(vendorBulkAssignmentRequest));
    bulkInternalProcessData1.setStatus(ProcessStatus.FAILED.name());
    Mockito.when(internalProcessService.getBulkInternalProcessDataById(STORE_ID, REQUEST_ID))
        .thenReturn(bulkInternalProcessData1);
    vendorProductBulkAssignServiceBean
        .processVendorBulkAssignment(STORE_ID, UPDATED_BY, BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name(),
            REQUEST_ID);
    Mockito.verify(internalProcessService).getBulkInternalProcessDataById(STORE_ID, REQUEST_ID);
  }

  @Test
  public void processVendorAutoAssignment() throws JsonProcessingException {
    VendorAutoAssignmentRequest vendorAutoAssignmentRequest = new VendorAutoAssignmentRequest();
    vendorProductBulkAssignServiceBean.processVendorAutoAssignment(vendorAutoAssignmentRequest);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    }

  @Test
  public void processVendorBulkAssignmentExceptionTest() throws Exception {
    bulkInternalProcessData1.setData(mapper.writeValueAsString(vendorBulkAssignmentRequest));
    bulkInternalProcessData1.setStatus(ProcessStatus.IN_PROGRESS.name());
    Mockito.when(internalProcessService.getBulkInternalProcessDataById(STORE_ID, REQUEST_ID))
        .thenReturn(bulkInternalProcessData1);
    Mockito.doThrow(Exception.class).when(productDistributionTaskRepository)
        .bulkVendorProductActions(Mockito.any(BulkVendorProductActionsRequest.class));
    Mockito.when(internalProcessService.saveBulkInternalProcessData(Mockito.any()))
        .thenReturn(bulkInternalProcessData1);
    vendorProductBulkAssignServiceBean
        .processVendorBulkAssignment(STORE_ID, UPDATED_BY, BulkInternalProcessType.VENDOR_BULK_ASSIGNMENT.name(),
            REQUEST_ID);
    Mockito.verify(internalProcessService).getBulkInternalProcessDataById(STORE_ID, REQUEST_ID);
    Mockito.verify(productDistributionTaskRepository)
        .bulkVendorProductActions(bulkVendorProductActionsRequestArgumentCaptor.capture());
    Assertions.assertEquals(VendorProductDataBulkParameters.ASSIGN,
        bulkVendorProductActionsRequestArgumentCaptor.getValue().getActionType());
    Assertions.assertEquals(PRODUCT_CODE,
        bulkVendorProductActionsRequestArgumentCaptor.getValue().getBulkScreeningProductActionsRequests().get(0)
            .getProductCodes().get(0));
    Assertions.assertEquals(ASSIGNED_BY,
        bulkVendorProductActionsRequestArgumentCaptor.getValue().getBulkScreeningProductActionsRequests().get(0)
            .getAssignedBy());
    Assertions.assertEquals(ASSIGNED_TO,
        bulkVendorProductActionsRequestArgumentCaptor.getValue().getBulkScreeningProductActionsRequests().get(0)
            .getAssignTo());
    Mockito.verify(internalProcessService, times(2)).saveBulkInternalProcessData(Mockito.any());
//    Assertions.assertEquals(ProcessStatus.FAILED.name(), bulkInternalProcessDataArgumentCaptor.getValue().getStatus());
//    Assertions.assertTrue(StringUtils.isNotBlank(bulkInternalProcessDataArgumentCaptor.getValue().getErrorMessage()));
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(bulkUpdateServiceUtil);
    Mockito.verifyNoMoreInteractions(systemParameter);
    Mockito.verifyNoMoreInteractions(productDistributionTaskRepository);
    Mockito.verifyNoMoreInteractions(bulkProcessService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(internalProcessService);
    Mockito.verifyNoMoreInteractions(notificationService);
  }

}