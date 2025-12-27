package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.models.download.responsedata.BulkAssignAutoApprovedProductsRequestData;
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
import org.mockito.MockitoAnnotations;

import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.mockito.Mockito.verify;

public class BulkAutoApprovedProductsServiceImplTest {

  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String BULK_UPDATE_FOLDER = "BulkUpdate";
  private static final String FILE_NAME = "auto-approved.xlsx";
  private static final String ID = "id";
  private static final String STORE_ID = "storeId";

  private List<BulkInternalProcessData> bulkInternalProcessDataList;
  private final ObjectMapper mapper = new ObjectMapper();
  private BulkAssignAutoApprovedProductsRequestData bulkAssignAutoApprovedProductsRequestData;
  private BulkInternalProcess bulkInternalProcess;

  @InjectMocks
  private BulkAutoApprovedProductsServiceImpl bulkAutoApprovedProductsService;

  @Mock
  private InternalProcessService internalProcessService;

  @Mock
  private NotificationService notificationService;

  @Captor
  private ArgumentCaptor<BulkInternalProcess> bulkInternalProcessArgumentCaptor;

  @BeforeEach
  public void setup() throws Exception {
    MockitoAnnotations.initMocks(this);
    bulkInternalProcessDataList = new ArrayList<>();
    bulkAssignAutoApprovedProductsRequestData = new BulkAssignAutoApprovedProductsRequestData();
    BulkInternalProcessData bulkInternalProcessData = new BulkInternalProcessData();
    bulkInternalProcessData.setErrorMessage(BULK_PROCESS_CODE);
    bulkInternalProcessData.setData(
      mapper.writeValueAsString(bulkAssignAutoApprovedProductsRequestData));
    BulkInternalProcessData bulkInternalProcessData1 = new BulkInternalProcessData();
    bulkInternalProcessData1.setData(
      mapper.writeValueAsString(bulkAssignAutoApprovedProductsRequestData));
    bulkInternalProcessDataList.add(bulkInternalProcessData);
    bulkInternalProcessDataList.add(bulkInternalProcessData1);

    ClassLoader classLoader = getClass().getClassLoader();
    File file =
      new File(classLoader.getResource(BULK_UPDATE_FOLDER + File.separator + FILE_NAME).getFile());
    String filePath = file.getAbsolutePath();
    bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setId(ID);
    bulkInternalProcess.setInternalProcessRequestCode(BULK_PROCESS_CODE);
    bulkInternalProcess.setFileName(filePath);
    bulkInternalProcess.setProcessType(
      BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(internalProcessService);
    Mockito.verifyNoMoreInteractions(notificationService);
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelTest() throws Exception {
    bulkInternalProcessDataList.get(0).setStatus(ProcessStatus.FAILED.name());
    bulkInternalProcessDataList.forEach(
      bulkInternalProcessData -> bulkInternalProcessData.setProcessType(
        BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name()));
    bulkInternalProcess.setProcessType(
      BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());
    Mockito.when(
      internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        STORE_ID, ID)).thenReturn(bulkInternalProcessDataList);
    bulkAutoApprovedProductsService.setFinalStatusAndGenerateNotificationForAutoApprovedBulkAssignee(
      STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    verify(notificationService).sendBulkAssignNotification(Mockito.any(BulkProcess.class),
      Mockito.eq(1), Mockito.eq(2), Mockito.eq(bulkInternalProcess.getSellerCode()));
    Assertions.assertEquals(ProcessStatus.PARTIAL_COMPLETED.name(),
      bulkInternalProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelSuccessTest() throws Exception {
    bulkInternalProcessDataList.get(0).setErrorMessage(StringUtils.EMPTY);
    bulkInternalProcessDataList.get(1).setErrorMessage(StringUtils.EMPTY);
    bulkInternalProcessDataList.forEach(
      bulkInternalProcessData -> bulkInternalProcessData.setProcessType(
        BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name()));
    bulkInternalProcess.setProcessType(
      BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());

    Mockito.when(
      internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        STORE_ID, ID)).thenReturn(bulkInternalProcessDataList);
    bulkAutoApprovedProductsService.setFinalStatusAndGenerateNotificationForAutoApprovedBulkAssignee(
      STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    verify(notificationService).sendBulkAssignNotification(Mockito.any(BulkProcess.class),
      Mockito.eq(2), Mockito.eq(2), Mockito.eq(bulkInternalProcess.getSellerCode()));
    Assertions.assertEquals(ProcessStatus.COMPLETED.name(),
      bulkInternalProcessArgumentCaptor.getValue().getStatus());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelEmptyTest() throws Exception {
    bulkInternalProcess.setProcessType(
      BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.toString());
    bulkInternalProcess.setErrorCount(0);
    bulkInternalProcess.setSuccessCount(3);
    bulkInternalProcess.setTotalCount(3);
    Mockito.when(
      internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        STORE_ID, ID)).thenReturn(Collections.EMPTY_LIST);
    bulkAutoApprovedProductsService.setFinalStatusAndGenerateNotificationForAutoApprovedBulkAssignee(
      STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelAutoAssignmentPendingTest() throws Exception {
    BulkInternalProcessData bulkInternalProcessData3 = BulkInternalProcessData.builder()
      .processType(BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name())
      .internalProcessRequestCode(BULK_PROCESS_CODE).status(ProcessStatus.IN_PROGRESS.name())
      .data(mapper.writeValueAsString(bulkAssignAutoApprovedProductsRequestData)).build();
    bulkInternalProcessDataList.get(0).setErrorMessage(StringUtils.EMPTY);
    bulkInternalProcessDataList.get(1).setErrorMessage(StringUtils.EMPTY);
    bulkInternalProcessDataList.get(0).setStatus(ProcessStatus.PENDING.name());
    bulkInternalProcessDataList.get(1).setStatus(ProcessStatus.PENDING.name());
    bulkInternalProcessDataList.add(bulkInternalProcessData3);
    bulkInternalProcess.setProcessType(
      BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.toString());
    bulkInternalProcess.setErrorCount(0);
    bulkInternalProcess.setSuccessCount(3);
    bulkInternalProcess.setTotalCount(3);
    Mockito.when(
      internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        STORE_ID, ID)).thenReturn(bulkInternalProcessDataList);
    bulkAutoApprovedProductsService.setFinalStatusAndGenerateNotificationForAutoApprovedBulkAssignee(
      STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelAutoAssignmentInProgressTest() throws Exception {
    BulkInternalProcessData bulkInternalProcessData3 = BulkInternalProcessData.builder()
      .processType(BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name())
      .internalProcessRequestCode(BULK_PROCESS_CODE).status(ProcessStatus.IN_PROGRESS.name())
      .data(mapper.writeValueAsString(bulkAssignAutoApprovedProductsRequestData)).build();
    bulkInternalProcessDataList.get(0).setErrorMessage(StringUtils.EMPTY);
    bulkInternalProcessDataList.get(1).setErrorMessage(StringUtils.EMPTY);
    bulkInternalProcessDataList.get(0).setStatus(ProcessStatus.IN_PROGRESS.name());
    bulkInternalProcessDataList.get(1).setStatus(ProcessStatus.IN_PROGRESS.name());
    bulkInternalProcessDataList.add(bulkInternalProcessData3);
    bulkInternalProcess.setProcessType(
      BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.toString());
    bulkInternalProcess.setErrorCount(0);
    bulkInternalProcess.setSuccessCount(3);
    bulkInternalProcess.setTotalCount(3);
    Mockito.when(
      internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        STORE_ID, ID)).thenReturn(bulkInternalProcessDataList);
    bulkAutoApprovedProductsService.setFinalStatusAndGenerateNotificationForAutoApprovedBulkAssignee(
      STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelFailedTest() throws Exception {
    bulkInternalProcessDataList.get(0).setStatus(ProcessStatus.FAILED.name());
    bulkInternalProcessDataList.get(1).setStatus(ProcessStatus.FAILED.name());
    bulkInternalProcess.setProcessType(
      BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name());
    bulkInternalProcessDataList.forEach(
      bulkInternalProcessData -> bulkInternalProcessData.setProcessType(
        BulkInternalProcessType.AUTO_APPROVED_PRODUCTS_BULK_ASSIGN.name()));
    Mockito.when(
      internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        STORE_ID, ID)).thenReturn(bulkInternalProcessDataList);
    bulkAutoApprovedProductsService.setFinalStatusAndGenerateNotificationForAutoApprovedBulkAssignee(
      STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    verify(notificationService).sendBulkAssignNotification(Mockito.any(BulkProcess.class),
      Mockito.eq(0), Mockito.eq(2), Mockito.eq(bulkInternalProcess.getSellerCode()));
    Assertions.assertEquals(ProcessStatus.FAILED.name(),
      bulkInternalProcessArgumentCaptor.getValue().getStatus());
  }
}