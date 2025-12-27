package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.models.download.responsedata.BulkMasterSkuReviewRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkAssigneeMasterSkuReviewRequestData;
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
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Arrays;
import java.util.Collections;

public class BulkMasterSkuReviewServiceImplTest {

  @InjectMocks
  private BulkMasterSkuReviewServiceImpl bulkMasterSkuReviewService;

  @Mock
  private InternalProcessService internalProcessService;

  @Mock
  private FileStorageService fileStorageService;

  @Mock
  private MailDeliveryService mailDeliveryService;

  @Mock
  private ObjectMapper objectMapper;

  @Captor
  private ArgumentCaptor<BulkInternalProcess> bulkInternalProcessArgumentCaptor;

  private static final String STORE_ID = "STORE_ID";
  private static final String ID = "ID";
  private static final String MASTER_SKU_BULK_ASSIGNEE = "MASTER_SKU_BULK_ASSIGNEE";
  private static final String COMPLETED = "COMPLETED";
  private static final String PARTIAL_COMPLETED = "PARTIAL_COMPLETED";
  private static final String FAILED = "FAILED";
  private static final String USERNAME = "USERNAME";
  private static final String FIRST_ANCHOR = "firstAnchor";
  private static final String SECOND_ANCHOR = "secondAnchor";

  private BulkInternalProcess bulkInternalProcess;
  private BulkInternalProcessData bulkInternalProcessData1;
  private BulkInternalProcessData bulkInternalProcessData2;
  private BulkAssigneeMasterSkuReviewRequestData bulkAssigneeMasterSkuReviewRequestData;
  private BulkMasterSkuReviewRequestData bulKMasterSkuReviewRequestData;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(MASTER_SKU_BULK_ASSIGNEE);
    bulkInternalProcess.setId(ID);
    bulkInternalProcess.setTotalCount(2);
    bulkInternalProcess.setInternalProcessRequestCode(ID);
    bulkInternalProcess.setCreatedBy(USERNAME);
    bulkInternalProcess.setFileName(USERNAME);
    bulkInternalProcess.setErrorFilePath(USERNAME);
    bulkInternalProcessData1 = new BulkInternalProcessData();
    bulkInternalProcessData2 = new BulkInternalProcessData();
    ReflectionTestUtils.setField(bulkMasterSkuReviewService, "masterSkuBulkAssigneeErrorFilePath",
      ID);
    ReflectionTestUtils.setField(bulkMasterSkuReviewService, "masterSkuBulkReviewErrorFilePath", ID);
    bulkAssigneeMasterSkuReviewRequestData = new BulkAssigneeMasterSkuReviewRequestData();
    bulKMasterSkuReviewRequestData = new BulkMasterSkuReviewRequestData();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(internalProcessService);
    Mockito.verifyNoMoreInteractions(mailDeliveryService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(fileStorageService);
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelFailedTest() throws Exception {
    bulkInternalProcess.setTotalCount(1);
    bulkAssigneeMasterSkuReviewRequestData.setRowNumber(1);
    bulkAssigneeMasterSkuReviewRequestData.setFirstAnchorSku(FIRST_ANCHOR);
    bulkAssigneeMasterSkuReviewRequestData.setSecondAnchorSku(SECOND_ANCHOR);
    String message = new ObjectMapper().writeValueAsString(bulkAssigneeMasterSkuReviewRequestData);
    bulkInternalProcessData1.setStatus(ProcessStatus.FAILED.name());
    bulkInternalProcessData1.setData(message);
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any()))
      .thenReturn(bulkInternalProcess);
    Mockito.when(
      internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        STORE_ID, ID)).thenReturn(Arrays.asList(bulkInternalProcessData1));
    Mockito.when(objectMapper.readValue(message, BulkAssigneeMasterSkuReviewRequestData.class))
      .thenReturn(bulkAssigneeMasterSkuReviewRequestData);
    bulkMasterSkuReviewService.setFinalStatusAndGenerateFailedExcelForBulkAssignee(STORE_ID,
      bulkInternalProcess);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    Mockito.verify(internalProcessService)
      .saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Assertions.assertEquals(FAILED, bulkInternalProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(1, bulkInternalProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(0, bulkInternalProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(1, bulkInternalProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Mockito.verify(mailDeliveryService).sendEmail(Mockito.any(), Mockito.anyMap());
    Mockito.verify(objectMapper).readValue(message, BulkAssigneeMasterSkuReviewRequestData.class);
    Mockito.verify(fileStorageService)
      .downloadFileAndGenerateErrorFile(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyList(), Mockito.any());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelPartialSuccessTest() throws Exception {
    bulkAssigneeMasterSkuReviewRequestData.setRowNumber(1);
    bulkAssigneeMasterSkuReviewRequestData.setFirstAnchorSku(FIRST_ANCHOR);
    bulkAssigneeMasterSkuReviewRequestData.setSecondAnchorSku(SECOND_ANCHOR);
    BulkAssigneeMasterSkuReviewRequestData bulkAssigneeMasterSkuReviewRequestData2 =
      new BulkAssigneeMasterSkuReviewRequestData();
    bulkAssigneeMasterSkuReviewRequestData2.setRowNumber(2);
    bulkAssigneeMasterSkuReviewRequestData2.setFirstAnchorSku(FIRST_ANCHOR);
    bulkAssigneeMasterSkuReviewRequestData2.setSecondAnchorSku(SECOND_ANCHOR);
    String message1 = new ObjectMapper().writeValueAsString(bulkAssigneeMasterSkuReviewRequestData);
    String message2 =
      new ObjectMapper().writeValueAsString(bulkAssigneeMasterSkuReviewRequestData2);
    bulkInternalProcessData1.setStatus(ProcessStatus.FAILED.name());
    bulkInternalProcessData2.setStatus(ProcessStatus.COMPLETED.name());
    bulkInternalProcessData1.setData(message1);
    bulkInternalProcessData2.setData(message2);
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any()))
      .thenReturn(bulkInternalProcess);
    Mockito.when(
        internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
          STORE_ID, ID))
      .thenReturn(Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2));
    Mockito.when(objectMapper.readValue(message1, BulkAssigneeMasterSkuReviewRequestData.class))
      .thenReturn(bulkAssigneeMasterSkuReviewRequestData);
    Mockito.when(objectMapper.readValue(message2, BulkAssigneeMasterSkuReviewRequestData.class))
      .thenReturn(bulkAssigneeMasterSkuReviewRequestData2);
    bulkMasterSkuReviewService.setFinalStatusAndGenerateFailedExcelForBulkAssignee(STORE_ID,
      bulkInternalProcess);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    Mockito.verify(internalProcessService)
      .saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Assertions.assertEquals(PARTIAL_COMPLETED,
      bulkInternalProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(2, bulkInternalProcessArgumentCaptor.getValue().getTotalCount(), 0);
    Assertions.assertEquals(1, bulkInternalProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(1, bulkInternalProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Mockito.verify(mailDeliveryService).sendEmail(Mockito.any(), Mockito.anyMap());
    Mockito.verify(objectMapper).readValue(message1, BulkAssigneeMasterSkuReviewRequestData.class);
    Mockito.verify(objectMapper).readValue(message2, BulkAssigneeMasterSkuReviewRequestData.class);
    Mockito.verify(fileStorageService)
      .downloadFileAndGenerateErrorFile(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyList(), Mockito.any());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelForBulkBrandAuthAddTest() throws Exception {
    bulkAssigneeMasterSkuReviewRequestData.setRowNumber(1);
    String message = new ObjectMapper().writeValueAsString(bulkAssigneeMasterSkuReviewRequestData);
    bulkInternalProcessData1.setProcessType(BulkInternalProcessType.BRAND_AUTH_ADD.name());
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_ADD.name());
    bulkInternalProcess.setTotalCount(1);
    bulkInternalProcessData1.setStatus(COMPLETED);
    bulkInternalProcessData1.setData(message);
    Mockito.when(
        internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
          STORE_ID, bulkInternalProcess.getId()))
      .thenReturn(Collections.singletonList(bulkInternalProcessData1));
    Mockito.when(objectMapper.readValue(message, BulkAssigneeMasterSkuReviewRequestData.class))
      .thenReturn(bulkAssigneeMasterSkuReviewRequestData);
    bulkMasterSkuReviewService.setFinalStatusAndGenerateFailedExcelForBulkAssignee(STORE_ID,
      bulkInternalProcess);
    Mockito.verify(mailDeliveryService).sendEmail(Mockito.any(), Mockito.anyMap());
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID,
        bulkInternalProcess.getId());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any());
    Mockito.verify(objectMapper).readValue(message, BulkAssigneeMasterSkuReviewRequestData.class);
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelForBulkMasterSkuReviewTest() throws Exception {
    bulKMasterSkuReviewRequestData.setExcelRowNumber(1);
    String message = new ObjectMapper().writeValueAsString(bulKMasterSkuReviewRequestData);
    bulkInternalProcessData1.setProcessType(BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name());
    bulkInternalProcess.setProcessType(BulkInternalProcessType.MASTER_SKU_BULK_REVIEW.name());
    bulkInternalProcess.setTotalCount(1);
    bulkInternalProcessData1.setStatus(COMPLETED);
    bulkInternalProcessData1.setData(message);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID,
        bulkInternalProcess.getId())).thenReturn(Collections.singletonList(bulkInternalProcessData1));
    Mockito.when(objectMapper.readValue(message, BulkMasterSkuReviewRequestData.class))
        .thenReturn(bulKMasterSkuReviewRequestData);
    bulkMasterSkuReviewService.setFinalStatusAndGenerateFailedExcelForBulkMasterSkuReview(STORE_ID,
        bulkInternalProcess);
    Mockito.verify(mailDeliveryService).sendEmail(Mockito.any(), Mockito.anyMap());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, bulkInternalProcess.getId());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any());
    Mockito.verify(objectMapper).readValue(message, BulkMasterSkuReviewRequestData.class);
  }


  @Test
  public void setFinalStatusAndGenerateFailedExcelForBulkBrandAuthStatusTest() throws Exception {
    bulkInternalProcessData1.setStatus(PARTIAL_COMPLETED);
    Mockito.when(
        internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
          STORE_ID, bulkInternalProcess.getId()))
      .thenReturn(Collections.singletonList(bulkInternalProcessData1));
    bulkMasterSkuReviewService.setFinalStatusAndGenerateFailedExcelForBulkAssignee(STORE_ID,
      bulkInternalProcess);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID,
        bulkInternalProcess.getId());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelForBulkBrandAuthNullTest() throws Exception {
    Mockito.when(
      internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
        STORE_ID, bulkInternalProcess.getId())).thenReturn(null);
    bulkMasterSkuReviewService.setFinalStatusAndGenerateFailedExcelForBulkAssignee(STORE_ID,
      bulkInternalProcess);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID,
        bulkInternalProcess.getId());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelForBulkBrandAuthDeleteTest() throws Exception {
    bulkAssigneeMasterSkuReviewRequestData.setRowNumber(1);
    String message = new ObjectMapper().writeValueAsString(bulkAssigneeMasterSkuReviewRequestData);
    bulkInternalProcess.setProcessType(BulkInternalProcessType.BRAND_AUTH_DELETE.name());
    bulkInternalProcessData1.setProcessType(BulkInternalProcessType.BRAND_AUTH_DELETE.name());
    bulkInternalProcess.setTotalCount(1);
    bulkInternalProcessData1.setStatus(COMPLETED);
    bulkInternalProcessData1.setData(message);
    Mockito.when(
        internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
          STORE_ID, bulkInternalProcess.getId()))
      .thenReturn(Collections.singletonList(bulkInternalProcessData1));
    Mockito.when(objectMapper.readValue(message, BulkAssigneeMasterSkuReviewRequestData.class))
      .thenReturn(bulkAssigneeMasterSkuReviewRequestData);
    bulkMasterSkuReviewService.setFinalStatusAndGenerateFailedExcelForBulkAssignee(STORE_ID,
      bulkInternalProcess);
    Mockito.verify(mailDeliveryService).sendEmail(Mockito.any(), Mockito.anyMap());
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID,
        bulkInternalProcess.getId());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any());
    Mockito.verify(objectMapper).readValue(message, BulkAssigneeMasterSkuReviewRequestData.class);
  }
}
