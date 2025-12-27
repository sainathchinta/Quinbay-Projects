package com.gdn.mta.bulk.service;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;

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

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.models.download.responsedata.BulkPriceRebateRequestData;

public class BulkRebateUpdateServiceImplTest {
  @InjectMocks
  private BulkRebateUpdateServiceImpl bulkRebateUpdateService;

  @Mock
  private InternalProcessService internalProcessService;
  @Mock
  private FileStorageService fileStorageService;
  @Mock
  private MailDeliveryService mailDeliveryService;
  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaPublisher kafkaProducer;

  @Captor
  private ArgumentCaptor<BulkInternalProcess> bulkInternalProcessArgumentCaptor;

  private static final String STORE_ID = "STORE_ID";
  private static final String ID = "ID";
  private static final String BULK_REBATE_UPDATE = "BULK_PRICE_REBATE";
  private static final String ERROR_MESSAGE = "ERROR_MESSAGE";
  private static final String COMPLETED = "COMPLETED";
  private static final String PARTIAL_COMPLETED = "PARTIAL_COMPLETED";
  private static final String FAILED = "FAILED";
  private static final String USERNAME = "USERNAME";

  private BulkInternalProcess bulkInternalProcess;
  private BulkInternalProcessData bulkInternalProcessData1;
  private BulkInternalProcessData bulkInternalProcessData2;
  private BulkPriceRebateRequestData bulkPriceRebateRequestData1;
  private BulkPriceRebateRequestData bulkPriceRebateRequestData2;
  private String data1;
  private String data2;

  @BeforeEach
  public void setUp() throws JsonProcessingException {
    MockitoAnnotations.initMocks(this);
    bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BULK_REBATE_UPDATE);
    bulkInternalProcess.setId(ID);
    bulkInternalProcess.setTotalCount(2);
    bulkInternalProcess.setInternalProcessRequestCode(ID);
    bulkInternalProcess.setCreatedBy(USERNAME);
    bulkInternalProcess.setFileName(USERNAME);
    bulkInternalProcess.setErrorFilePath(USERNAME);
    bulkInternalProcessData1 = new BulkInternalProcessData();
    bulkInternalProcessData2 = new BulkInternalProcessData();
    bulkPriceRebateRequestData1 = new BulkPriceRebateRequestData();
    bulkPriceRebateRequestData2 = new BulkPriceRebateRequestData();
    bulkPriceRebateRequestData1.setRowNumber(1);
    bulkPriceRebateRequestData2.setRowNumber(2);
    data1 = new ObjectMapper().writeValueAsString(bulkPriceRebateRequestData1);
    data2 = new ObjectMapper().writeValueAsString(bulkPriceRebateRequestData2);
    bulkInternalProcessData1.setData(data1);
    bulkInternalProcessData2.setData(data2);
    bulkInternalProcessData1.setStatus(COMPLETED);
    bulkInternalProcessData2.setStatus(COMPLETED);
    ReflectionTestUtils.setField(bulkRebateUpdateService, "gcsBulkRebateUpdateErrorPath", ID);
    ReflectionTestUtils.setField(bulkRebateUpdateService, "gcsBulkRebateUpdateErrorPath", "");
  }

  @AfterEach
  public void tearDown(){
    Mockito.verifyNoMoreInteractions(objectMapper,fileStorageService, mailDeliveryService, internalProcessService);
  }

  @Test
  public void setFinalStatusAndGenerateErrorFileForBulkRebateUpdate() throws Exception {
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
        .thenReturn(Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2));
    Mockito.when(objectMapper.readValue(data1, BulkPriceRebateRequestData.class))
        .thenReturn(bulkPriceRebateRequestData1);
    Mockito.when(objectMapper.readValue(data2, BulkPriceRebateRequestData.class))
        .thenReturn(bulkPriceRebateRequestData2);
    bulkRebateUpdateService.setFinalStatusAndGenerateErrorFileForBulkRebateUpdate(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Assertions.assertEquals(COMPLETED, bulkInternalProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(2, bulkInternalProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(0, bulkInternalProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Mockito.verify(objectMapper).readValue(data2, BulkPriceRebateRequestData.class);
    Mockito.verify(objectMapper).readValue(data1, BulkPriceRebateRequestData.class);
    Mockito.verify(mailDeliveryService).sendEmail(Mockito.any(), Mockito.any());
  }

  @Test
  public void setFinalStatusAndGenerateErrorFileForBulkRebateUpdateSuccessAndFailedRows() throws Exception {
    bulkInternalProcess.setFileName("fileName");
    bulkInternalProcessData1.setStatus(ProcessStatus.FAILED.name());
    bulkInternalProcessData2.setStatus(ProcessStatus.COMPLETED.name());
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
        .thenReturn(Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2));
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any())).thenReturn(bulkInternalProcess);
    Mockito.when(objectMapper.readValue(data1, BulkPriceRebateRequestData.class))
        .thenReturn(bulkPriceRebateRequestData1);
    Mockito.when(objectMapper.readValue(data2, BulkPriceRebateRequestData.class))
        .thenReturn(bulkPriceRebateRequestData2);
    Mockito.doNothing().when(fileStorageService)
        .downloadFileAndGenerateErrorFile(Mockito.anyString(), Mockito.anyString(), Mockito.anyList(), Mockito.any());
    bulkRebateUpdateService.setFinalStatusAndGenerateErrorFileForBulkRebateUpdate(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Assertions.assertEquals(PARTIAL_COMPLETED, bulkInternalProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(1, bulkInternalProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(1, bulkInternalProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Mockito.verify(objectMapper).readValue(data2, BulkPriceRebateRequestData.class);
    Mockito.verify(objectMapper).readValue(data1, BulkPriceRebateRequestData.class);
    Mockito.verify(mailDeliveryService).sendEmail(Mockito.any(), Mockito.any());
    Mockito.verify(fileStorageService)
        .downloadFileAndGenerateErrorFile(Mockito.anyString(), Mockito.anyString(), Mockito.anyList(), Mockito.any());
  }

  @Test
  public void setFinalStatusAndGenerateErrorFileForBulkRebateUpdateAllFailedRows() throws Exception {
    bulkInternalProcess.setFileName("fileName");
    bulkInternalProcessData1.setStatus(ProcessStatus.FAILED.name());
    bulkInternalProcessData2.setStatus(ProcessStatus.FAILED.name());
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
        .thenReturn(Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2));
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any())).thenReturn(bulkInternalProcess);
    Mockito.when(objectMapper.readValue(data1, BulkPriceRebateRequestData.class))
        .thenReturn(bulkPriceRebateRequestData1);
    Mockito.when(objectMapper.readValue(data2, BulkPriceRebateRequestData.class))
        .thenReturn(bulkPriceRebateRequestData2);
    Mockito.doNothing().when(fileStorageService)
        .downloadFileAndGenerateErrorFile(Mockito.anyString(), Mockito.anyString(), Mockito.anyList(), Mockito.any());
    bulkRebateUpdateService.setFinalStatusAndGenerateErrorFileForBulkRebateUpdate(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Assertions.assertEquals(FAILED, bulkInternalProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(0, bulkInternalProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(2, bulkInternalProcessArgumentCaptor.getValue().getErrorCount(), 0);
    Mockito.verify(objectMapper).readValue(data2, BulkPriceRebateRequestData.class);
    Mockito.verify(objectMapper).readValue(data1, BulkPriceRebateRequestData.class);
    Mockito.verify(fileStorageService)
        .downloadFileAndGenerateErrorFile(Mockito.anyString(), Mockito.anyString(), Mockito.anyList(), Mockito.any());
    Mockito.verify(mailDeliveryService).sendEmail(Mockito.any(), Mockito.any());
  }

  @Test
  public void setFinalStatusAndGenerateErrorFileForBulkRebateUpdatePendingProcess() throws Exception {
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
        .thenReturn(new ArrayList<>());
    bulkRebateUpdateService.setFinalStatusAndGenerateErrorFileForBulkRebateUpdate(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
  }

  @Test
  public void setFinalStatusAndGenerateErrorFileForBulkRebateUpdatePendingProcess2() throws Exception {
    bulkInternalProcessData1.setStatus(ProcessStatus.PENDING.name());
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
        .thenReturn(Collections.singletonList(bulkInternalProcessData1));
    bulkRebateUpdateService.setFinalStatusAndGenerateErrorFileForBulkRebateUpdate(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
  }
}