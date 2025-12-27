package com.gdn.mta.bulk.service;

import java.util.ArrayList;
import java.util.Arrays;

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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.models.BulkPriceUpdateRequestData;

public class BulkPriceUpdateServiceTest {

  @InjectMocks
  private BulkPriceUpdateServiceImpl bulkPriceUpdateService;

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
  private static final String BULK_PRICE_UPDATE = "BULK_PRICE_UPDATE";
  private static final String ERROR_MESSAGE = "ERROR_MESSAGE";
  private static final String COMPLETED = "COMPLETED";
  private static final String PARTIAL_COMPLETED = "PARTIAL_COMPLETED";
  private static final String FAILED = "FAILED";
  private static final String USERNAME = "USERNAME";

  private BulkInternalProcess bulkInternalProcess;
  private BulkInternalProcessData bulkInternalProcessData1;
  private BulkInternalProcessData bulkInternalProcessData2;
  private BulkPriceUpdateRequestData bulkPriceUpdateRequestData1;
  private BulkPriceUpdateRequestData bulkPriceUpdateRequestData2;
  private String data1;
  private String data2;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BULK_PRICE_UPDATE);
    bulkInternalProcess.setId(ID);
    bulkInternalProcess.setTotalCount(2);
    bulkInternalProcess.setInternalProcessRequestCode(ID);
    bulkInternalProcess.setCreatedBy(USERNAME);
    bulkInternalProcess.setFileName(USERNAME);
    bulkInternalProcess.setErrorFilePath(USERNAME);
    bulkInternalProcessData1 = new BulkInternalProcessData();
    bulkInternalProcessData2 = new BulkInternalProcessData();
    bulkPriceUpdateRequestData1 = new BulkPriceUpdateRequestData();
    bulkPriceUpdateRequestData2 = new BulkPriceUpdateRequestData();
    bulkPriceUpdateRequestData1.setExcelRowNumber(1);
    bulkPriceUpdateRequestData2.setExcelRowNumber(2);
    data1 = new ObjectMapper().writeValueAsString(bulkPriceUpdateRequestData1);
    data2 = new ObjectMapper().writeValueAsString(bulkPriceUpdateRequestData2);
    bulkInternalProcessData1.setData(data1);
    bulkInternalProcessData2.setData(data2);
    bulkInternalProcessData1.setStatus(ProcessStatus.FINISHED.name());
    bulkInternalProcessData2.setStatus(ProcessStatus.FINISHED.name());
    ReflectionTestUtils.setField(bulkPriceUpdateService, "gcsBulkPriceUpdateErrorPath", ID);
    ReflectionTestUtils.setField(bulkPriceUpdateService, "gcsBulkPriceUpdateUploadPath", ID);
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelCompletedTest() throws Exception {
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
        .thenReturn(Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2));
    Mockito.when(objectMapper.readValue(data1, BulkPriceUpdateRequestData.class))
        .thenReturn(bulkPriceUpdateRequestData1);
    Mockito.when(objectMapper.readValue(data2, BulkPriceUpdateRequestData.class))
        .thenReturn(bulkPriceUpdateRequestData2);
    bulkPriceUpdateService.setFinalStatusAndGenerateFailedExcelForBulkPriceUpdate(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Assertions.assertEquals(COMPLETED, bulkInternalProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(2, bulkInternalProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(0, bulkInternalProcessArgumentCaptor.getValue().getErrorCount(), 0);
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelPartiallyCompletedTest() throws Exception {
    bulkInternalProcessData1.setErrorMessage(ERROR_MESSAGE);
    bulkInternalProcessData1.setStatus(FAILED);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
        .thenReturn(Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2));
    Mockito.when(objectMapper.readValue(data1, BulkPriceUpdateRequestData.class))
        .thenReturn(bulkPriceUpdateRequestData1);
    Mockito.when(objectMapper.readValue(data2, BulkPriceUpdateRequestData.class))
        .thenReturn(bulkPriceUpdateRequestData2);
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any())).thenReturn(bulkInternalProcess);
    bulkPriceUpdateService.setFinalStatusAndGenerateFailedExcelForBulkPriceUpdate(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Mockito.verify(fileStorageService)
        .downloadFileAndGenerateErrorFile(Mockito.anyString(), Mockito.anyString(), Mockito.anyList(), Mockito.any());
    Assertions.assertEquals(PARTIAL_COMPLETED, bulkInternalProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(1, bulkInternalProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(1, bulkInternalProcessArgumentCaptor.getValue().getErrorCount(), 0);
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelFailedTest() throws Exception {
    bulkInternalProcessData1.setErrorMessage(ERROR_MESSAGE);
    bulkInternalProcessData2.setErrorMessage(ERROR_MESSAGE);
    bulkInternalProcessData1.setStatus(FAILED);
    bulkInternalProcessData2.setStatus(FAILED);
    Mockito.when(objectMapper.readValue(data1, BulkPriceUpdateRequestData.class))
        .thenReturn(bulkPriceUpdateRequestData1);
    Mockito.when(objectMapper.readValue(data2, BulkPriceUpdateRequestData.class))
        .thenReturn(bulkPriceUpdateRequestData2);
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any())).thenReturn(bulkInternalProcess);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
        .thenReturn(Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2));
    bulkPriceUpdateService.setFinalStatusAndGenerateFailedExcelForBulkPriceUpdate(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Assertions.assertEquals(FAILED, bulkInternalProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(0, bulkInternalProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(2, bulkInternalProcessArgumentCaptor.getValue().getErrorCount(), 0);
  }

  @Test
  public void setFinalStatusAndGenerateEmptyDataTableTest() throws Exception {
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
        .thenReturn(new ArrayList<>());
    bulkPriceUpdateService.setFinalStatusAndGenerateFailedExcelForBulkPriceUpdate(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    Assertions.assertEquals(2, bulkInternalProcess.getTotalCount().intValue());
  }

  @Test
  public void setFinalStatusAndGenerateInProgressTest() throws Exception {
    bulkInternalProcessData1.setErrorMessage(ERROR_MESSAGE);
    bulkInternalProcessData2.setErrorMessage(ERROR_MESSAGE);
    bulkInternalProcessData1.setStatus(FAILED);
    bulkInternalProcessData2.setStatus(ERROR_MESSAGE);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
        .thenReturn(Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2));
    bulkPriceUpdateService.setFinalStatusAndGenerateFailedExcelForBulkPriceUpdate(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    Assertions.assertEquals(2, bulkInternalProcess.getTotalCount().intValue());
  }
}
