package com.gdn.mta.bulk.service;

import static org.mockito.ArgumentMatchers.anyMap;
import static org.mockito.ArgumentMatchers.anyString;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.models.download.responsedata.BulkPriceProductTypeTaggingRequest;
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

public class BulkProductTypeTaggingUpdateServiceImplTest {
  @InjectMocks
  private BulkProductTypeTaggingUpdateServiceImpl bulkProductTypeTaggingUpdateService;

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
  private BulkPriceProductTypeTaggingRequest bulkPriceProductTypeTaggingRequest;
  private BulkPriceProductTypeTaggingRequest bulkPriceProductTypeTaggingRequest1;
  private String data1;
  private String data2;
  private ObjectMapper mapper = new ObjectMapper();


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
    bulkPriceProductTypeTaggingRequest = new BulkPriceProductTypeTaggingRequest();
    bulkPriceProductTypeTaggingRequest1 = new BulkPriceProductTypeTaggingRequest();
    bulkPriceProductTypeTaggingRequest.setExcelRowNumber(1);
    bulkPriceProductTypeTaggingRequest1.setExcelRowNumber(2);
    data1 = new ObjectMapper().writeValueAsString(bulkPriceProductTypeTaggingRequest);
    data2 = new ObjectMapper().writeValueAsString(bulkPriceProductTypeTaggingRequest1);
    bulkInternalProcessData1.setData(data1);
    bulkInternalProcessData2.setData(data2);
    bulkInternalProcessData1.setStatus(COMPLETED);
    bulkInternalProcessData2.setStatus(COMPLETED);
    ReflectionTestUtils.setField(bulkProductTypeTaggingUpdateService, "gcsBulkProductTypeTaggingErrorFilePath", ID);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(mailDeliveryService, fileStorageService,
      internalProcessService, objectMapper);
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelCompletedTest() throws Exception {
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
      .thenReturn(Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2));
    Mockito.when(objectMapper.readValue(data1, BulkPriceProductTypeTaggingRequest.class))
      .thenReturn(bulkPriceProductTypeTaggingRequest);
    Mockito.when(objectMapper.readValue(data2, BulkPriceProductTypeTaggingRequest.class))
      .thenReturn(bulkPriceProductTypeTaggingRequest1);
    bulkProductTypeTaggingUpdateService.setFinalStatusAndGenerateFailedExcelForBulkProductTypeUpdate(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Mockito.verify(mailDeliveryService).sendEmail(Mockito.any(), anyMap());
    Mockito.verify(objectMapper).readValue(data1, BulkPriceProductTypeTaggingRequest.class);
    Mockito.verify(objectMapper).readValue(data2, BulkPriceProductTypeTaggingRequest.class);
    Assertions.assertEquals(COMPLETED, bulkInternalProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(2, bulkInternalProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(0, bulkInternalProcessArgumentCaptor.getValue().getErrorCount(), 0);
  }


  @Test
  public void setFinalStatusAndGenerateFailedExcelInCompletedTest() throws Exception {
    bulkInternalProcessData2.setStatus(PARTIAL_COMPLETED);
    bulkInternalProcessData1.setStatus(PARTIAL_COMPLETED);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
      .thenReturn(Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2));
    Mockito.when(objectMapper.readValue(data1, BulkPriceProductTypeTaggingRequest.class))
      .thenReturn(bulkPriceProductTypeTaggingRequest);
    Mockito.when(objectMapper.readValue(data2, BulkPriceProductTypeTaggingRequest.class))
      .thenReturn(bulkPriceProductTypeTaggingRequest1);
    bulkProductTypeTaggingUpdateService.setFinalStatusAndGenerateFailedExcelForBulkProductTypeUpdate(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
  }


  @Test
  public void setFinalStatusAndGenerateFailedExcelEmptyListTest() throws Exception {
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
      .thenReturn(Collections.emptyList());
    Mockito.when(objectMapper.readValue(data1, BulkPriceProductTypeTaggingRequest.class))
      .thenReturn(bulkPriceProductTypeTaggingRequest);
    Mockito.when(objectMapper.readValue(data2, BulkPriceProductTypeTaggingRequest.class))
      .thenReturn(bulkPriceProductTypeTaggingRequest1);
    bulkProductTypeTaggingUpdateService.setFinalStatusAndGenerateFailedExcelForBulkProductTypeUpdate(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
  }


  @Test
  public void setFinalStatusAndGenerateFailedExcelPartiallyCompletedTest() throws Exception {
    bulkInternalProcessData1.setErrorMessage(ERROR_MESSAGE);
    bulkInternalProcessData1.setStatus(FAILED);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
      .thenReturn(Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2));
    Mockito.when(objectMapper.readValue(data1, BulkPriceProductTypeTaggingRequest.class))
      .thenReturn(bulkPriceProductTypeTaggingRequest);
    Mockito.when(objectMapper.readValue(data2, BulkPriceProductTypeTaggingRequest.class))
      .thenReturn(bulkPriceProductTypeTaggingRequest1);
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any())).thenReturn(bulkInternalProcess);
    bulkProductTypeTaggingUpdateService.setFinalStatusAndGenerateFailedExcelForBulkProductTypeUpdate(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Mockito.verify(fileStorageService)
      .downloadFileAndGenerateErrorFile(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyList() , anyString());
    Mockito.verify(mailDeliveryService).sendEmail(Mockito.any(), anyMap());
    Mockito.verify(objectMapper).readValue(data1, BulkPriceProductTypeTaggingRequest.class);
    Mockito.verify(objectMapper).readValue(data2, BulkPriceProductTypeTaggingRequest.class);
    Assertions.assertEquals(PARTIAL_COMPLETED, bulkInternalProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(1, bulkInternalProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(1, bulkInternalProcessArgumentCaptor.getValue().getErrorCount(), 0);
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelFailTest() throws Exception {
    bulkInternalProcessData1.setErrorMessage(ERROR_MESSAGE);
    bulkInternalProcessData1.setStatus(FAILED);
    bulkInternalProcessData2.setStatus(FAILED);
    bulkInternalProcessData2.setErrorMessage(ERROR_MESSAGE);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
      .thenReturn(Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2));
    Mockito.when(objectMapper.readValue(data1, BulkPriceProductTypeTaggingRequest.class))
      .thenReturn(bulkPriceProductTypeTaggingRequest);
    Mockito.when(objectMapper.readValue(data2, BulkPriceProductTypeTaggingRequest.class))
      .thenReturn(bulkPriceProductTypeTaggingRequest1);
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any())).thenReturn(bulkInternalProcess);
    bulkProductTypeTaggingUpdateService.setFinalStatusAndGenerateFailedExcelForBulkProductTypeUpdate(STORE_ID, bulkInternalProcess);
    Mockito.verify(internalProcessService)
      .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID);
    Mockito.verify(internalProcessService).saveInternalProcess(bulkInternalProcessArgumentCaptor.capture());
    Mockito.verify(fileStorageService)
      .downloadFileAndGenerateErrorFile(Mockito.anyString(), Mockito.anyString(),
        Mockito.anyList(), anyString());
    Mockito.verify(mailDeliveryService).sendEmail(Mockito.any(), anyMap());
    Mockito.verify(objectMapper).readValue(data1, BulkPriceProductTypeTaggingRequest.class);
    Mockito.verify(objectMapper).readValue(data2, BulkPriceProductTypeTaggingRequest.class);
    Assertions.assertEquals(FAILED, bulkInternalProcessArgumentCaptor.getValue().getStatus());
    Assertions.assertEquals(0, bulkInternalProcessArgumentCaptor.getValue().getSuccessCount(), 0);
    Assertions.assertEquals(2, bulkInternalProcessArgumentCaptor.getValue().getErrorCount(), 0);
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelFailedTest() throws Exception {
    bulkInternalProcessData1.setErrorMessage(ERROR_MESSAGE);
    bulkInternalProcessData2.setErrorMessage(ERROR_MESSAGE);
    bulkInternalProcessData1.setStatus(FAILED);
    bulkInternalProcessData2.setStatus(FAILED);
    Mockito.when(objectMapper.readValue(data1, BulkPriceProductTypeTaggingRequest.class))
      .thenReturn(bulkPriceProductTypeTaggingRequest);
    Mockito.when(objectMapper.readValue(data2, BulkPriceProductTypeTaggingRequest.class))
      .thenReturn(bulkPriceProductTypeTaggingRequest1);
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any())).thenReturn(bulkInternalProcess);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
      .thenReturn(Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2));
    bulkProductTypeTaggingUpdateService.sendEmailNotificationForBulkProductTypeUpdate(
      "temp", "subject", "requestId", "username", "1","1", "error", "fileName");
    Mockito.verify(mailDeliveryService).sendEmail(Mockito.any(), Mockito.anyMap());
  }

  @Test
  public void setFinalStatusAndGenerateInProgressTest() throws Exception {
    bulkInternalProcessData1.setErrorMessage(ERROR_MESSAGE);
    bulkInternalProcessData2.setErrorMessage(ERROR_MESSAGE);
    bulkInternalProcessData1.setStatus(FAILED);
    bulkInternalProcessData2.setStatus(ERROR_MESSAGE);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, ID))
      .thenReturn(Arrays.asList(bulkInternalProcessData1, bulkInternalProcessData2));
    bulkProductTypeTaggingUpdateService.sendEmailNotificationForBulkProductTypeUpdate(
      "temp", "subject", "requestId", "username", "1","1", "error", "fileName");
    Mockito.verify(mailDeliveryService).sendEmail(Mockito.any(), Mockito.anyMap());
  }

}