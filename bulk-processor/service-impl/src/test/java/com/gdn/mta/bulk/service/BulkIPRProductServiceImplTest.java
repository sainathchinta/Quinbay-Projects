package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.BulkInternalProcessType;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.models.download.responsedata.BulkAddReviewIPRProductsRequestData;
import com.gdn.mta.bulk.models.download.responsedata.BulkMasterSkuReviewRequestData;
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

public class BulkIPRProductServiceImplTest {

  @InjectMocks
  private BulkIPRProductServiceImpl bulkIPRProductService;

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
  private static final String PRODUCT_NAME = "productName";
  private static final String PRODUCT_SKU = "productSku";
  private static final String ACTION = "IN_REVIEW";

  private BulkInternalProcess bulkInternalProcess;
  private BulkInternalProcessData bulkInternalProcessData1;
  private BulkInternalProcessData bulkInternalProcessData2;
  private BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData;
  private BulkMasterSkuReviewRequestData bulKMasterSkuReviewRequestData;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setProcessType(BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.getValue());
    bulkInternalProcess.setId(ID);
    bulkInternalProcess.setTotalCount(2);
    bulkInternalProcess.setInternalProcessRequestCode(ID);
    bulkInternalProcess.setCreatedBy(USERNAME);
    bulkInternalProcess.setFileName(USERNAME);
    bulkInternalProcess.setErrorFilePath(USERNAME);
    bulkInternalProcessData1 = new BulkInternalProcessData();
    bulkInternalProcessData2 = new BulkInternalProcessData();
    ReflectionTestUtils.setField(bulkIPRProductService, "iprProductAddReviewErrorFilePath",
        ID);
    bulkAddReviewIPRProductsRequestData = new BulkAddReviewIPRProductsRequestData();
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
    bulkAddReviewIPRProductsRequestData.setExcelRowNumber(1);
    bulkAddReviewIPRProductsRequestData.setProductName(PRODUCT_NAME);
    bulkAddReviewIPRProductsRequestData.setProductSku(PRODUCT_SKU);
    bulkAddReviewIPRProductsRequestData.setAction(ACTION);
    String message = new ObjectMapper().writeValueAsString(bulkAddReviewIPRProductsRequestData);
    bulkInternalProcessData1.setStatus(ProcessStatus.FAILED.name());
    bulkInternalProcessData1.setData(message);
    Mockito.when(internalProcessService.saveInternalProcess(Mockito.any()))
        .thenReturn(bulkInternalProcess);
    Mockito.when(
        internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
            STORE_ID, ID)).thenReturn(Arrays.asList(bulkInternalProcessData1));
    Mockito.when(objectMapper.readValue(message, BulkAddReviewIPRProductsRequestData.class))
        .thenReturn(bulkAddReviewIPRProductsRequestData);
    bulkIPRProductService.setFinalStatusAndGenerateFailedExcelForBulkAddReviewIPRProducts(STORE_ID,
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
    Mockito.verify(objectMapper).readValue(message, BulkAddReviewIPRProductsRequestData.class);
    Mockito.verify(fileStorageService)
        .downloadFileAndGenerateErrorFile(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyList(), Mockito.any());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelPartialSuccessTest() throws Exception {
    bulkAddReviewIPRProductsRequestData.setExcelRowNumber(1);
    bulkAddReviewIPRProductsRequestData.setProductName(PRODUCT_NAME);
    bulkAddReviewIPRProductsRequestData.setProductSku(PRODUCT_SKU);
    bulkAddReviewIPRProductsRequestData.setAction(ACTION);
    BulkAddReviewIPRProductsRequestData bulkAddReviewIPRProductsRequestData2 =
        new BulkAddReviewIPRProductsRequestData();
    bulkAddReviewIPRProductsRequestData2.setExcelRowNumber(2);
    bulkAddReviewIPRProductsRequestData2.setProductName(PRODUCT_NAME);
    bulkAddReviewIPRProductsRequestData2.setProductSku(PRODUCT_SKU);
    bulkAddReviewIPRProductsRequestData2.setAction(ACTION);
    String message1 = new ObjectMapper().writeValueAsString(bulkAddReviewIPRProductsRequestData);
    String message2 =
        new ObjectMapper().writeValueAsString(bulkAddReviewIPRProductsRequestData2);
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
    Mockito.when(objectMapper.readValue(message1, BulkAddReviewIPRProductsRequestData.class))
        .thenReturn(bulkAddReviewIPRProductsRequestData);
    Mockito.when(objectMapper.readValue(message2, BulkAddReviewIPRProductsRequestData.class))
        .thenReturn(bulkAddReviewIPRProductsRequestData2);
    bulkIPRProductService.setFinalStatusAndGenerateFailedExcelForBulkAddReviewIPRProducts(STORE_ID,
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
    Mockito.verify(objectMapper).readValue(message1, BulkAddReviewIPRProductsRequestData.class);
    Mockito.verify(objectMapper).readValue(message2, BulkAddReviewIPRProductsRequestData.class);
    Mockito.verify(fileStorageService)
        .downloadFileAndGenerateErrorFile(Mockito.anyString(), Mockito.anyString(),
            Mockito.anyList(), Mockito.any());
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelForBulkIPRProductsTest() throws Exception {
    bulkAddReviewIPRProductsRequestData.setExcelRowNumber(1);
    String message = new ObjectMapper().writeValueAsString(bulKMasterSkuReviewRequestData);
    bulkInternalProcessData1.setProcessType(BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    bulkInternalProcess.setProcessType(BulkInternalProcessType.IPR_PORTAL_BULK_ADD_REVIEW.name());
    bulkInternalProcess.setTotalCount(1);
    bulkInternalProcessData1.setStatus(COMPLETED);
    bulkInternalProcessData1.setData(message);
    Mockito.when(internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID,
        bulkInternalProcess.getId())).thenReturn(Collections.singletonList(bulkInternalProcessData1));
    Mockito.when(objectMapper.readValue(message, BulkAddReviewIPRProductsRequestData.class))
        .thenReturn(bulkAddReviewIPRProductsRequestData);
    bulkIPRProductService.setFinalStatusAndGenerateFailedExcelForBulkAddReviewIPRProducts(STORE_ID,
        bulkInternalProcess);
    Mockito.verify(mailDeliveryService).sendEmail(Mockito.any(), Mockito.anyMap());
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID, bulkInternalProcess.getId());
    Mockito.verify(internalProcessService).saveInternalProcess(Mockito.any());
    Mockito.verify(objectMapper).readValue(message, BulkAddReviewIPRProductsRequestData.class);
  }

  @Test
  public void setFinalStatusAndGenerateFailedExcelForIPRProductsPartialCompletedTest() throws Exception {
    bulkInternalProcessData1.setStatus(PARTIAL_COMPLETED);
    Mockito.when(
            internalProcessService.getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(
                STORE_ID, bulkInternalProcess.getId()))
        .thenReturn(Collections.singletonList(bulkInternalProcessData1));
    bulkIPRProductService.setFinalStatusAndGenerateFailedExcelForBulkAddReviewIPRProducts(STORE_ID,
        bulkInternalProcess);
    Mockito.verify(internalProcessService)
        .getAllBulkInternalProcessDataByStoreIdAndInternalProcessRequestId(STORE_ID,
            bulkInternalProcess.getId());
  }
}
