package com.gdn.mta.bulk.service;

import static org.mockito.ArgumentMatchers.eq;

import java.util.Arrays;

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

import com.gdn.mta.bulk.config.KafkaPublisher;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.ProcessStatus;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.BulkInternalProcessData;
import com.gdn.mta.bulk.models.InternalBulkUploadDataDomainEventModel;
import com.gdn.mta.bulk.models.ProductBrandUpdateRequest;

public class BrandUpdateServiceImplTest {

  private static final String STORE_ID = "storeId";
  private static final int BATCH_SIZE = 1;
  private static final String ID = "id";
  private static final String PENDING = "PENDING";
  private static final String IN_PROGRESS = "IN_PROGRESS";
  private static final String BULK_INTERNAL_PROCESS_DATA_ID = "bulkInternalProcessDataId";
  private static final String PROCESS_TYPE = "processType";
  private static final String PRODUCT_CODE = "productCode1";
  private static final String OLD_BRAND_CODE = "oldBrandCode1";
  private static final String NEW_BRAND_CODE = "newBrandCode1";
  private static final String MESSAGE = "{\"productCode\":\"productCode1\","
      + "\"oldBrandCode\":\"oldBrandCode1\",\"newBrandCode\":\"newBrandCode1\"}";

  private BulkInternalProcess bulkInternalProcess;
  private BulkInternalProcessData bulkInternalProcessData;

  @Mock
  private InternalProcessService internalProcessService;

  @Mock
  private KafkaPublisher kafkaProducer;

  @InjectMocks
  private BrandUpdateServiceImpl brandUpdateService;

  @Mock
  private PBPOutboundService pbpOutboundService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Captor
  private ArgumentCaptor<BulkInternalProcessData> bulkProcessDataArgumentCaptor;

  @Captor
  private ArgumentCaptor<InternalBulkUploadDataDomainEventModel>
      internalBulkUploadDataDomainEventModelArgumentCaptor;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    bulkInternalProcess = new BulkInternalProcess();
    bulkInternalProcess.setId(ID);
    bulkInternalProcessData = new BulkInternalProcessData();
    bulkInternalProcessData.setInternalProcessRequestId(ID);
    bulkInternalProcessData.setId(BULK_INTERNAL_PROCESS_DATA_ID);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(internalProcessService);
    Mockito.verifyNoMoreInteractions(kafkaProducer);
    Mockito.verifyNoMoreInteractions(pbpOutboundService);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void publishBrandUpdateEventTest() {
    Mockito.when(internalProcessService.getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
        Arrays.asList(ID), PENDING, BATCH_SIZE)).thenReturn(Arrays.asList(bulkInternalProcessData));
    brandUpdateService.publishBrandUpdateEvent(STORE_ID, Arrays.asList(bulkInternalProcess), BATCH_SIZE);
    Mockito.verify(internalProcessService).getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
        Arrays.asList(ID), PENDING, BATCH_SIZE);
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(bulkProcessDataArgumentCaptor.capture());
    Mockito.verify(kafkaProducer).send(eq(kafkaTopicProperties.getBrandUpdateEvent()),
            internalBulkUploadDataDomainEventModelArgumentCaptor.capture());
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBrandUpdateEvent();
    Assertions.assertEquals(bulkProcessDataArgumentCaptor.getValue().getStatus(), IN_PROGRESS);
    Assertions.assertEquals(internalBulkUploadDataDomainEventModelArgumentCaptor.getValue()
        .getInternalProcessDataRequestId(), BULK_INTERNAL_PROCESS_DATA_ID);
  }

  @Test
  public void publishBrandUpdateEventNullTest() {
    brandUpdateService.publishBrandUpdateEvent(STORE_ID, Arrays.asList(bulkInternalProcess), BATCH_SIZE);
    Mockito.verify(internalProcessService).getBulkInternalProcessDataByRequestIdsAndStatus(STORE_ID,
        Arrays.asList(ID), PENDING, BATCH_SIZE);
  }

  @Test
  public void processBrandUpdateEventTest() throws Exception {
    Mockito.when(internalProcessService.getBulkInternalProcessDataById(STORE_ID,
        BULK_INTERNAL_PROCESS_DATA_ID)).thenReturn(bulkInternalProcessData);
    bulkInternalProcessData.setData(MESSAGE);
    brandUpdateService.processBrandUpdateEvent(STORE_ID, PROCESS_TYPE, BULK_INTERNAL_PROCESS_DATA_ID);
    Mockito.verify(internalProcessService).getBulkInternalProcessDataById(STORE_ID,
        BULK_INTERNAL_PROCESS_DATA_ID);
    Mockito.verify(pbpOutboundService).updateBrandOfProduct(STORE_ID, new ProductBrandUpdateRequest(PRODUCT_CODE,
        OLD_BRAND_CODE, NEW_BRAND_CODE));
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(bulkProcessDataArgumentCaptor.capture());
    Assertions.assertEquals(bulkProcessDataArgumentCaptor.getValue().getStatus(), ProcessStatus.COMPLETED.name());
  }

  @Test
  public void processBrandUpdateEventExceptionTest() throws Exception {
    Mockito.when(internalProcessService.getBulkInternalProcessDataById(STORE_ID,
        BULK_INTERNAL_PROCESS_DATA_ID)).thenReturn(bulkInternalProcessData);
    bulkInternalProcessData.setData(MESSAGE);
    Mockito.doThrow(ApplicationRuntimeException.class).when(pbpOutboundService)
        .updateBrandOfProduct(STORE_ID, new ProductBrandUpdateRequest(PRODUCT_CODE, OLD_BRAND_CODE, NEW_BRAND_CODE));
    brandUpdateService.processBrandUpdateEvent(STORE_ID, PROCESS_TYPE, BULK_INTERNAL_PROCESS_DATA_ID);
    Mockito.verify(internalProcessService).getBulkInternalProcessDataById(STORE_ID,
        BULK_INTERNAL_PROCESS_DATA_ID);
    Mockito.verify(pbpOutboundService).updateBrandOfProduct(STORE_ID, new ProductBrandUpdateRequest(PRODUCT_CODE,
        OLD_BRAND_CODE, NEW_BRAND_CODE));
    Mockito.verify(internalProcessService).saveBulkInternalProcessData(bulkProcessDataArgumentCaptor.capture());
    Assertions.assertEquals(bulkProcessDataArgumentCaptor.getValue().getStatus(), ProcessStatus.FAILED.name());
  }

  @Test
  public void processBrandUpdateEventNullTest() {
    brandUpdateService.processBrandUpdateEvent(STORE_ID, PROCESS_TYPE, BULK_INTERNAL_PROCESS_DATA_ID);
    Mockito.verify(internalProcessService).getBulkInternalProcessDataById(STORE_ID,
        BULK_INTERNAL_PROCESS_DATA_ID);
  }

}
