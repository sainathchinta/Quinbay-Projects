package com.gdn.mta.bulk.listener.kafka;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.InternalBulkUploadDataDomainEventModel;
import com.gdn.mta.bulk.service.BrandUpdateService;

public class BrandUpdateEventListenerTest {

  private static final String MESSAGE = "message";
  private static final String STORE_ID = "storeId";
  private static final String PROCESS_TYPE = "processType";
  private static final String ID = "id";

  private InternalBulkUploadDataDomainEventModel internalProcessDataDomainEventModel;

  @InjectMocks
  private BrandUpdateEventListener brandUpdateEventListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private BrandUpdateService brandUpdateService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    internalProcessDataDomainEventModel = new InternalBulkUploadDataDomainEventModel();
    internalProcessDataDomainEventModel.setStoreId(STORE_ID);
    internalProcessDataDomainEventModel.setProcessType(PROCESS_TYPE);
    internalProcessDataDomainEventModel.setInternalProcessDataRequestId(ID);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(brandUpdateService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, InternalBulkUploadDataDomainEventModel.class))
        .thenReturn(internalProcessDataDomainEventModel);
    brandUpdateEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.brandUpdateService).processBrandUpdateEvent(STORE_ID, PROCESS_TYPE, ID);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, InternalBulkUploadDataDomainEventModel.class);
    Mockito.verify(kafkaTopicProperties).getBrandUpdateEvent();
  }

  @Test
  public void onDomainEventConsumed_exceptionOnUploadTest() throws Exception {
    Mockito.when(this.objectMapper.readValue(MESSAGE, InternalBulkUploadDataDomainEventModel.class))
        .thenReturn(internalProcessDataDomainEventModel);
    Mockito.doThrow(RuntimeException.class).when(this.brandUpdateService).processBrandUpdateEvent(STORE_ID, PROCESS_TYPE, ID);
    brandUpdateEventListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(this.brandUpdateService).processBrandUpdateEvent(STORE_ID, PROCESS_TYPE, ID);
    Mockito.verify(this.objectMapper).readValue(MESSAGE, InternalBulkUploadDataDomainEventModel.class);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBrandUpdateEvent();
  }

}
