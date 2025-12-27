package com.gdn.mta.bulk.listener.kafka;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.InternalBulkUploadDataDomainEventModel;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;
import com.gdn.partners.bulk.util.Constant;

public class BulkInternalPriceUpdateListenerTest {

  @InjectMocks
  private BulkInternalPriceUpdateListener bulkInternalPriceUpdateListener;

  @Mock
  private InternalProcessServiceWrapper internalProcessServiceWrapper;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  private ObjectMapper mapper = new ObjectMapper();
  private InternalBulkUploadDataDomainEventModel internalBulkUploadDataDomainEventModel =
      new InternalBulkUploadDataDomainEventModel();

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    internalBulkUploadDataDomainEventModel.setStoreId(Constant.STORE_ID);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(internalProcessServiceWrapper);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(mapper.writeValueAsString(internalBulkUploadDataDomainEventModel),
        InternalBulkUploadDataDomainEventModel.class)).thenReturn(internalBulkUploadDataDomainEventModel);
    bulkInternalPriceUpdateListener.onDomainEventConsumed(
        mapper.writeValueAsString(internalBulkUploadDataDomainEventModel));
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(internalBulkUploadDataDomainEventModel),
        InternalBulkUploadDataDomainEventModel.class);
    Mockito.verify(kafkaTopicProperties).getBulkInternalPriceUpdateEvent();
    Mockito.verify(internalProcessServiceWrapper)
        .processBulkInternalBulkPriceUpdate(internalBulkUploadDataDomainEventModel, false);
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.doThrow(JsonParseException.class).when(objectMapper)
        .readValue(mapper.writeValueAsString(internalBulkUploadDataDomainEventModel),
            InternalBulkUploadDataDomainEventModel.class);
    bulkInternalPriceUpdateListener.onDomainEventConsumed(
        mapper.writeValueAsString(internalBulkUploadDataDomainEventModel));
    Mockito.verify(objectMapper).readValue(mapper.writeValueAsString(internalBulkUploadDataDomainEventModel),
        InternalBulkUploadDataDomainEventModel.class);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkInternalPriceUpdateEvent();
  }
}
