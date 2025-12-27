package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.InternalBulkUploadDataDomainEventModel;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import static org.mockito.Mockito.verifyNoMoreInteractions;

public class BulkAssignAutoApprovedProductsListenerTest {

  @Mock
  private InternalProcessServiceWrapper internalProcessServiceWrapper;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @InjectMocks
  private BulkAssignAutoApprovedProductsListener listener;

  private static final String STORE_ID = "10001";
  private static final String INTERNAL_PROCESS_REQUEST_ID = "id";
  private InternalBulkUploadDataDomainEventModel internalBulkUploadDataDomainEventModel;
  private String json;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    internalBulkUploadDataDomainEventModel = new InternalBulkUploadDataDomainEventModel();
    internalBulkUploadDataDomainEventModel.setStoreId(STORE_ID);
    internalBulkUploadDataDomainEventModel.setInternalProcessDataRequestId(
        INTERNAL_PROCESS_REQUEST_ID);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(internalProcessServiceWrapper);
    verifyNoMoreInteractions(kafkaTopicProperties);
    verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(json, InternalBulkUploadDataDomainEventModel.class))
        .thenReturn(internalBulkUploadDataDomainEventModel);
    Mockito.doNothing().when(internalProcessServiceWrapper)
        .processAutoApprovedProductsBulkAssignEvent(STORE_ID, INTERNAL_PROCESS_REQUEST_ID);
    listener.onDomainEventConsumed(json);
    Mockito.verify(objectMapper).readValue(json, InternalBulkUploadDataDomainEventModel.class);
    Mockito.verify(internalProcessServiceWrapper)
        .processAutoApprovedProductsBulkAssignEvent(STORE_ID, INTERNAL_PROCESS_REQUEST_ID);
    Mockito.verify(kafkaTopicProperties).getAutoApprovedProductsBulkAssignProcessEvent();
  }

  @Test
  public void onDomainEventConsumed_exceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(json, InternalBulkUploadDataDomainEventModel.class))
        .thenThrow(JsonProcessingException.class);
    listener.onDomainEventConsumed(json);
    Mockito.verify(objectMapper).readValue(json, InternalBulkUploadDataDomainEventModel.class);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2))
        .getAutoApprovedProductsBulkAssignProcessEvent();
  }
}
