package com.gdn.mta.bulk.listener.kafka;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.InternalBulkUploadDataDomainEventModel;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;
import com.gdn.partners.bulk.util.Constant;

public class BulkVendorActionsListenerTest {
  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private InternalProcessServiceWrapper internalProcessServiceWrapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @InjectMocks
  private BulkVendorActionsListener bulkVendorActionsListener;

  private InternalBulkUploadDataDomainEventModel internalBulkUploadDataDomainEventModel;
  private String message;

  @BeforeEach
  public void init() throws JsonProcessingException {
    initMocks(this);
    internalBulkUploadDataDomainEventModel =
      InternalBulkUploadDataDomainEventModel.builder().storeId(Constant.STORE_ID)
        .updatedBy(Constant.USER_NAME).processType(Constant.INSTORE_BULK_PROCESS_TYPE)
        .internalProcessDataRequestId(Constant.ID).build();
    message = new ObjectMapper().writeValueAsString(internalBulkUploadDataDomainEventModel);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(internalProcessServiceWrapper);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedException_Test() throws Exception {
    Mockito.when(
        objectMapper.readValue(Constant.CLIENT_ID, InternalBulkUploadDataDomainEventModel.class))
      .thenThrow(JsonProcessingException.class);
    bulkVendorActionsListener.onDomainEventConsumed(Constant.CLIENT_ID);
    verify(objectMapper).readValue(Constant.CLIENT_ID,
      InternalBulkUploadDataDomainEventModel.class);
    verify(kafkaTopicProperties, times(2)).getBulkApprovalRejectionProcessEvent();
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(message, InternalBulkUploadDataDomainEventModel.class))
      .thenReturn(internalBulkUploadDataDomainEventModel);
    bulkVendorActionsListener.onDomainEventConsumed(message);
    verify(internalProcessServiceWrapper).processBulkVendorActionsEvent(
      internalBulkUploadDataDomainEventModel.getStoreId(),
      internalBulkUploadDataDomainEventModel.getProcessType(),
      internalBulkUploadDataDomainEventModel.getInternalProcessDataRequestId());
    verify(objectMapper).readValue(message, InternalBulkUploadDataDomainEventModel.class);
    verify(kafkaTopicProperties).getBulkApprovalRejectionProcessEvent();
  }
}

