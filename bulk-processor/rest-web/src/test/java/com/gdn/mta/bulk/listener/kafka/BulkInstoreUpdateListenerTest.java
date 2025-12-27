package com.gdn.mta.bulk.listener.kafka;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.service.BulkInstoreUpdateService;

public class BulkInstoreUpdateListenerTest {

  @InjectMocks
  private BulkInstoreUpdateListener bulkInstoreUpdateListener;

  @Mock
  private BulkInstoreUpdateService bulkInstoreUpdateService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(bulkInstoreUpdateService, kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    Mockito.doNothing().when(bulkInstoreUpdateService).processInstoreUpdateEvent(new BulkUpdateEventModel());
    Mockito.when(objectMapper.readValue("", BulkUpdateEventModel.class)).thenReturn(new BulkUpdateEventModel());
    bulkInstoreUpdateListener.onDomainEventConsumed("");
    Mockito.verify(bulkInstoreUpdateService).processInstoreUpdateEvent(new BulkUpdateEventModel());
    Mockito.verify(kafkaTopicProperties).getBulkUploadInstoreUpdate();
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(bulkInstoreUpdateService)
        .processInstoreUpdateEvent(new BulkUpdateEventModel());
    Mockito.when(objectMapper.readValue("", BulkUpdateEventModel.class)).thenReturn(new BulkUpdateEventModel());
    bulkInstoreUpdateListener.onDomainEventConsumed("");
    Mockito.verify(bulkInstoreUpdateService).processInstoreUpdateEvent(new BulkUpdateEventModel());
    Mockito.verify(kafkaTopicProperties).getBulkUploadInstoreUpdate();
  }

}
