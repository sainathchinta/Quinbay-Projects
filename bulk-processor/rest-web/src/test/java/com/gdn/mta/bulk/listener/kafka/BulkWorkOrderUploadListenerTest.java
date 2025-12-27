package com.gdn.mta.bulk.listener.kafka;

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
import com.gdn.mta.bulk.entity.WorkOrderEventModel;
import com.gdn.mta.bulk.service.BulkWorkOrderServiceWrapper;
import com.gdn.partners.bulk.util.Constant;

public class BulkWorkOrderUploadListenerTest {
  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private BulkWorkOrderServiceWrapper bulkWorkOrderServiceWrapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @InjectMocks
  private BulkWorkOrderUploadListener bulkWorkOrderUploadListener;

  private WorkOrderEventModel workOrderEventModel;
  private String message;

  @BeforeEach
  public void init() throws JsonProcessingException {
    initMocks(this);
    workOrderEventModel =
        WorkOrderEventModel.builder().id(Constant.ID).bulkProcessCode(Constant.CATEGORY_CODE).bulkProcessId(Constant.ID)
            .storeId(Constant.STORE_ID).build();
    message = new ObjectMapper().writeValueAsString(workOrderEventModel);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(bulkWorkOrderServiceWrapper);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumedException_Test() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, WorkOrderEventModel.class))
        .thenThrow(JsonProcessingException.class);
    bulkWorkOrderUploadListener.onDomainEventConsumed(Constant.CLIENT_ID);
    verify(objectMapper).readValue(Constant.CLIENT_ID, WorkOrderEventModel.class);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(message, WorkOrderEventModel.class)).thenReturn(workOrderEventModel);
    bulkWorkOrderUploadListener.onDomainEventConsumed(message);
    verify(bulkWorkOrderServiceWrapper).processBulkWorkOrderUpload(workOrderEventModel);
    verify(objectMapper).readValue(message, WorkOrderEventModel.class);
    verify(kafkaTopicProperties).getBulkWorkOrderUploadEvent();
  }
}
