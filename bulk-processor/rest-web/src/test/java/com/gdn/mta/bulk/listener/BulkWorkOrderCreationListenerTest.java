package com.gdn.mta.bulk.listener;


import com.gdn.mta.bulk.entity.BulkProcess;
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
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.service.BulkProcessService;

public class BulkWorkOrderCreationListenerTest {

  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String MESSAGE = "message";

  @InjectMocks
  private BulkWorkOrderCreationListener bulkWorkOrderCreationListener;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(bulkProcessService, objectMapper, kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setBulkProcessCode(BULK_PROCESS_CODE);
    BulkProcess bulkProcess = new BulkProcess();
    Mockito.when(objectMapper.readValue(MESSAGE, BulkUpdateQueue.class)).thenReturn(bulkUpdateQueue);
    Mockito.when(bulkProcessService.validateAndUpdateWorkOrder(bulkUpdateQueue)).thenReturn(bulkProcess);
    Mockito.doNothing().when(bulkProcessService).processWorkOrder(bulkUpdateQueue, bulkProcess);
    bulkWorkOrderCreationListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, BulkUpdateQueue.class);
    Mockito.verify(bulkProcessService).processWorkOrder(bulkUpdateQueue, bulkProcess);
    Mockito.verify(bulkProcessService).validateAndUpdateWorkOrder(bulkUpdateQueue);
    Mockito.verify(kafkaTopicProperties).getWorkOrderEvent();
  }

  @Test
  public void onDomainEventConsumedErrorTest() throws Exception {
    BulkUpdateQueue bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setBulkProcessCode(BULK_PROCESS_CODE);
    BulkProcess bulkProcess = new BulkProcess();
    Mockito.when(objectMapper.readValue(MESSAGE, BulkUpdateQueue.class)).thenReturn(bulkUpdateQueue);
    Mockito.when(bulkProcessService.validateAndUpdateWorkOrder(bulkUpdateQueue)).thenReturn(bulkProcess);
    Mockito.doThrow(new ApplicationRuntimeException()).when(bulkProcessService).processWorkOrder(bulkUpdateQueue, bulkProcess);
    try {
      bulkWorkOrderCreationListener.onDomainEventConsumed(MESSAGE);
    } finally {
      Mockito.verify(objectMapper).readValue(MESSAGE, BulkUpdateQueue.class);
      Mockito.verify(bulkProcessService).processWorkOrder(bulkUpdateQueue, bulkProcess);
      Mockito.verify(bulkProcessService).validateAndUpdateWorkOrder(bulkUpdateQueue);
      Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getWorkOrderEvent();
    }
  }
}