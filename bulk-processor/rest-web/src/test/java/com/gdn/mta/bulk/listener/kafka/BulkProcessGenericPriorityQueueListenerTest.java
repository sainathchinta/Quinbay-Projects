package com.gdn.mta.bulk.listener.kafka;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkProcessQueue;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.mta.bulk.service.ProcessorService;
import com.gdn.partners.bulk.util.Constant;

public class BulkProcessGenericPriorityQueueListenerTest {
  private static final String STORE_ID = "10001";
  private static final String BULK_PROCESS_TYPE = "ProductLevel3Generic";
  private static final String BULK_PROCESS_CODE = "BulkProcessCode";

  private BulkProcessQueue bulkProcessQueue;

  @InjectMocks
  private BulkProcessGenericPriorityQueueListener bulkProcessGenericPriorityQueueListener;

  @Mock
  private AutowireCapableBeanFactory autowireCapableBeanFactory;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    bulkProcessQueue = new BulkProcessQueue();
    bulkProcessQueue.setBulkProcessType(BULK_PROCESS_TYPE);
    bulkProcessQueue.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkProcessQueue.setStoreId(STORE_ID);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(autowireCapableBeanFactory, bulkProcessService, kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkProcessQueue.class)).thenReturn(bulkProcessQueue);
    ProcessorService processorService = Mockito.mock(ProcessorService.class);
    Mockito.when(autowireCapableBeanFactory.getBean(bulkProcessQueue.getBulkProcessType() + "ProcessorService"))
        .thenReturn(processorService);
    bulkProcessGenericPriorityQueueListener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(autowireCapableBeanFactory).getBean(bulkProcessQueue.getBulkProcessType() + "ProcessorService");
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkProcessQueue.class);
    Mockito.verify(kafkaTopicProperties).getBulkGenericCreatePriorityEvent();
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkProcessQueue.class)).thenReturn(bulkProcessQueue);
    Mockito.when(autowireCapableBeanFactory.getBean(bulkProcessQueue.getBulkProcessType() + "ProcessorService"))
        .thenThrow(ApplicationRuntimeException.class);
    bulkProcessGenericPriorityQueueListener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(autowireCapableBeanFactory).getBean(bulkProcessQueue.getBulkProcessType() + "ProcessorService");
    Mockito.verify(bulkProcessService).abortBulkProcess(STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkProcessQueue.class);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkGenericCreatePriorityEvent();
  }
}
