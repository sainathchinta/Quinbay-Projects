package com.gdn.mta.bulk.listener.kafka;

import java.util.Map;
import java.util.UUID;

import org.apache.commons.collections.map.HashedMap;
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
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.entity.BulkProcessQueue;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.mta.bulk.service.ProcessorService;
import com.gdn.partners.bulk.util.Constant;

public class BulkProcessPriorityQueueListenerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_BULK_PROCESS_CODE = UUID.randomUUID().toString();

  @InjectMocks
  private BulkProcessPriorityQueueListener listener;

  @Mock
  private AutowireCapableBeanFactory autowireCapableBeanFactory;

  @Mock
  private BulkProcessService bulkProcessService;

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
    Mockito.verifyNoMoreInteractions(autowireCapableBeanFactory, bulkProcessService, kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    BulkProcessQueue bulkProcessQueue =
        new BulkProcessQueue(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE, BulkProcessType.PRODUCT_LEVEL_3.getValue(),
            null);
    ProcessorService processorService = Mockito.mock(ProcessorService.class);
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkProcessQueue.class)).thenReturn(bulkProcessQueue);
    Mockito.when(autowireCapableBeanFactory.getBean(bulkProcessQueue.getBulkProcessType() + "ProcessorService"))
        .thenReturn(processorService);
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(autowireCapableBeanFactory).getBean(bulkProcessQueue.getBulkProcessType() + "ProcessorService");
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkProcessQueue.class);
    Mockito.verify(kafkaTopicProperties).getBulkCreatePriorityEvent();
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Map<String, String> args = new HashedMap();
    BulkProcessQueue bulkProcessQueue =
        new BulkProcessQueue(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE, BulkProcessType.PRODUCT_LEVEL_3.getValue(),
            args);
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkProcessQueue.class)).thenReturn(bulkProcessQueue);
    Mockito.when(autowireCapableBeanFactory.getBean(bulkProcessQueue.getBulkProcessType() + "ProcessorService"))
        .thenThrow(ApplicationRuntimeException.class);
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(autowireCapableBeanFactory).getBean(bulkProcessQueue.getBulkProcessType() + "ProcessorService");
    Mockito.verify(bulkProcessService).abortBulkProcess(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkProcessQueue.class);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkCreatePriorityEvent();
  }

}
