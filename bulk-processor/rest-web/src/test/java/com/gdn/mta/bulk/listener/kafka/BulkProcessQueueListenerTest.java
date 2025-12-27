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
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkProcessType;
import com.gdn.mta.bulk.dto.product.constant.TrackerConstants;
import com.gdn.mta.bulk.entity.BulkProcessQueue;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.mta.bulk.service.ProcessorService;
import com.gdn.mta.bulk.service.TrackerService;
import com.gdn.partners.bulk.util.Constant;

/**
 * Created by hardikbohra on 29/05/18.
 */
public class BulkProcessQueueListenerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_BULK_PROCESS_CODE = UUID.randomUUID().toString();

  @InjectMocks
  private BulkProcessQueueListener listener;

  @Mock
  private AutowireCapableBeanFactory autowireCapableBeanFactory;

  @Mock
  private TrackerService trackerService;

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
    Mockito.verifyNoMoreInteractions(autowireCapableBeanFactory, trackerService, bulkProcessService,
        kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @SuppressWarnings("rawtypes")
  @Test
  public void testListen() throws Exception {
    BulkProcessQueue bulkProcessQueue = getBulkProcessQueue();
    ProcessorService processorService = Mockito.mock(ProcessorService.class);
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkProcessQueue.class)).thenReturn(bulkProcessQueue);
    Mockito.when(autowireCapableBeanFactory.getBean(bulkProcessQueue.getBulkProcessType() + "ProcessorService"))
        .thenReturn(processorService);
    listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(autowireCapableBeanFactory).getBean(bulkProcessQueue.getBulkProcessType() + "ProcessorService");
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkProcessQueue.class);
    Mockito.verify(kafkaTopicProperties).getBulkCreateEvent();
  }

  @SuppressWarnings("rawtypes")
  @Test
  public void testListenWhenError() throws Exception {
    BulkProcessQueue bulkProcessQueue = getBulkProcessQueue();
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkProcessQueue.class)).thenReturn(bulkProcessQueue);
    try{
      listener.onDomainEventConsumed(Constant.CLIENT_ID);
    } catch(Exception e){
      Mockito.verify(autowireCapableBeanFactory).getBean(bulkProcessQueue.getBulkProcessType() + "ProcessorService");
      Mockito.verify(bulkProcessService).abortBulkProcess(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
      Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkProcessQueue.class);
      Mockito.verify(kafkaTopicProperties).getBulkCreateEvent();
    }
  }

  @SuppressWarnings("rawtypes")
  @Test
  public void testListenWhenError_WhenTypeIsRecat() throws Exception {
    Map<String, String> args = new HashedMap();
    BulkProcessQueue bulkProcessQueue = new BulkProcessQueue(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE,
        BulkProcessType.RECATEGORIZATION.getValue(), args);
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkProcessQueue.class)).thenReturn(bulkProcessQueue);
    try{
      listener.onDomainEventConsumed(Constant.CLIENT_ID);
    } catch(Exception e){
      Mockito.verify(autowireCapableBeanFactory).getBean(bulkProcessQueue.getBulkProcessType() + "ProcessorService");
      Mockito.verify(bulkProcessService).abortBulkProcess(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
      Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkProcessQueue.class);
      Mockito.verify(kafkaTopicProperties).getBulkCreateEvent();
    }
  }

  @SuppressWarnings("rawtypes")
  @Test
  public void testListenWhenError_WhenTypeIsProductLevel3() throws Exception {
    Map<String, String> args = new HashedMap();
    BulkProcessQueue bulkProcessQueue = new BulkProcessQueue(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE,
        BulkProcessType.PRODUCT_LEVEL_3.getValue(), args);
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkProcessQueue.class)).thenReturn(bulkProcessQueue);
    try{
      listener.onDomainEventConsumed(Constant.CLIENT_ID);
    } catch(Exception e){
      Mockito.verify(autowireCapableBeanFactory).getBean(bulkProcessQueue.getBulkProcessType() + "ProcessorService");
      Mockito.verify(trackerService).sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants
              .CREATE_FLOW1_WEB, TrackerConstants.SUBMIT, TrackerConstants.FAILED, null);
      Mockito.verify(bulkProcessService).abortBulkProcess(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE);
      Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkProcessQueue.class);
      Mockito.verify(kafkaTopicProperties).getBulkCreateEvent();
    }
  }

  private BulkProcessQueue getBulkProcessQueue() {
    return new BulkProcessQueue(DEFAULT_STORE_ID, DEFAULT_BULK_PROCESS_CODE,
        BulkProcessType.PRODUCT_LEVEL_3.getValue(), null);
  }
}
