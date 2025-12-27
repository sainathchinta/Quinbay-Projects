package com.gdn.mta.bulk.listener.kafka;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

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
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.mta.bulk.service.BulkUpdateService;
import com.gdn.partners.bulk.util.Constant;

public class BulkUpdateOff2OnListenerTest {
  private static final String BULK_PROCESS_CODE = "bp-code";
  private static final String BULK_PROCESS_TYPE = "ProductLevel3";
  private static final String SERVICE_BEAN_NAME = "BulkUpdateService";
  private static final String DEFAULT_STORE_ID = "10001";

  private BulkUpdateQueue bulkUpdateQueue;

  @Mock
  private AutowireCapableBeanFactory autowireCapableBeanFactory;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private BulkUpdateService bulkUpdateService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @InjectMocks
  private BulkUpdateOff2OnListener bulkUpdateOff2OnListener;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkUpdateQueue.setBulkProcessType(BULK_PROCESS_TYPE);
    bulkUpdateQueue.setStoreId(DEFAULT_STORE_ID);
    when(autowireCapableBeanFactory.getBean(anyString())).thenReturn(bulkUpdateService);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(autowireCapableBeanFactory);
    Mockito.verifyNoMoreInteractions(bulkProcessService);
    Mockito.verifyNoMoreInteractions(bulkUpdateService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkUpdateQueue.class)).thenReturn(bulkUpdateQueue);
    bulkUpdateOff2OnListener.onDomainEventConsumed(Constant.CLIENT_ID);
    verify(autowireCapableBeanFactory).getBean(BULK_PROCESS_TYPE + SERVICE_BEAN_NAME);
    verify(bulkUpdateService).processBulkUpdateOff2On(bulkUpdateQueue);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkUpdateQueue.class);
    Mockito.verify(kafkaTopicProperties).getBulkUpdateOff2On();
  }

  @Test
  public void onDomainEventExceptionConsumed() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkUpdateQueue.class)).thenReturn(bulkUpdateQueue);
    Mockito.doThrow(Exception.class).when(bulkUpdateService).processBulkUpdateOff2On(bulkUpdateQueue);
    bulkUpdateOff2OnListener.onDomainEventConsumed(Constant.CLIENT_ID);
    verify(autowireCapableBeanFactory).getBean(BULK_PROCESS_TYPE + SERVICE_BEAN_NAME);
    verify(bulkUpdateService).processBulkUpdateOff2On(bulkUpdateQueue);
    verify(bulkProcessService).abortBulkProcess(DEFAULT_STORE_ID, BULK_PROCESS_CODE);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkUpdateQueue.class);
    Mockito.verify(kafkaTopicProperties).getBulkUpdateOff2On();
  }
}