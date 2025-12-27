package com.gdn.mta.bulk.listener.kafka;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.mta.bulk.service.BulkUpdateService;
import com.gdn.partners.bulk.util.Constant;

public class BulkArchiveProductsListenerTest {

  private static final String BULK_PROCESS_CODE = "bp-code";
  private static final String BULK_PROCESS_TYPE = "ProductLevel3";
  private static final String SERVICE_BEAN_NAME = "BulkUpdateService";
  private static final String DEFAULT_STORE_ID = "10001";

  private BulkUpdateQueue bulkUpdateQueue;

  @Mock
  private AutowireCapableBeanFactory autowireCapableBeanFactory;

  @Mock
  private BulkUpdateService bulkUpdateService;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @InjectMocks
  private BulkArchiveProductsListener bulkArchiveProductsListener;


  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    bulkUpdateQueue = new BulkUpdateQueue();
    bulkUpdateQueue.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkUpdateQueue.setBulkProcessType(BULK_PROCESS_TYPE);
    bulkUpdateQueue.setStoreId(DEFAULT_STORE_ID);

    doNothing().when(bulkUpdateService).processBulkArchiveProducts(any(BulkUpdateQueue.class));
    when(autowireCapableBeanFactory.getBean(anyString())).thenReturn(bulkUpdateService);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(autowireCapableBeanFactory);
    verifyNoMoreInteractions(bulkUpdateService);
    verifyNoMoreInteractions(bulkProcessService);
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    when(objectMapper.readValue(Constant.CLIENT_ID, BulkUpdateQueue.class)).thenReturn(bulkUpdateQueue);
    bulkArchiveProductsListener.onDomainEventConsumed(Constant.CLIENT_ID);
    verify(autowireCapableBeanFactory).getBean(BULK_PROCESS_TYPE + SERVICE_BEAN_NAME);
    verify(bulkUpdateService).processBulkArchiveProducts(bulkUpdateQueue);
    verify(objectMapper).readValue(Constant.CLIENT_ID, BulkUpdateQueue.class);
    verify(kafkaTopicProperties).getBulkArchiveProducts();
  }

  @Test
  public void onDomainEventConsumedError() throws Exception {
    when(objectMapper.readValue(Constant.CLIENT_ID, BulkUpdateQueue.class)).thenReturn(bulkUpdateQueue);
    doThrow(Exception.class).when(bulkUpdateService).processBulkArchiveProducts(any(BulkUpdateQueue.class));
    bulkArchiveProductsListener.onDomainEventConsumed(Constant.CLIENT_ID);
    verify(autowireCapableBeanFactory).getBean(BULK_PROCESS_TYPE + SERVICE_BEAN_NAME);
    verify(bulkUpdateService).processBulkArchiveProducts(bulkUpdateQueue);
    verify(bulkProcessService).abortBulkProcess(DEFAULT_STORE_ID, BULK_PROCESS_CODE);
    verify(objectMapper).readValue(Constant.CLIENT_ID, BulkUpdateQueue.class);
    verify(kafkaTopicProperties).getBulkArchiveProducts();
  }

  @Test
  public void onDomainEventConsumedNullBulkProcessCode() throws Exception {
    when(objectMapper.readValue(Constant.CLIENT_ID, BulkUpdateQueue.class)).thenReturn(bulkUpdateQueue);
    bulkUpdateQueue.setBulkProcessCode(null);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> bulkArchiveProductsListener.onDomainEventConsumed(Constant.CLIENT_ID));
    } finally {
      verify(objectMapper).readValue(Constant.CLIENT_ID, BulkUpdateQueue.class);
      verify(kafkaTopicProperties).getBulkArchiveProducts();
    }
  }
}