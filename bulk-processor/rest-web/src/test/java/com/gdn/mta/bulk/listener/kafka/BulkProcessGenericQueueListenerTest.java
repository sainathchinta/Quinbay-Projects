package com.gdn.mta.bulk.listener.kafka;

import com.gdn.common.exception.ApplicationException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.product.constant.TrackerConstants;
import com.gdn.mta.bulk.entity.BulkProcessQueue;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.mta.bulk.service.ProcessorService;
import com.gdn.mta.bulk.service.TrackerService;
import com.gdn.partners.bulk.util.Constant;

public class BulkProcessGenericQueueListenerTest {

  private static final String STORE_ID = "10001";
  private static final String BULK_PROCESS_TYPE = "ProductLevel3Generic";
  private static final String BULK_PROCESS_CODE = "BulkProcessCode";

  private BulkProcessQueue bulkProcessQueue;

  @InjectMocks
  private BulkProcessGenericQueueListener bulkProcessGenericQueueListener;

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
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    bulkProcessQueue = new BulkProcessQueue();
    bulkProcessQueue.setBulkProcessType(BULK_PROCESS_TYPE);
    bulkProcessQueue.setBulkProcessCode(BULK_PROCESS_CODE);
    bulkProcessQueue.setStoreId(STORE_ID);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(autowireCapableBeanFactory, trackerService, bulkProcessService, kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(objectMapper);
  }

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkProcessQueue.class)).thenReturn(bulkProcessQueue);
    ProcessorService processorService = Mockito.mock(ProcessorService.class);
    Mockito.when(autowireCapableBeanFactory.getBean(bulkProcessQueue.getBulkProcessType() + "ProcessorService"))
        .thenReturn(processorService);
    bulkProcessGenericQueueListener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(autowireCapableBeanFactory).getBean(bulkProcessQueue.getBulkProcessType() + "ProcessorService");
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkProcessQueue.class);
    Mockito.verify(kafkaTopicProperties).getBulkGenericCreateEvent();
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkProcessQueue.class)).thenReturn(bulkProcessQueue);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> bulkProcessGenericQueueListener.onDomainEventConsumed(Constant.CLIENT_ID));
    } finally {
      Mockito.verify(autowireCapableBeanFactory).getBean(bulkProcessQueue.getBulkProcessType() + "ProcessorService");
      Mockito.verify(bulkProcessService).abortBulkProcess(STORE_ID, BULK_PROCESS_CODE);
      Mockito.verify(trackerService)
          .sendTracker(TrackerConstants.PRODUCT_CREATE_EVENT, TrackerConstants.CREATE_FLOW1_WEB,
              TrackerConstants.SUBMIT, TrackerConstants.FAILED, null);
      Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkProcessQueue.class);
      Mockito.verify(kafkaTopicProperties).getBulkGenericCreateEvent();
    }
  }
}