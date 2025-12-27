package com.gdn.mta.bulk.listener.kafka;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.slf4j.MDC;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkProcessQueue;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.mta.bulk.service.ProcessorService;
import com.gdn.partners.bulk.util.Constant;

class BulkConvertedFileProductCreationListenerTest {

  @InjectMocks
  private BulkConvertedFileProductCreationListener listener;

  @Mock
  private AutowireCapableBeanFactory autowireCapableBeanFactory;

  @Mock
  private BulkProcessService bulkProcessService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private ProcessorService processorService;

  private static final String TEST_TOPIC = "test-bulk-converted-product-creation-event";
  private static final String TEST_STORE_ID = "test-store-123";
  private static final String TEST_BULK_PROCESS_CODE = "BPC-123456";
  private static final String TEST_BULK_PROCESS_TYPE = "ProductCreation";
  private static final String TEST_MESSAGE =
      "{\"storeId\":\"test-store-123\",\"bulkProcessCode\":\"BPC-123456\",\"bulkProcessType\":\"ProductCreation\"}";

  @BeforeEach
  void setUp() {
    MockitoAnnotations.openMocks(this);
    when(kafkaTopicProperties.getBulkConvertedProductCreationEvent()).thenReturn(TEST_TOPIC);
  }

  @AfterEach
  void tearDown() {
    MDC.clear();
    Mockito.verifyNoMoreInteractions(processorService, kafkaTopicProperties, objectMapper, bulkProcessService,
        autowireCapableBeanFactory);
  }

  @Test
  void onDomainEventConsumed_SuccessfulProcessing_ShouldProcessMessageSuccessfully() throws Exception {
    BulkProcessQueue bulkProcessQueue = createTestBulkProcessQueue();
    when(objectMapper.readValue(TEST_MESSAGE, BulkProcessQueue.class)).thenReturn(bulkProcessQueue);
    when(autowireCapableBeanFactory.getBean(TEST_BULK_PROCESS_TYPE + Constant.PROCESSOR_SERVICE)).thenReturn(
        processorService);
    doNothing().when(processorService).process(bulkProcessQueue);
    assertDoesNotThrow(() -> listener.onDomainEventConsumed(TEST_MESSAGE));
    verify(objectMapper).readValue(TEST_MESSAGE, BulkProcessQueue.class);
    verify(autowireCapableBeanFactory).getBean(TEST_BULK_PROCESS_TYPE + Constant.PROCESSOR_SERVICE);
    verify(processorService).process(bulkProcessQueue);
    verify(bulkProcessService, never()).abortBulkProcess(anyString(), anyString());
    Mockito.verify(kafkaTopicProperties).getBulkConvertedProductCreationEvent();
  }

  @Test
  void onDomainEventConsumed_BeanNotFoundException_ShouldAbortBulkProcess() throws Exception {
    BulkProcessQueue bulkProcessQueue = createTestBulkProcessQueue();
    when(objectMapper.readValue(TEST_MESSAGE, BulkProcessQueue.class)).thenReturn(bulkProcessQueue);
    when(autowireCapableBeanFactory.getBean(TEST_BULK_PROCESS_TYPE + Constant.PROCESSOR_SERVICE)).thenThrow(
        new RuntimeException("Bean not found"));
    assertDoesNotThrow(() -> listener.onDomainEventConsumed(TEST_MESSAGE));
    verify(objectMapper).readValue(TEST_MESSAGE, BulkProcessQueue.class);
    verify(autowireCapableBeanFactory).getBean(TEST_BULK_PROCESS_TYPE + Constant.PROCESSOR_SERVICE);
    verify(bulkProcessService).abortBulkProcess(TEST_STORE_ID, TEST_BULK_PROCESS_CODE);
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkConvertedProductCreationEvent();
  }

  @Test
  void onDomainEventConsumed_ProcessorServiceException_ShouldAbortBulkProcess() throws Exception {
    BulkProcessQueue bulkProcessQueue = createTestBulkProcessQueue();
    when(objectMapper.readValue(TEST_MESSAGE, BulkProcessQueue.class)).thenReturn(bulkProcessQueue);
    when(autowireCapableBeanFactory.getBean(TEST_BULK_PROCESS_TYPE + Constant.PROCESSOR_SERVICE)).thenReturn(
        processorService);
    doThrow(new RuntimeException("Processing failed")).when(processorService).process(bulkProcessQueue);
    assertDoesNotThrow(() -> listener.onDomainEventConsumed(TEST_MESSAGE));
    verify(objectMapper).readValue(TEST_MESSAGE, BulkProcessQueue.class);
    verify(autowireCapableBeanFactory).getBean(TEST_BULK_PROCESS_TYPE + Constant.PROCESSOR_SERVICE);
    verify(processorService).process(bulkProcessQueue);
    verify(bulkProcessService).abortBulkProcess(TEST_STORE_ID, TEST_BULK_PROCESS_CODE);
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkConvertedProductCreationEvent();
  }

  @Test
  void onDomainEventConsumed_ShouldSetMandatoryParameters() throws Exception {
    BulkProcessQueue bulkProcessQueue = createTestBulkProcessQueue();
    when(objectMapper.readValue(TEST_MESSAGE, BulkProcessQueue.class)).thenReturn(bulkProcessQueue);
    when(autowireCapableBeanFactory.getBean(TEST_BULK_PROCESS_TYPE + Constant.PROCESSOR_SERVICE)).thenReturn(
        processorService);
    doNothing().when(processorService).process(bulkProcessQueue);
    listener.onDomainEventConsumed(TEST_MESSAGE);
    verify(objectMapper).readValue(TEST_MESSAGE, BulkProcessQueue.class);
    verify(autowireCapableBeanFactory).getBean(TEST_BULK_PROCESS_TYPE + Constant.PROCESSOR_SERVICE);
    verify(processorService).process(bulkProcessQueue);
    Mockito.verify(kafkaTopicProperties).getBulkConvertedProductCreationEvent();
  }

  @Test
  void onDomainEventConsumed_WithDifferentBulkProcessType_ShouldUseCorrectBeanName() throws Exception {
    String customProcessType = "CustomProcess";
    BulkProcessQueue bulkProcessQueue = createTestBulkProcessQueue();
    bulkProcessQueue.setBulkProcessType(customProcessType);
    when(objectMapper.readValue(TEST_MESSAGE, BulkProcessQueue.class)).thenReturn(bulkProcessQueue);
    when(autowireCapableBeanFactory.getBean(customProcessType + Constant.PROCESSOR_SERVICE)).thenReturn(
        processorService);
    doNothing().when(processorService).process(bulkProcessQueue);
    listener.onDomainEventConsumed(TEST_MESSAGE);
    verify(autowireCapableBeanFactory).getBean(customProcessType + Constant.PROCESSOR_SERVICE);
    verify(processorService).process(bulkProcessQueue);
    Mockito.verify(kafkaTopicProperties).getBulkConvertedProductCreationEvent();
    Mockito.verify(objectMapper).readValue(TEST_MESSAGE, BulkProcessQueue.class);
  }

  private BulkProcessQueue createTestBulkProcessQueue() {
    BulkProcessQueue bulkProcessQueue = new BulkProcessQueue();
    bulkProcessQueue.setStoreId(TEST_STORE_ID);
    bulkProcessQueue.setBulkProcessCode(TEST_BULK_PROCESS_CODE);
    bulkProcessQueue.setBulkProcessType(TEST_BULK_PROCESS_TYPE);

    Map<String, String> args = new HashMap<>();
    args.put("key1", "value1");
    args.put("key2", "value2");
    bulkProcessQueue.setArgs(args);

    return bulkProcessQueue;
  }
}