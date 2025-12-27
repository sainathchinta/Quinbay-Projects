package com.gdn.mta.bulk.listener.kafka;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkCreateProductEventModel;
import com.gdn.mta.bulk.service.BulkGenericProcessorService;
import com.gdn.partners.bulk.util.Constant;

public class BulkGenericCreateProductListenerTest {

  private static final String STORE_ID = "10001";
  private static final String PARENT_PRODUCT = "parentProduct";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";

  @InjectMocks
  private BulkGenericCreateProductListener listener;

  @Mock
  private BulkGenericProcessorService bulkGenericProcessorService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private ObjectMapper objectMapper;

  private BulkCreateProductEventModel bulkCreateProductEventModel;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    bulkCreateProductEventModel =
        BulkCreateProductEventModel.builder().parentProduct(PARENT_PRODUCT).bulkProcessCode(BULK_PROCESS_CODE)
            .businessPartnerCode(BUSINESS_PARTNER_CODE).storeId(STORE_ID).build();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(bulkGenericProcessorService);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class))
        .thenReturn(bulkCreateProductEventModel);
    Mockito.doNothing().when(bulkGenericProcessorService).processBulkGenericEvent(bulkCreateProductEventModel);
    this.listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(bulkGenericProcessorService).processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkGenericCreateProductEvent();
  }

  @Test
  public void onDomainEventConsumedForPriority1() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class))
        .thenReturn(bulkCreateProductEventModel);
    Mockito.doNothing().when(bulkGenericProcessorService).processBulkGenericEvent(bulkCreateProductEventModel);
    this.listener.onDomainEventConsumedForPriority1(Constant.CLIENT_ID);
    Mockito.verify(bulkGenericProcessorService).processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkGenericCreateProductForPriority1Event();
  }

  @Test
  public void onDomainEventConsumedForPriority2() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class))
      .thenReturn(bulkCreateProductEventModel);
    Mockito.doNothing().when(bulkGenericProcessorService).processBulkGenericEvent(bulkCreateProductEventModel);
    this.listener.onDomainEventConsumedForPriority2(Constant.CLIENT_ID);
    Mockito.verify(bulkGenericProcessorService).processBulkGenericEvent(bulkCreateProductEventModel);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkGenericCreateProductForPriority2Event();
  }

  @Test
  public void onDomainEventConsumed_Exception() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class))
        .thenReturn(bulkCreateProductEventModel);
    Mockito.doThrow(Exception.class).when(bulkGenericProcessorService)
        .processBulkGenericEvent(bulkCreateProductEventModel);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.listener.onDomainEventConsumed(Constant.CLIENT_ID));
    } finally {
      Mockito.verify(bulkGenericProcessorService).processBulkGenericEvent(bulkCreateProductEventModel);
      Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class);
      Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkGenericCreateProductEvent();
    }
  }

  @Test
  public void onDomainEventConsumedPriority1_Exception() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class))
        .thenReturn(bulkCreateProductEventModel);
    Mockito.doThrow(Exception.class).when(bulkGenericProcessorService)
        .processBulkGenericEvent(bulkCreateProductEventModel);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.listener.onDomainEventConsumedForPriority1(Constant.CLIENT_ID));
    } finally {
      Mockito.verify(bulkGenericProcessorService).processBulkGenericEvent(bulkCreateProductEventModel);
      Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class);
      Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkGenericCreateProductForPriority1Event();
    }
  }

  @Test
  public void onDomainEventConsumedPriority2_Exception() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class))
      .thenReturn(bulkCreateProductEventModel);
    Mockito.doThrow(Exception.class).when(bulkGenericProcessorService)
      .processBulkGenericEvent(bulkCreateProductEventModel);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.listener.onDomainEventConsumedForPriority2(Constant.CLIENT_ID));
    } finally {
      Mockito.verify(bulkGenericProcessorService).processBulkGenericEvent(bulkCreateProductEventModel);
      Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class);
      Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getBulkGenericCreateProductForPriority2Event();
    }
  }
}
