package com.gdn.mta.bulk.listener.kafka;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verifyNoMoreInteractions;

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
import com.gdn.mta.bulk.service.BulkCategoryProcessorService;
import com.gdn.partners.bulk.util.Constant;

public class BulkCategoryCreateProductListenerTest {

  private static final String STORE_ID = "10001";
  private static final String PARENT_PRODUCT = "parentProduct";
  private static final String BULK_PROCESS_CODE = "bulkProcessCode";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";

  @InjectMocks
  private BulkCategoryCreateProductListener listener;

  @Mock
  private BulkCategoryProcessorService bulkCategoryProcessorService;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

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
    verifyNoMoreInteractions(bulkCategoryProcessorService);
    verifyNoMoreInteractions(objectMapper);
    verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @Test
  public void onDomainEventConsumed() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class))
        .thenReturn(bulkCreateProductEventModel);
    Mockito.doNothing().when(bulkCategoryProcessorService).processEvent(bulkCreateProductEventModel);
    this.listener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(bulkCategoryProcessorService).processEvent(bulkCreateProductEventModel);
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class))
        .thenReturn(bulkCreateProductEventModel);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class);
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCnCreateProductEvent();
  }

  @Test
  public void onDomainEventConsumedForPriority1() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class))
      .thenReturn(bulkCreateProductEventModel);
    Mockito.doNothing().when(bulkCategoryProcessorService).processEvent(bulkCreateProductEventModel);
    this.listener.onDomainEventConsumedForPriority1(Constant.CLIENT_ID);
    Mockito.verify(bulkCategoryProcessorService).processEvent(bulkCreateProductEventModel);
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class))
      .thenReturn(bulkCreateProductEventModel);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class);
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCnCreateProductForPriority1Event();
  }

  @Test
  public void onDomainEventConsumedForPriority2() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class))
      .thenReturn(bulkCreateProductEventModel);
    Mockito.doNothing().when(bulkCategoryProcessorService).processEvent(bulkCreateProductEventModel);
    this.listener.onDomainEventConsumedForPriority2(Constant.CLIENT_ID);
    Mockito.verify(bulkCategoryProcessorService).processEvent(bulkCreateProductEventModel);
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class))
      .thenReturn(bulkCreateProductEventModel);
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class);
    Mockito.verify(kafkaTopicProperties, times(2)).getBulkCnCreateProductForPriority2Event();
  }


  @Test
  public void onDomainEventConsumed_Exception() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class))
        .thenReturn(bulkCreateProductEventModel);
    Mockito.doThrow(Exception.class).when(bulkCategoryProcessorService).processEvent(bulkCreateProductEventModel);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.listener.onDomainEventConsumed(Constant.CLIENT_ID));
    } finally {
      Mockito.verify(bulkCategoryProcessorService).processEvent(bulkCreateProductEventModel);
      Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class);
      Mockito.verify(kafkaTopicProperties, times(2)).getBulkCnCreateProductEvent();
    }
  }

  @Test
  public void onDomainEventConsumedPriority1_Exception() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class))
      .thenReturn(bulkCreateProductEventModel);
    Mockito.doThrow(Exception.class).when(bulkCategoryProcessorService).processEvent(bulkCreateProductEventModel);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.listener.onDomainEventConsumedForPriority1(Constant.CLIENT_ID));
    } finally {
      Mockito.verify(bulkCategoryProcessorService).processEvent(bulkCreateProductEventModel);
      Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class);
      Mockito.verify(kafkaTopicProperties, times(2)).getBulkCnCreateProductForPriority1Event();
    }
  }

  @Test
  public void onDomainEventConsumedPriority2_Exception() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class))
      .thenReturn(bulkCreateProductEventModel);
    Mockito.doThrow(Exception.class).when(bulkCategoryProcessorService).processEvent(bulkCreateProductEventModel);
    try {
      Assertions.assertThrows(ApplicationException.class,
          () -> this.listener.onDomainEventConsumedForPriority2(Constant.CLIENT_ID));
    } finally {
      Mockito.verify(bulkCategoryProcessorService).processEvent(bulkCreateProductEventModel);
      Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, BulkCreateProductEventModel.class);
      Mockito.verify(kafkaTopicProperties, times(2)).getBulkCnCreateProductForPriority2Event();
    }
  }
}
