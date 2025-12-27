package com.gdn.mta.bulk.listener.kafka;

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
import com.gdn.mta.bulk.entity.ProductCodeAndCategoryCodeListEvent;
import com.gdn.mta.bulk.service.RecatProcessServiceWrapper;
import com.gdn.partners.bulk.util.Constant;

public class ProductRecatProcessListenerTest {

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(processServiceWrapper);
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
  }

  @InjectMocks
  private ProductRecatProcessListener productRecatProcessListener;

  @Mock
  private RecatProcessServiceWrapper processServiceWrapper;

  @Test
  public void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, ProductCodeAndCategoryCodeListEvent.class))
        .thenReturn(new ProductCodeAndCategoryCodeListEvent());
    productRecatProcessListener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(processServiceWrapper).updateProductCategory(new ProductCodeAndCategoryCodeListEvent());
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, ProductCodeAndCategoryCodeListEvent.class);
    Mockito.verify(kafkaTopicProperties).getProductRecatProcess();
  }

  @Test
  public void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(Constant.CLIENT_ID, ProductCodeAndCategoryCodeListEvent.class))
        .thenReturn(new ProductCodeAndCategoryCodeListEvent());
    Mockito.doThrow(ApplicationRuntimeException.class).when(processServiceWrapper)
        .updateProductCategory(new ProductCodeAndCategoryCodeListEvent());
    productRecatProcessListener.onDomainEventConsumed(Constant.CLIENT_ID);
    Mockito.verify(processServiceWrapper).updateProductCategory(new ProductCodeAndCategoryCodeListEvent());
    Mockito.verify(objectMapper).readValue(Constant.CLIENT_ID, ProductCodeAndCategoryCodeListEvent.class);
    Mockito.verify(kafkaTopicProperties).getProductRecatProcess();
  }
}