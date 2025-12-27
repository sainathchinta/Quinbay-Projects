package com.gdn.x.mta.distributiontask.inbound.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductEmailEventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.service.api.ProductEmailService;
import org.bson.json.JsonParseException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ProductEmailListenerTest {

  private static final String PRODUCT_SKU = "ProductSku";
  private static final String JSON = "{}";

  @InjectMocks
  private ProductEmailListener productEmailListener;
  @Mock
  private ObjectMapper objectMapper;
  @Mock
  private ProductEmailService productEmailService;
  @Mock
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  private ProductEmailEventModel productEmailEventModel;

  @BeforeEach
  void setUp() {
    productEmailEventModel = new ProductEmailEventModel();
    productEmailEventModel.setProductSku(PRODUCT_SKU);
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper, productEmailService, kafkaTopicPropertiesConsumer);
  }

  @Test
  void onDomainEventConsumedTest() throws Exception {
    Mockito.when(objectMapper.readValue(JSON, ProductEmailEventModel.class))
      .thenReturn(productEmailEventModel);
    productEmailListener.onDomainEventConsumed(JSON);
    Mockito.verify(objectMapper).readValue(JSON, ProductEmailEventModel.class);
    Mockito.verify(productEmailService).addProductToEmailProcess(productEmailEventModel);
    Mockito.verify(kafkaTopicPropertiesConsumer).getAddProductMailEvent();
  }

  @Test
  void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(JSON, ProductEmailEventModel.class))
      .thenThrow(JsonParseException.class);
    try {
      productEmailListener.onDomainEventConsumed(JSON);
    } finally {
      Mockito.verify(objectMapper).readValue(JSON, ProductEmailEventModel.class);
      Mockito.verify(kafkaTopicPropertiesConsumer, Mockito.times(2)).getAddProductMailEvent();
    }
  }
}