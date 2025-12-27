package com.gdn.x.mta.distributiontask.inbound.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductDeleteEventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.service.api.ProductService;

import org.bson.json.JsonParseException;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Collections;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertNotNull;

@ExtendWith(MockitoExtension.class)
class ProductDeleteEventListenerTest {

  private static final String STORE_ID = "STORE-123";
  private static final String PRODUCT_CODE = "PRD-001";
  private static final String JSON = "{}";
  private static final String TOPIC = "delete-product-event";
  private static final int FETCH_SIZE = 50;

  @InjectMocks
  private ProductDeleteEventListener productDeleteEventListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductService productService;

  @Mock
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  private ProductDeleteEventModel productDeleteEventModel;
  private Product product;

  @BeforeEach
  void setUp() {
    ReflectionTestUtils.setField(productDeleteEventListener, "productDeleteItemFetchSize", FETCH_SIZE);
    productDeleteEventModel = new ProductDeleteEventModel();
    productDeleteEventModel.setStoreId(STORE_ID);
    productDeleteEventModel.setIdentifier(UUID.randomUUID().toString());
    productDeleteEventModel.setProductCodeList(Collections.singletonList(PRODUCT_CODE));

    product = new Product();
    product.setId("1");
  }

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper, productService, kafkaTopicPropertiesConsumer);
  }

  @Test
  void onDomainEventConsumedWhenProductPickedForDeletionTest() throws Exception {
    product.setPickedForDeletion(true);
    Mockito.when(objectMapper.readValue(JSON, ProductDeleteEventModel.class)).thenReturn(productDeleteEventModel);
    Mockito.when(kafkaTopicPropertiesConsumer.getDeleteProductEvent()).thenReturn(TOPIC);
    Mockito.when(productService.findProductByProductCode(PRODUCT_CODE)).thenReturn(product);

    productDeleteEventListener.onDomainEventConsumed(JSON);

    Mockito.verify(objectMapper).readValue(JSON, ProductDeleteEventModel.class);
    Mockito.verify(kafkaTopicPropertiesConsumer).getDeleteProductEvent();
    Mockito.verify(productService).findProductByProductCode(PRODUCT_CODE);
    Mockito.verify(productService)
        .deleteProducts(STORE_ID, Collections.singletonList(product.getId()), Collections.singletonList(PRODUCT_CODE),
            FETCH_SIZE);
    assertNotNull(productDeleteEventModel.getIdentifier());
  }

  @Test
  void onDomainEventConsumedWhenProductNotPickedForDeletionTest() throws Exception {
    product.setPickedForDeletion(false);
    Mockito.when(objectMapper.readValue(JSON, ProductDeleteEventModel.class)).thenReturn(productDeleteEventModel);
    Mockito.when(kafkaTopicPropertiesConsumer.getDeleteProductEvent()).thenReturn(TOPIC);
    Mockito.when(productService.findProductByProductCode(PRODUCT_CODE)).thenReturn(product);

    productDeleteEventListener.onDomainEventConsumed(JSON);

    Mockito.verify(objectMapper).readValue(JSON, ProductDeleteEventModel.class);
    Mockito.verify(kafkaTopicPropertiesConsumer).getDeleteProductEvent();
    Mockito.verify(productService).findProductByProductCode(PRODUCT_CODE);
  }

  @Test
  void onDomainEventConsumedExceptionTest() throws Exception {
    Mockito.when(objectMapper.readValue(JSON, ProductDeleteEventModel.class)).thenThrow(JsonParseException.class);
    Mockito.when(kafkaTopicPropertiesConsumer.getDeleteProductEvent()).thenReturn(TOPIC);
    try {
      productDeleteEventListener.onDomainEventConsumed(JSON);
    } finally {
      Mockito.verify(objectMapper).readValue(JSON, ProductDeleteEventModel.class);
      Mockito.verify(kafkaTopicPropertiesConsumer).getDeleteProductEvent();
    }
  }
}
