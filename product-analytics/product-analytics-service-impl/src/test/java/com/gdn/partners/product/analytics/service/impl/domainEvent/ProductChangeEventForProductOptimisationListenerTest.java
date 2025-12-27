package com.gdn.partners.product.analytics.service.impl.domainEvent;


import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.service.ProductOptimisationService;
import model.ProductChangeEventModel;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

public class ProductChangeEventForProductOptimisationListenerTest {

  @InjectMocks
  private ProductChangeEventForProductOptimisationListener listener;
  @Mock
  private KafkaTopicProperties kafkaTopicProperties;
  @Mock
  private ObjectMapper objectMapper;
  @Mock
  private ProductOptimisationService productOptimisationService;
  private ProductChangeEventModel productChangeEventModel;
  private final String PRODUCT_SKU = "productSku";
  private final String EVENT = "EVENT";
  private final String MESSAGE = "{\"productSku\":\"productSku\"}";

  @BeforeEach
  public void setup() {
    MockitoAnnotations.initMocks(this);
    productChangeEventModel = new ProductChangeEventModel();
    productChangeEventModel.setProductSku(PRODUCT_SKU);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(productOptimisationService);
  }

  @Test
  void onDomainEventConsumedTest() throws JsonProcessingException {
    Mockito.when(kafkaTopicProperties.getProductChangeEventName()).thenReturn(EVENT);
    Mockito.when(objectMapper.readValue(MESSAGE, ProductChangeEventModel.class))
        .thenReturn(productChangeEventModel);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, ProductChangeEventModel.class);
    Mockito.verify(productOptimisationService).removeDeletedProduct(productChangeEventModel);
    Mockito.verify(kafkaTopicProperties).getProductChangeEventName();
  }

  @Test
  void onDomainEventConsumedTest_exceptionTest() throws Exception {
    Mockito.when(kafkaTopicProperties.getProductChangeEventName()).thenReturn(EVENT);
    Mockito.when(objectMapper.readValue(MESSAGE, ProductChangeEventModel.class))
        .thenThrow(JsonProcessingException.class);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, ProductChangeEventModel.class);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getProductChangeEventName();
  }
}
