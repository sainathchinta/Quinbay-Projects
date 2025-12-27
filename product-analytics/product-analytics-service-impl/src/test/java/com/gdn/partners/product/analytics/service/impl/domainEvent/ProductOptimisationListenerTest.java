package com.gdn.partners.product.analytics.service.impl.domainEvent;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.service.ProductOptimisationService;
import model.ProductOptimisationEventModel;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class ProductOptimisationListenerTest {

  private static final String MESSAGE = "message";
  private static final String EVENT_NAME = "event-name";

  @InjectMocks
  private ProductOptimisationListener productOptimisationListener;
  @Mock
  private KafkaTopicProperties kafkaTopicProperties;
  @Mock
  private ProductOptimisationService productOptimisationService;

  @Mock
  private ObjectMapper objectMapper;

  private ProductOptimisationEventModel eventModel;

  @BeforeEach
  public void setup() {
    eventModel = new ProductOptimisationEventModel();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties, productOptimisationService,
      objectMapper);
  }

  @Test
  void onDomainEventConsumedTest() throws JsonProcessingException {
    Mockito.when(kafkaTopicProperties.getProductOptimisationEventName()).thenReturn(EVENT_NAME);
    Mockito.when(objectMapper.readValue(MESSAGE, ProductOptimisationEventModel.class))
      .thenReturn(eventModel);
    Mockito.doNothing().when(productOptimisationService).upsertProductOptimisationData(eventModel);
    productOptimisationListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, ProductOptimisationEventModel.class);
    Mockito.verify(kafkaTopicProperties).getProductOptimisationEventName();
    Mockito.verify(productOptimisationService).upsertProductOptimisationData(eventModel);
  }

  @Test
  void onDomainEventConsumedNullEventBodyTest() throws JsonProcessingException {
    Mockito.when(kafkaTopicProperties.getProductOptimisationEventName()).thenReturn(EVENT_NAME);
    Mockito.when(objectMapper.readValue(MESSAGE, ProductOptimisationEventModel.class))
      .thenReturn(null);
    productOptimisationListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, ProductOptimisationEventModel.class);
    Mockito.verify(kafkaTopicProperties).getProductOptimisationEventName();
  }

  @Test
  void onDomainEventConsumedExceptionTest() throws JsonProcessingException {
    Mockito.when(kafkaTopicProperties.getProductOptimisationEventName()).thenReturn(EVENT_NAME);
    Mockito.doThrow(RuntimeException.class).when(objectMapper)
      .readValue(MESSAGE, ProductOptimisationEventModel.class);
    productOptimisationListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, ProductOptimisationEventModel.class);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getProductOptimisationEventName();
  }
}
