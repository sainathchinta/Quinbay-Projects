package com.gdn.partners.product.analytics.service.impl.domainEvent;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.service.ProductOptimisationService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;


@ExtendWith(MockitoExtension.class)
class SellerLevelCacheClearForProductOptimisationListenerTest {

  @InjectMocks
  private SellerLevelCacheClearForProductOptimisationListener listener;
  @Mock
  private KafkaTopicProperties kafkaTopicProperties;
  @Mock
  private ObjectMapper objectMapper;
  @Mock
  private ProductOptimisationService productOptimisationService;

  private static final String MESSAGE = "[\"SELLER_CODE\"]";
  private static final String EVENT = "EVENT";

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(productOptimisationService);
  }

  @BeforeEach
  void setUp() {
  }

  @Test
  void onDomainEventConsumedTest() {
    Mockito.when(kafkaTopicProperties.getSellerCacheClearEventName()).thenReturn(EVENT);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(productOptimisationService).clearSellerLevelCache(Mockito.any());
    Mockito.verify(kafkaTopicProperties).getSellerCacheClearEventName();
  }

  @Test
  void onDomainEventConsumedExceptionTest() {
    Mockito.when(kafkaTopicProperties.getSellerCacheClearEventName()).thenReturn(EVENT);
    Mockito.doThrow(RuntimeException.class).when(productOptimisationService).clearSellerLevelCache(MESSAGE);
    listener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(kafkaTopicProperties, Mockito.times(2)).getSellerCacheClearEventName();
  }
}
