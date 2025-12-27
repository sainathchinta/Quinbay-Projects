package com.gdn.partners.product.analytics.service.impl.domainEvent;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.service.DsExtractedAttributeService;
import model.AttributeUpdateEventModel;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.MockitoAnnotations.initMocks;

class DsExtractedAttributeListenerTest {

  private static final String MESSAGE = "message";

  private static final String TOPIC = "topic";

  @InjectMocks
  private DsExtractedAttributeListener dsExtractedAttributeListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private DsExtractedAttributeService dsExtractedAttributeService;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @BeforeEach
  void setUp() {
    initMocks(this);
  }

  @AfterEach
  void afterTest() {
    Mockito.verifyNoMoreInteractions(objectMapper);
    Mockito.verifyNoMoreInteractions(dsExtractedAttributeService);
    Mockito.verifyNoMoreInteractions(kafkaTopicProperties);
    Mockito.verifyNoMoreInteractions(dsExtractedAttributeService);
  }

  @Test
  void onDomainEventConsumedTest() throws Exception {
    Mockito.when(kafkaTopicProperties.getAttributeChangeEventName())
        .thenReturn(TOPIC);
    dsExtractedAttributeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, AttributeUpdateEventModel.class);
    Mockito.verify(dsExtractedAttributeService).updateDsExtractedAttribute(any());
    Mockito.verify(kafkaTopicProperties).getAttributeChangeEventName();
  }

  @Test
  void onDomainEventConsumedTest_ThrowsException() throws Exception {
    Mockito.when(kafkaTopicProperties.getAttributeChangeEventName()).thenReturn(TOPIC);
    Mockito.when(objectMapper.readValue(MESSAGE, AttributeUpdateEventModel.class))
        .thenThrow(new RuntimeException("Deserialization error"));
    dsExtractedAttributeListener.onDomainEventConsumed(MESSAGE);
    Mockito.verify(objectMapper).readValue(MESSAGE, AttributeUpdateEventModel.class);
    Mockito.verify(dsExtractedAttributeService, Mockito.never()).updateDsExtractedAttribute(Mockito.any());
    Mockito.verify(kafkaTopicProperties).getAttributeChangeEventName();
  }
}
