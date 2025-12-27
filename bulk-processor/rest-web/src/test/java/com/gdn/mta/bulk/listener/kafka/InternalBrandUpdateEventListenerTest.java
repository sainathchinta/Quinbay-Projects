package com.gdn.mta.bulk.listener.kafka;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.InternalBrandUpdateEventModel;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;

@ExtendWith(MockitoExtension.class)
public class InternalBrandUpdateEventListenerTest {

  @Mock
  private InternalProcessServiceWrapper internalProcessServiceWrapper;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @InjectMocks
  private InternalBrandUpdateEventListener internalBrandUpdateEventListener;

  private static final String MESSAGE = "{\"storeId\":\"10001\",\"brandCode\":\"BRAND123\",\"processType\":\"INTERNAL_BRAND_UPDATE\"}";

  @Test
  void testOnDomainEventConsumed_Success() throws Exception {
    // Given
    InternalBrandUpdateEventModel eventModel = InternalBrandUpdateEventModel.builder()
        .storeId("10001")
        .oldBrandCode("BRAND123")
        .processType("INTERNAL_BRAND_UPDATE")
        .build();

    when(objectMapper.readValue(MESSAGE, InternalBrandUpdateEventModel.class)).thenReturn(eventModel);
    when(kafkaTopicProperties.getInternalBrandUpdateEvent()).thenReturn("internal-brand-update-event");

    // When
    internalBrandUpdateEventListener.onDomainEventConsumed(MESSAGE);

    // Then
    verify(internalProcessServiceWrapper).processInternalBrandUpdateEvent(eventModel);
  }

  @Test
  void testOnDomainEventConsumed_Exception() throws Exception {
    // Given
    when(objectMapper.readValue(MESSAGE, InternalBrandUpdateEventModel.class))
        .thenThrow(new RuntimeException("JSON parsing error"));
    when(kafkaTopicProperties.getInternalBrandUpdateEvent()).thenReturn("internal-brand-update-event");

    // When
    internalBrandUpdateEventListener.onDomainEventConsumed(MESSAGE);

    // Then - Should not throw exception, just log it
  }
}
