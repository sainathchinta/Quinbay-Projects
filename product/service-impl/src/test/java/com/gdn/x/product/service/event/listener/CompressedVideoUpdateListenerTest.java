package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.model.CompressedVideoUpdateEventModel;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.properties.KafkaTopicProperties;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class CompressedVideoUpdateListenerTest {

  private static final String JSON =
    "{\"videoId\":\"VID-12345\",\"finalUrl\":\"https://example.com/video.mp4\","
      + "\"productCode\":\"PRD-12345\",\"productSku\":\"SKU-12345\","
      + "\"coverImagePath\":\"https://example.com/cover.jpg\","
      + "\"sourceUrl\":\"https://example.com/source.mp4\","
      + "\"videoName\":\"Product Demo Video\"}";

  @InjectMocks
  private CompressedVideoUpdateListener compressedVideoUpdateListener;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductService productServiceWrapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private CompressedVideoUpdateEventModel mockModel;

  @AfterEach
  void tearDown() {
    Mockito.verifyNoMoreInteractions(objectMapper, productServiceWrapper);
  }

  @Test
  @DisplayName("Should successfully process video update message")
  void onDomainEventConsumed_Success() throws JsonProcessingException {
    Mockito.when(objectMapper.readValue(JSON, CompressedVideoUpdateEventModel.class))
      .thenReturn(mockModel);

    compressedVideoUpdateListener.onDomainEventConsumed(JSON);

    Mockito.verify(objectMapper).readValue(JSON, CompressedVideoUpdateEventModel.class);
    Mockito.verify(productServiceWrapper).updateFinalVideoData(mockModel);
  }

  @Test
  @DisplayName("Should handle JSON processing errors")
  void onDomainEventConsumed_JsonProcessingException() throws JsonProcessingException {
    JsonProcessingException exception = Mockito.mock(JsonProcessingException.class);
    Mockito.when(exception.getMessage()).thenReturn("Invalid JSON format");
    Mockito.when(objectMapper.readValue(JSON, CompressedVideoUpdateEventModel.class))
      .thenThrow(exception);

    compressedVideoUpdateListener.onDomainEventConsumed(JSON);

    Mockito.verify(objectMapper).readValue(JSON, CompressedVideoUpdateEventModel.class);
    Mockito.verifyNoInteractions(productServiceWrapper);
  }

  @Test
  @DisplayName("Should handle service-level errors")
  void onDomainEventConsumed_RuntimeException() throws JsonProcessingException {
    Mockito.when(objectMapper.readValue(JSON, CompressedVideoUpdateEventModel.class))
      .thenReturn(mockModel);
    RuntimeException exception = new RuntimeException("Service error");
    Mockito.doThrow(exception).when(productServiceWrapper).updateFinalVideoData(mockModel);

    compressedVideoUpdateListener.onDomainEventConsumed(JSON);

    Mockito.verify(objectMapper).readValue(JSON, CompressedVideoUpdateEventModel.class);
    Mockito.verify(productServiceWrapper).updateFinalVideoData(mockModel);
  }
} 