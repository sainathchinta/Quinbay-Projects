package com.gdn.x.productcategorybase.domainevent;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.lenient;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.junit.jupiter.api.Assertions.assertThrows;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.domain.event.model.CompressedVideoUpdateEventModel;
import com.gdn.x.productcategorybase.service.ProductServiceWrapper;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Map;
import java.util.Objects;

@ExtendWith(MockitoExtension.class)
class CompressedVideoUpdateListenerTest {

  private static final String TEST_TOPIC = "test-video-update-topic";
  private static final String TEST_MESSAGE = "{\"videoId\":\"VID001\",\"finalUrl\":\"https://example.com/video.mp4\",\"productCode\":\"PROD001\"}";
  public static final String FINAL_VIDEO_URL = "https://example.com/video.mp4";
  public static final String VIDEO_ID = "VID001";
  public static final String PRODUCT_CODE = "PROD001";
  public static final String MESSAGE = "Processing error";
  public static final String INVALID_JSON = "{invalid-json}";
  public static final String NULL_EVENT_MESSAGE = "{\"empty\":true}";

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private ProductServiceWrapper productServiceWrapper;

  @InjectMocks
  private CompressedVideoUpdateListener compressedVideoUpdateListener;

  private CompressedVideoUpdateEventModel compressedVideoUpdateEventModel;

  @BeforeEach
  void setUp() throws JsonProcessingException {
    compressedVideoUpdateEventModel =
      CompressedVideoUpdateEventModel.builder().videoId(VIDEO_ID).finalUrl(FINAL_VIDEO_URL)
        .additionalFields(Map.of(Constants.PRODUCT_CODE, PRODUCT_CODE)).build();

    // Use lenient() to avoid UnnecessaryStubbingException
    lenient().when(kafkaTopicProperties.getCompressedVideoUpdateEvent()).thenReturn(TEST_TOPIC);
    lenient().when(objectMapper.readValue(TEST_MESSAGE, CompressedVideoUpdateEventModel.class))
      .thenReturn(compressedVideoUpdateEventModel);
  }

  @Test
  void testOnDomainEventConsumed_Success() throws Exception {
    // Act
    compressedVideoUpdateListener.onDomainEventConsumed(TEST_MESSAGE);

    // Assert
    verify(objectMapper).readValue(TEST_MESSAGE, CompressedVideoUpdateEventModel.class);
    verify(productServiceWrapper).processCompressedUpdatedVideo(compressedVideoUpdateEventModel);
  }

  @Test
  void testOnDomainEventConsumed_ProcessingException() throws Exception {
    doThrow(new RuntimeException(MESSAGE)).when(productServiceWrapper)
      .processCompressedUpdatedVideo(any(CompressedVideoUpdateEventModel.class));

    compressedVideoUpdateListener.onDomainEventConsumed(TEST_MESSAGE);
    verify(objectMapper).readValue(TEST_MESSAGE, CompressedVideoUpdateEventModel.class);
    verify(productServiceWrapper).processCompressedUpdatedVideo(compressedVideoUpdateEventModel);
  }

  @Test
  void testOnDomainEventConsumed_JsonProcessingException() throws Exception {
    String invalidMessage = INVALID_JSON;
    when(objectMapper.readValue(invalidMessage, CompressedVideoUpdateEventModel.class))
      .thenThrow(new JsonProcessingException(MESSAGE) {});
    assertThrows(JsonProcessingException.class, () ->
      compressedVideoUpdateListener.onDomainEventConsumed(invalidMessage));
  }

  @Test
  void testOnDomainEventConsumed_NullEventModel() throws Exception {
    String nullEventMessage = NULL_EVENT_MESSAGE;
    when(objectMapper.readValue(nullEventMessage, CompressedVideoUpdateEventModel.class))
      .thenReturn(null);
    ApplicationRuntimeException exception = assertThrows(ApplicationRuntimeException.class, () ->
      compressedVideoUpdateListener.onDomainEventConsumed(nullEventMessage));
    Assertions.assertTrue(Objects.nonNull(exception));
  }
} 