package com.gdn.partners.product.analytics.service.impl.domainEvent;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.service.ProductAttributeExtractionsService;
import model.ProductAttributeFeedbackEventModel;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class AttributeFeedbackEventListenerTest {

  private static final String TOPIC_NAME = "product-attribute-feedback-topic";
  private static final String TEST_MESSAGE =
    "{\"productCode\":\"PRODUCT_CODE\",\"attributeName\":\"color\",\"previousValue\":[\"red\"],"
      + "\"currentValue\":[\"blue\"],\"username\":\"username\"}";
  private static final String PRODUCT_CODE = "PRODUCT_CODE";
  private static final String ATTRIBUTE_NAME = "color";

  @Mock
  private KafkaTopicProperties kafkaTopicProperties;

  @Mock
  private ObjectMapper objectMapper;

  @Mock
  private ProductAttributeExtractionsService extractionsService;

  @InjectMocks
  private ProductAttributeFeedbackEventListener attributeFeedbackEventListener;

  private ProductAttributeFeedbackEventModel testEventModel;

  @BeforeEach
  void setUp() {
    testEventModel =
      ProductAttributeFeedbackEventModel.builder().productCode(PRODUCT_CODE).attributeName(ATTRIBUTE_NAME)
        .previousValue(List.of("red")).currentValue(List.of("blue")).build();
  }

  @Test
  void onDomainEventConsumed_Success() throws Exception {
    // Arrange
    when(kafkaTopicProperties.getProductAttributeFeedbackEventName()).thenReturn(TOPIC_NAME);
    when(objectMapper.readValue(TEST_MESSAGE, ProductAttributeFeedbackEventModel.class)).thenReturn(
      testEventModel);

    // Act
    attributeFeedbackEventListener.onDomainEventConsumed(TEST_MESSAGE);

    // Assert
    verify(kafkaTopicProperties).getProductAttributeFeedbackEventName();
    verify(objectMapper).readValue(TEST_MESSAGE, ProductAttributeFeedbackEventModel.class);
    verify(extractionsService).updateFeedbackForProductAttribute(testEventModel);
  }

  @Test
  void onDomainEventConsumed_GeneralException() throws Exception {
    // Arrange
    when(kafkaTopicProperties.getProductAttributeFeedbackEventName()).thenReturn(TOPIC_NAME);
    when(objectMapper.readValue(TEST_MESSAGE, ProductAttributeFeedbackEventModel.class)).thenThrow(
      new RuntimeException("Service error"));

    // Act
    attributeFeedbackEventListener.onDomainEventConsumed(TEST_MESSAGE);

    // Assert
    verify(kafkaTopicProperties).getProductAttributeFeedbackEventName();
    verify(objectMapper).readValue(TEST_MESSAGE, ProductAttributeFeedbackEventModel.class);
    verify(extractionsService, never()).updateFeedbackForProductAttribute(any());
  }
} 