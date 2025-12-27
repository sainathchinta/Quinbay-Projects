package com.gdn.partners.product.analytics.service.impl.domainEvent;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.service.ProductAttributeExtractionsService;
import model.AttributeValueExtractionsEventModel;
import model.ProductAttributeExtractionsEventModel;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.util.Collections;
import java.util.Date;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoInteractions;
import static org.mockito.Mockito.when;


@ExtendWith(MockitoExtension.class)
class ProductAttributeExtractionsListenerTest {

    @Mock
    private KafkaTopicProperties kafkaTopicProperties;

    @Mock
    private ObjectMapper objectMapper;

    @Mock
    private ProductAttributeExtractionsService productAttributeExtractionsService;

    @InjectMocks
    private ProductAttributeExtractionsListener productAttributeExtractionsListener;

    private static final String VALIDATION_TOPIC = "product-attribute-extractions-validation";
    private static final String PRODUCT_ID = "test-product-id";
    private static final String PRODUCT_SKU = "test-sku";
    private static final String ATTRIBUTE_NAME = "test-attribute";
    private static final String ATTRIBUTE_VALUE = "test-value";

    @Test
    void onDomainEventConsumed_WithValidMessage_Success() throws Exception {
        // Given
        String message = "{\"productId\":\"test-product-id\"}"; // Simplified JSON for example
        ProductAttributeExtractionsEventModel eventModel = createEventModel();

        when(objectMapper.readValue(message, ProductAttributeExtractionsEventModel.class))
            .thenReturn(eventModel);

        // When
        productAttributeExtractionsListener.onDomainEventConsumed(message);

        // Then
        verify(objectMapper).readValue(message, ProductAttributeExtractionsEventModel.class);
        verify(productAttributeExtractionsService)
            .validateAndPublishPCBEventsForProductAttributeExtractions(eventModel);
    }

    @Test
    void onDomainEventConsumed_WhenJsonParsingFails_LogsError() throws Exception {
        // Given
        String invalidMessage = "invalid-json";
        when(objectMapper.readValue(invalidMessage, ProductAttributeExtractionsEventModel.class))
            .thenThrow(new RuntimeException("JSON parsing failed"));
        when(kafkaTopicProperties.getProductAttributeExtractionsValidationEventName())
            .thenReturn(VALIDATION_TOPIC);

        // When
        productAttributeExtractionsListener.onDomainEventConsumed(invalidMessage);

        // Then
        verify(objectMapper).readValue(invalidMessage, ProductAttributeExtractionsEventModel.class);
        verify(kafkaTopicProperties).getProductAttributeExtractionsValidationEventName();
        verifyNoInteractions(productAttributeExtractionsService);
    }

    @Test
    void onDomainEventConsumed_WhenServiceThrowsException_LogsError() throws Exception {
        // Given
        String message = "{\"productId\":\"test-product-id\"}";
        ProductAttributeExtractionsEventModel eventModel = createEventModel();

        when(objectMapper.readValue(message, ProductAttributeExtractionsEventModel.class))
            .thenReturn(eventModel);
        doThrow(new RuntimeException("Service error"))
            .when(productAttributeExtractionsService)
            .validateAndPublishPCBEventsForProductAttributeExtractions(eventModel);
        when(kafkaTopicProperties.getProductAttributeExtractionsValidationEventName())
            .thenReturn(VALIDATION_TOPIC);

        // When
        productAttributeExtractionsListener.onDomainEventConsumed(message);

        // Then
        verify(objectMapper).readValue(message, ProductAttributeExtractionsEventModel.class);
        verify(productAttributeExtractionsService)
            .validateAndPublishPCBEventsForProductAttributeExtractions(eventModel);
        verify(kafkaTopicProperties).getProductAttributeExtractionsValidationEventName();
    }

    private ProductAttributeExtractionsEventModel createEventModel() {
        AttributeValueExtractionsEventModel attributeModel = AttributeValueExtractionsEventModel.builder()
            .attributeName(ATTRIBUTE_NAME)
            .value(ATTRIBUTE_VALUE)
            .build();

        return ProductAttributeExtractionsEventModel.builder()
            .productId(PRODUCT_ID)
            .productSku(PRODUCT_SKU)
            .addedDate(new Date())
            .attributeValueExtractions(Collections.singletonList(attributeModel))
            .build();
    }
} 