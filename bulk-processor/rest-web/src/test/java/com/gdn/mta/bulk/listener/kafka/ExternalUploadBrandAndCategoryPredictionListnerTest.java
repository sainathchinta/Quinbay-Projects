package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BrandAndCategoryPredictionRequest;
import com.gdn.mta.bulk.service.CategoryAndBrandPredictionService;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class ExternalUploadBrandAndCategoryPredictionListnerTest {
    @InjectMocks
    private ExternalUploadBrandAndCategoryPredictionListner listner;

    @Mock
    private ObjectMapper objectMapper;

    @Mock
    private KafkaTopicProperties kafkaTopicProperties;

    @Mock
    private CategoryAndBrandPredictionService categoryAndBrandPredictionService;

    private final String topic = "test-topic";

    @BeforeEach
    void setUp() {
        MockitoAnnotations.initMocks(this);
        when(kafkaTopicProperties.getExternalUploadCategoryAndBrandPrediction()).thenReturn(topic);
    }

    @Test
    void testOnDomainEventConsumption_HappyFlow() throws Exception {
        String message = "{\"stroeId\":\"storeId\",\"bulkProcessId\":\"bulkProcessId\","
            + "\"bulkProcessCode\":\"bulkProcessCode\","
            + "\"parentProduct\":\"parentProduct\"\"externalCategory\":\"externalCategory\","
            + "\"productName\":\"productName\",\"productDescription\":\"productDescription\"}";

        BrandAndCategoryPredictionRequest request = new BrandAndCategoryPredictionRequest();
        when(objectMapper.readValue(message, BrandAndCategoryPredictionRequest.class)).thenReturn(
            request);

        listner.onDomainEventConsumption(message);

        verify(objectMapper).readValue(message, BrandAndCategoryPredictionRequest.class);
        verify(categoryAndBrandPredictionService).process(request);
    }

    @Test
    void testOnDomainEventConsumption_processThrowsException() throws Exception {
        String message = "{\"stroeId\":\"storeId\",\"bulkProcessId\":\"bulkProcessId\","
            + "\"bulkProcessCode\":\"bulkProcessCode\","
            + "\"parentProduct\":\"parentProduct\"\"externalCategory\":\"externalCategory\","
            + "\"productName\":\"productName\",\"productDescription\":\"productDescription\"}";

        BrandAndCategoryPredictionRequest request = new BrandAndCategoryPredictionRequest();
        when(objectMapper.readValue(message, BrandAndCategoryPredictionRequest.class)).thenReturn(
            request);
        doThrow(new RuntimeException("processing failed")).when(categoryAndBrandPredictionService)
            .process(request);

        listner.onDomainEventConsumption(message);

        verify(categoryAndBrandPredictionService).process(request);
    }
}
