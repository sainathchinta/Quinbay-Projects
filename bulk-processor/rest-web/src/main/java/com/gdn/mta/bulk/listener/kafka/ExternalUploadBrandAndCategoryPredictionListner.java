package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BrandAndCategoryPredictionRequest;
import com.gdn.mta.bulk.service.CategoryAndBrandPredictionService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class ExternalUploadBrandAndCategoryPredictionListner {

    @Autowired
    private KafkaTopicProperties kafkaTopicProperties;

    @Autowired
    private ObjectMapper objectMapper;

    @Autowired
    private CategoryAndBrandPredictionService categoryAndBrandPredictionService;

    @KafkaListener(topics = "#{kafkaTopicProperties.getExternalUploadCategoryAndBrandPrediction()"
        + "}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
    public void onDomainEventConsumption(String message) throws Exception {
        log.info("Consume event {} with message {} ",
            kafkaTopicProperties.getExternalUploadCategoryAndBrandPrediction(), message);
        BrandAndCategoryPredictionRequest brandAndCategoryPredictionRequest = null;
        try {
            brandAndCategoryPredictionRequest =
                objectMapper.readValue(message, BrandAndCategoryPredictionRequest.class);
            categoryAndBrandPredictionService.process(brandAndCategoryPredictionRequest);
        } catch (Exception e) {
            log.error("Error processing Kafka message | topic={} | error={} ",
                kafkaTopicProperties.getExternalUploadCategoryAndBrandPrediction(), e.getMessage(),
                e);
        }
    }

}
