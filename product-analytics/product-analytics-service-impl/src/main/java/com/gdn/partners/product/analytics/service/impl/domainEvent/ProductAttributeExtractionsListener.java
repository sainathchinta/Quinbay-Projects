package com.gdn.partners.product.analytics.service.impl.domainEvent;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.service.ProductAttributeExtractionsService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import model.ProductAttributeExtractionsEventModel;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@RequiredArgsConstructor
public class ProductAttributeExtractionsListener {

  private final KafkaTopicProperties kafkaTopicProperties;
  private final ObjectMapper objectMapper;
  private final ProductAttributeExtractionsService productAttributeExtractionsService;

  @KafkaListener(topics = "#{kafkaTopicProperties.getProductAttributeExtractionsValidationEventName()}", autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    try {
      ProductAttributeExtractionsEventModel productAttributeExtractionsEventModel =
          objectMapper.readValue(message, ProductAttributeExtractionsEventModel.class);
      productAttributeExtractionsService.validateAndPublishPCBEventsForProductAttributeExtractions(
          productAttributeExtractionsEventModel);
    } catch (Exception e){
      log.error("Error while listening to the event:{}, with payload:{} , e - ",
          kafkaTopicProperties.getProductAttributeExtractionsValidationEventName(), message, e);
    }
  }
}