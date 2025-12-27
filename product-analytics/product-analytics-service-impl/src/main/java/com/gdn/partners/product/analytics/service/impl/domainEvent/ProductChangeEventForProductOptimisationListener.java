package com.gdn.partners.product.analytics.service.impl.domainEvent;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.service.ProductOptimisationService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import model.ProductChangeEventModel;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@RequiredArgsConstructor
public class ProductChangeEventForProductOptimisationListener {


  private final KafkaTopicProperties kafkaTopicProperties;
  private final ObjectMapper objectMapper;
  private final ProductOptimisationService productOptimisationService;

  @KafkaListener(topics = "#{kafkaTopicProperties.getProductChangeEventName()}", autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Listened message from topic : {} , message : {} ",
        kafkaTopicProperties.getProductChangeEventName(), message);
    try {
      ProductChangeEventModel productChangeEventModel =
          objectMapper.readValue(message, ProductChangeEventModel.class);
      productOptimisationService.removeDeletedProduct(productChangeEventModel);
    } catch (Exception e) {
      log.error("Error occurred while listening to event = {} , message = {} error - ",
          kafkaTopicProperties.getProductChangeEventName(), message, e);
    }
  }
}