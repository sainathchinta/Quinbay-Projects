package com.gdn.partners.product.analytics.service.impl.domainEvent;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.service.ProductOptimisationService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import model.ProductOptimisationEventModel;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.Objects;

@RequiredArgsConstructor
@Service
@Slf4j
public class ProductOptimisationListener {

  private final KafkaTopicProperties kafkaTopicProperties;
  private final ObjectMapper objectMapper;
  private final ProductOptimisationService productOptimisationService;

  @KafkaListener(topics = "#{kafkaTopicProperties.getProductOptimisationEventName()}",
    autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Listened message from topic : {} , message : {} ",
      kafkaTopicProperties.getProductOptimisationEventName(), message);
    try {
      ProductOptimisationEventModel productOptimisationEventModel =
        objectMapper.readValue(message, ProductOptimisationEventModel.class);
      if (Objects.nonNull(productOptimisationEventModel)) {
        productOptimisationService.upsertProductOptimisationData(productOptimisationEventModel);
      }
    } catch (Exception e) {
      log.error("Error occurred while listening to event {} , message = {} error - ",
        kafkaTopicProperties.getProductOptimisationEventName(), message, e);
    }
  }
}
