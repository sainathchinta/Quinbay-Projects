package com.gdn.partners.product.analytics.service.impl.domainEvent;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.service.DsExtractedAttributeService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import model.AttributeUpdateEventModel;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@RequiredArgsConstructor
public class DsExtractedAttributeListener {

  private final KafkaTopicProperties kafkaTopicProperties;
  private final ObjectMapper objectMapper;

  private final DsExtractedAttributeService dsExtractedAttributeService;

  @KafkaListener(topics = "#{kafkaTopicProperties.getAttributeChangeEventName()}", autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Listened message from topic : {} , message : {} ",
        kafkaTopicProperties.getAttributeChangeEventName(), message);
    try {
      AttributeUpdateEventModel attributeUpdateEventModel =
          objectMapper.readValue(message, AttributeUpdateEventModel.class);
      dsExtractedAttributeService.updateDsExtractedAttribute(attributeUpdateEventModel);
    } catch (Exception e) {
      log.error("Error occurred while listening to event , message = {} error - ", message, e);
    }
  }

}