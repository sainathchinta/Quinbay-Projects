package com.gdn.partners.product.analytics.service.impl.domainEvent;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.service.ProductAttributeExtractionsService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import model.ProductAttributeFeedbackEventModel;
import org.slf4j.MDC;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@RequiredArgsConstructor
public class ProductAttributeFeedbackEventListener {

  private final KafkaTopicProperties kafkaTopicProperties;
  private final ObjectMapper objectMapper;
  private final ProductAttributeExtractionsService extractionsService;

  /**
   * Consumes attribute feedback events from Kafka and processes them.
   *
   * @param message The JSON message containing the attribute feedback event
   */

  @KafkaListener(topics = "#{kafkaTopicProperties.getProductAttributeFeedbackEventName()}",
    autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    String topic = kafkaTopicProperties.getProductAttributeFeedbackEventName();
    log.info("Listened message from topic : {}, message: {}", topic, message);
    try {
      ProductAttributeFeedbackEventModel eventModel =
        objectMapper.readValue(message, ProductAttributeFeedbackEventModel.class);
      MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, eventModel.getUsername());
      extractionsService.updateFeedbackForProductAttribute(eventModel);
    } catch (Exception e) {
      log.error("Error occurred while listening to event: {} , message: {} error - ", topic,
        message, e);
    }
  }
}
