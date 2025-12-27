package com.gdn.partners.product.analytics.service.impl.domainEvent;

import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.service.ProductOptimisationService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@RequiredArgsConstructor
public class SellerLevelCacheClearForProductOptimisationListener {

  private final KafkaTopicProperties kafkaTopicProperties;
  private final ProductOptimisationService productOptimisationService;

  @KafkaListener(topics = "#{kafkaTopicProperties.getSellerCacheClearEventName()}", autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Listened message from topic : {} , message : {} ",
      kafkaTopicProperties.getSellerCacheClearEventName(), message);
    try {
      productOptimisationService.clearSellerLevelCache(message);
    } catch (Exception e) {
      log.error("Error occurred while listening to event = {} , message = {} error - ",
        kafkaTopicProperties.getSellerCacheClearEventName(), message, e);
    }
  }
}
