package com.gdn.x.product.service.event.listener;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.model.PricingPwpPromoEvent;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.properties.KafkaTopicProperties;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@ConditionalOnProperty(value = "com.gdn.x.product.pwp.promo.enabled", havingValue = "true")
public class PricingPwpPromoSkuEventListener {

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @KafkaListener(topics = "#{kafkaTopicProperties.getPwpPromoSkuEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Pricing pwp event consumed : {} consume event for payload: {}",
        kafkaTopicProperties.getPwpPromoSkuEvent(), message);
    try {
      PricingPwpPromoEvent pricingPwpPromoEvent = this.objectMapper.readValue(message, PricingPwpPromoEvent.class);
      itemPickupPointService.updatePwpFlagsByItemSkuAndPickupPointCode(pricingPwpPromoEvent);
    } catch (Exception ex) {
      log.error("Error while listening Pricing PWP event : {} for payload: {}",
          kafkaTopicProperties.getPwpPromoSkuEvent(), message, ex);
    }
  }
}
