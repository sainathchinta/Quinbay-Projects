package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.pricing.streaming.model.DomainEventName;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.PromoBundlingDeactivatedEvent;
import com.gdn.x.product.service.api.ItemService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

/**
 * @author bryan.arista
 */
@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.x.promotion.promo.bundling.deactivated.listener.enabled",
                       havingValue = "true")
public class PromoBundlingDeactivatedEventListener {

  @Autowired
  private ItemService itemService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = DomainEventName.PROMO_BUNDLING_DEACTIVATED_EVENT_NAME, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Consume event with message : {}", message);
    try {
      PromoBundlingDeactivatedEvent promoBundlingDeactivated = this.objectMapper.readValue(message,
        PromoBundlingDeactivatedEvent.class);
      if (isMessageValid(promoBundlingDeactivated)) {
        this.itemService.removeActivePromoBundling(promoBundlingDeactivated.getStoreId(),
            promoBundlingDeactivated.getSku(), promoBundlingDeactivated.getPromoBundlingType());
      }
    } catch (Exception e) {
      log.error("Error while consuming event with message : {}, error - ",
        message, e);
    }
  }

  private boolean isMessageValid(PromoBundlingDeactivatedEvent promoBundlingDeactivated) {
    return StringUtils.isNotEmpty(promoBundlingDeactivated.getStoreId()) &&
        StringUtils.isNotEmpty(promoBundlingDeactivated.getSku()) &&
        StringUtils.isNotEmpty(promoBundlingDeactivated.getPromoBundlingType());
  }
}
