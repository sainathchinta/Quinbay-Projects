package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.pricing.streaming.model.DomainEventName;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.PromoBundlingActivatedEvent;
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
@ConditionalOnProperty(value = "com.gdn.x.promotion.promo.bundling.activated.listener.enabled",
                       havingValue = "true")
public class PromoBundlingActivatedEventListener {

  @Autowired
  private ItemService itemService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = DomainEventName.PROMO_BUNDLING_ACTIVATED_EVENT_NAME, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Consume event with message : {}", message);
    try {
      PromoBundlingActivatedEvent promoBundlingActivated =
        objectMapper.readValue(message, PromoBundlingActivatedEvent.class);
      if (this.isMessageValid(promoBundlingActivated)) {
        this.itemService.addActivePromoBundling(promoBundlingActivated.getStoreId(),
            promoBundlingActivated.getMainItemSku(), promoBundlingActivated.getPromoBundlingType());
      }
    } catch (Exception e) {
      log.error("Error while consuming event with message : {}, error - ", message, e);
    }
  }

  private boolean isMessageValid(PromoBundlingActivatedEvent promoBundlingActivated) {
    return StringUtils.isNotEmpty(promoBundlingActivated.getStoreId()) &&
        StringUtils.isNotEmpty(promoBundlingActivated.getMainItemSku()) &&
        StringUtils.isNotEmpty(promoBundlingActivated.getPromoBundlingType());
  }
}
