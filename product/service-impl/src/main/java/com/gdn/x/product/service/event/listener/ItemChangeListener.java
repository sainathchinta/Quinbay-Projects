package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ItemChange;
import com.gdn.x.product.service.api.ItemPickupPointWrapperService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.Objects;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.x.product.item.change.listener.enabled", havingValue = "true")
public class ItemChangeListener {

  @Autowired
  private ItemPickupPointWrapperService itemPickupPointWrapperService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = ProductDomainEventName.ITEM_CHANGE_EVENT_NAME, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Item change event consumed with payload : {}", message);
    try {
      ItemChange itemChange = this.objectMapper.readValue(message, ItemChange.class);
      if (Objects.nonNull(itemChange)) {
        this.itemPickupPointWrapperService.updateItemPickupPointOnItemChange(itemChange);
      }
    } catch (Exception e) {
      log.error("Error on consumption of item change event. Payload : {}, error - ", message, e);
    }
  }
}
