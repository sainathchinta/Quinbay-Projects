package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.OfflineItemChange;
import com.gdn.x.product.service.api.ItemPickupPointWrapperService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.x.product.offlineitem.change.listener.enabled",
                       havingValue = "true")
public class OfflineItemChangeListener {

  @Autowired
  private ItemPickupPointWrapperService itemPickupPointWrapperService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = ProductDomainEventName.OFFLINE_ITEM_CHANGE_EVENT_NAME, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Offline item change event consumed with payload : {}", message);
    try {
      OfflineItemChange offlineItemChange = this.objectMapper.readValue(message,
        OfflineItemChange.class);
      this.itemPickupPointWrapperService.updateItemPickupPointOnOfflineItemChange(offlineItemChange);
    } catch (Exception e) {
      log.error("Error on consume of offline item change event with payload : {}, error - ",
        message, e);
    }
  }
}
