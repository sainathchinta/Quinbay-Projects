package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ItemPickupPointMigrationEvent;
import com.gdn.x.product.service.api.ItemPickupPointWrapperService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.x.product.item.migration.listener.enabled",
                       havingValue = "true")
public class ItemPickupPointMigrationListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ItemPickupPointWrapperService itemPickupPointWrapperService;

  @KafkaListener(topics = ProductDomainEventName.ITEM_MIGRATION_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Received message for Item L5 migration, payload : {}", message);
    try {
      ItemPickupPointMigrationEvent itemPickupPointMigrationEvent =
        this.objectMapper.readValue(message, ItemPickupPointMigrationEvent.class);
      GdnPreconditions.checkArgument(CollectionUtils.isNotEmpty(itemPickupPointMigrationEvent.getItemSkuList()),
        ErrorMessages.ITEM_LIST_NOT_EMPTY);
      this.itemPickupPointWrapperService.processItemMigrationEvent(itemPickupPointMigrationEvent);
    } catch (Exception e) {
      log.error("Error on consuming item migration event, error - ", e);
    }
  }
}
