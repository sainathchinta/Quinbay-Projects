package com.gdn.x.product.service.event.listener;


import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.MigrationForItemCreationEvent;
import com.gdn.x.product.enums.ItemMigrationStatus;
import com.gdn.x.product.service.api.ItemPickupPointMigrationService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.x.product.migration.for.creation.listener.enabled",
                       havingValue = "true")
public class MigrationForCreationListener {

  @Autowired
  private ItemPickupPointMigrationService itemPickupPointMigrationService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = ProductDomainEventName.MIGRATION_FOR_CREATION_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Received event for topic : {}, payload : {}",
      ProductDomainEventName.MIGRATION_FOR_CREATION_EVENT, message);
    try {
      MigrationForItemCreationEvent migrationForItemCreationEvent =
        this.objectMapper.readValue(message, MigrationForItemCreationEvent.class);
      this.itemPickupPointMigrationService.insertItemSkuByState(migrationForItemCreationEvent.getItemSkuList(),
        ItemMigrationStatus.PENDING.name());
    } catch (Exception e) {
      log.error("Exception on consumption of message : {}, event : {}, error - ", message,
        ProductDomainEventName.MIGRATION_FOR_CREATION_EVENT, e);
    }
  }
}
