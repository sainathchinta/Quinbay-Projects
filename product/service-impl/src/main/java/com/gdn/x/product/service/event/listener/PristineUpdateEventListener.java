package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.ext.catalog.config.ExtCatalogDomainEventName;
import com.gdn.ext.catalog.model.entity.BlibliUpdateChangeEventModel;
import com.gdn.x.product.domain.event.enums.PristineEventActionType;
import com.gdn.x.product.service.api.ItemService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.Objects;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.ext.catalog.update.pristine.product.listener.enabled",
                       havingValue = "true")
public class PristineUpdateEventListener {

  @Autowired
  private ItemService itemService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = ExtCatalogDomainEventName.PRISTINE_PRODUCT_UPDATE_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Listened to event : {} - {}", ExtCatalogDomainEventName.PRISTINE_PRODUCT_UPDATE_EVENT,
      message);
    try {
      BlibliUpdateChangeEventModel blibliUpdateChangeEventModel =
        this.objectMapper.readValue(message, BlibliUpdateChangeEventModel.class);
      if (PristineEventActionType.DPC_UPDATE.name()
        .equals(blibliUpdateChangeEventModel.getEventActionType().name())) {
        itemService.updatePristineDPC(null, blibliUpdateChangeEventModel.getPristineMasterId(),
          blibliUpdateChangeEventModel.getDefaultProductCode());
      } else if (PristineEventActionType.MASTER_DPC_UPDATE.name()
        .equals(blibliUpdateChangeEventModel.getEventActionType().name()) && Objects.nonNull(
        blibliUpdateChangeEventModel.getPcbProductItemId())) {
        itemService.updatePristineDPC(blibliUpdateChangeEventModel.getPcbProductItemId(),
          blibliUpdateChangeEventModel.getPristineMasterId(),
          blibliUpdateChangeEventModel.getDefaultProductCode());
      }
    } catch (Exception e) {
      log.error("Error while Event listening PRISTINE_PRODUCT_UPDATE_EVENT from Ext-catalog , "
        + "event: {} ,", message, e);
    }
  }
}
