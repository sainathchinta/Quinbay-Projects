package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.pricing.streaming.model.DomainEventName;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.PromoBundlingSkuCreationExpiredEventModel;
import com.gdn.x.product.service.api.ItemService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.partners.product.pricing.promo.bundling.sku.creation"
    + ".expired.event.listener.enabled", havingValue = "true")
public class PromoBundlingFlagUpdateListener {

  @Autowired
  private ItemService itemService;

  @Autowired
  private ObjectMapper objectMapper;


  @KafkaListener(topics = DomainEventName.PROMO_BUNDLING_SKU_CREATION_EXPIRED_EVENT_EXTERNAL, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Listened to event : {} and payload : {}", DomainEventName.PROMO_BUNDLING_SKU_CREATION_EXPIRED_EVENT_EXTERNAL, message);
    try {
      PromoBundlingSkuCreationExpiredEventModel promoBundlingSkuCreationExpiredEventModel =
          this.objectMapper.readValue(message, PromoBundlingSkuCreationExpiredEventModel.class);
      if (CollectionUtils.isNotEmpty(promoBundlingSkuCreationExpiredEventModel.getItemInfoList())) {
        itemService.updatePromoBundlingByItemSkusInItemPickupPointByItemInfo(
            promoBundlingSkuCreationExpiredEventModel.getStoreId(),
            promoBundlingSkuCreationExpiredEventModel.getItemInfoList(),
            promoBundlingSkuCreationExpiredEventModel.isPromoBundlingActive());
      }
    } catch (Exception ex) {
      log.error("Exception caught while updating  promo bundling flag : {} ", message, ex);
    }
  }

}
