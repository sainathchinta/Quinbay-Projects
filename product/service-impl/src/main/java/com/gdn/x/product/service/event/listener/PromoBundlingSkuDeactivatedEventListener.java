package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.pricing.streaming.model.DomainEventName;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.PromoBundlingSkuChanged;
import com.gdn.x.product.service.api.ItemService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.HashSet;
import java.util.Set;

/**
 * Created by w.william on 12/8/2017.
 */
@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.x.promotion.promo.bundling.sku.deactivated.listener"
    + ".enabled", havingValue = "true")
public class PromoBundlingSkuDeactivatedEventListener {

  @Autowired
  private ItemService itemService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = DomainEventName.PROMO_BUNDLING_SKU_DEACTIVATED_EVENT_NAME, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("PromoBundlingSkuDeactivatedEventListener consume event with payload : {}", message);
    try {
      PromoBundlingSkuChanged promoBundlingSkuChanged =
        this.objectMapper.readValue(message, PromoBundlingSkuChanged.class);
      if (StringUtils.isNotBlank(promoBundlingSkuChanged.getStoreId()) && CollectionUtils.isNotEmpty(promoBundlingSkuChanged.getSkuList())) {
        Set<String> itemSkuSet = new HashSet<>(promoBundlingSkuChanged.getSkuList());
        itemService.updatePromoBundlingByItemSkusInItemPickupPoint(promoBundlingSkuChanged.getStoreId(), itemSkuSet,
            false);
      }
    } catch (Exception ex) {
      log.error(
        "Error while Event listening PromoBundling Deactivation from x-promotion , payload: {}, "
          + "error - ", message, ex);
    }
  }
}
