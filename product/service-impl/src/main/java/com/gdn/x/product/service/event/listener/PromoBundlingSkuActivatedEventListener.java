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
 * Created by w.william on 12/6/2017.
 */
@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.x.promotion.promo.bundling.sku.activated.listener"
    + ".enabled", havingValue = "true")
public class PromoBundlingSkuActivatedEventListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ItemService itemService;

  @KafkaListener(topics = DomainEventName.PROMO_BUNDLING_SKU_ACTIVATED_EVENT_NAME, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("PromoBundlingSkuActivatedEventListener consume event with payload : {}", message);
    try {
      PromoBundlingSkuChanged promoBundlingSkuChanged = this.objectMapper.readValue(message,
        PromoBundlingSkuChanged.class);
      if (StringUtils.isNotBlank(promoBundlingSkuChanged.getStoreId()) && CollectionUtils.isNotEmpty(promoBundlingSkuChanged.getSkuList())) {
          Set<String> itemSkuSet = new HashSet<>(promoBundlingSkuChanged.getSkuList());
        itemService.updatePromoBundlingByItemSkusInItemPickupPoint(promoBundlingSkuChanged.getStoreId(), itemSkuSet,
            true);
      }
    } catch (Exception ex) {
      log.error("Error {}, while Event listening PromoBundling Activation from x-promotion , item "
          + "payload: {}, error - ", ex.getMessage(), message, ex);
    }
  }
}
