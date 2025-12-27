package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.pricing.model.enums.PromoBundlingEventType;
import com.gdn.partners.product.pricing.streaming.model.DomainEventName;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.PromoBundlingActivatedDeactivatedEventModel;
import com.gdn.x.product.service.api.ItemService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.Arrays;
import java.util.List;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.partners.product.pricing.promo.bundling.sku.activated"
    + ".deactivated.event.listener.enabled", havingValue = "true")
public class PromoBundlingActivatedDeactivatedEventListener {

  @Autowired
  private ItemService itemService;

  @Autowired
  private ObjectMapper objectMapper;

  private static List<String> changeTypeList =
      Arrays.asList(PromoBundlingEventType.PROMO_START_COMPLEMENTARY_AVAILABLE.name(),
          PromoBundlingEventType.PROMO_START_COMPLEMENTARY_UNAVAILABLE.name(),
          PromoBundlingEventType.PROMO_STOP_COMPLEMENTARY_AVAILABLE.name(),
          PromoBundlingEventType.PROMO_STOP_COMPLEMENTARY_UNAVAILABLE.name(),
          PromoBundlingEventType.PROMO_STARTED.name(), PromoBundlingEventType.PROMO_ENDED.name());


  @KafkaListener(topics = DomainEventName.PROMO_BUNDLING_SKU_ACTIVATED_DEACTIVATED_EVENT_EXTERNAL, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Consume PromoBundlingActivatedDeactivatedEventListener event : {} with message : {}",
        DomainEventName.PROMO_BUNDLING_SKU_ACTIVATED_DEACTIVATED_EVENT_EXTERNAL, message);
    try {
      PromoBundlingActivatedDeactivatedEventModel promoBundlingActivatedDeactivatedEventModel =
          objectMapper.readValue(message, PromoBundlingActivatedDeactivatedEventModel.class);
      if (!changeTypeList.contains(promoBundlingActivatedDeactivatedEventModel.getPromoBundlingEventType().name())) {
        log.info("Invalid changeType : {} itemSkus : {} merchantCode :{}",
            promoBundlingActivatedDeactivatedEventModel.getPromoBundlingEventType().name(),
            promoBundlingActivatedDeactivatedEventModel.getItemSkus(),
            promoBundlingActivatedDeactivatedEventModel.getMerchantCode());
        return;
      }
      itemService.activeDeactivatePromoBundlingInItemPickupPointByPPCode(promoBundlingActivatedDeactivatedEventModel);
    } catch (Exception e) {
      log.error(
          "Error while consuming PromoBundlingActivatedDeactivatedEventListener event with message : {}, error - ",
          message, e);
    }
  }
}
