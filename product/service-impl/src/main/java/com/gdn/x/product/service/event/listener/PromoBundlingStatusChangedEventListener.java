package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.pricing.streaming.model.DomainEventName;
import com.gdn.partners.product.pricing.streaming.model.promo.bundling.PromoBundlingStatusChangedEventModel;
import com.gdn.x.product.service.api.ItemService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.Objects;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.partners.product.pricing.promo.bundling.status.changed"
    + ".listener.enabled", havingValue = "true")
public class PromoBundlingStatusChangedEventListener {

  @Value("${promobundling.event.from.pricing.enabled}")
  private boolean promoEventPricingEnabled;

  @Autowired
  private ItemService itemService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = DomainEventName.PROMO_BUNDLING_STATUS_CHANGED_EVENT_EXTERNAL, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("PromoBundlingStatusChangedEventListener consume event with message : {}", message);
    try {
      PromoBundlingStatusChangedEventModel promoBundlingStatusChangedEventModel =
        objectMapper.readValue(message, PromoBundlingStatusChangedEventModel.class);
      if (promoEventPricingEnabled && isMessageValid(promoBundlingStatusChangedEventModel)) {
        itemService.processPromoBundlingStatusChangedEventInItemPickupPoint(promoBundlingStatusChangedEventModel.getStoreId(),
            promoBundlingStatusChangedEventModel.getMainItemSku(), promoBundlingStatusChangedEventModel.getPromoBundlingType(),
            promoBundlingStatusChangedEventModel.isPromoBundlingActivated(), promoBundlingStatusChangedEventModel.getWholesalePriceActivated());
      }
    } catch (Exception e) {
      log.error("Exception caught on processing PromoBundlingStatusChangedEvent payload :{}, "
          + "error - ", message, e);
    }
  }

  private boolean isMessageValid(PromoBundlingStatusChangedEventModel promoBundlingStatusChangedEventModel) {
    return StringUtils.isNotEmpty(promoBundlingStatusChangedEventModel.getStoreId()) && StringUtils
        .isNotEmpty(promoBundlingStatusChangedEventModel.getMainItemSku()) && StringUtils
        .isNotEmpty(promoBundlingStatusChangedEventModel.getPromoBundlingType())
        && !promoBundlingStatusChangedEventModel.isIgnore() && checkPromoBundlingFlags(
        promoBundlingStatusChangedEventModel);
  }

  private boolean checkPromoBundlingFlags(PromoBundlingStatusChangedEventModel promoBundlingStatusChangedEventModel) {
    if (Objects.nonNull(promoBundlingStatusChangedEventModel.getWholesalePriceActivated())) {
      return !(promoBundlingStatusChangedEventModel.getWholesalePriceActivated() && promoBundlingStatusChangedEventModel
          .isPromoBundlingActivated());
    }
    return true;
  }
}
