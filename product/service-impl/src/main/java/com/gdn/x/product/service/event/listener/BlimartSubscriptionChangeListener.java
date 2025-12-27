package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.BlimartSubscriptionChangeRequest;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemService;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@ConditionalOnProperty(value = "com.gdn.subscription.product.activated.listener.enabled",
                       havingValue = "true")
public class BlimartSubscriptionChangeListener  {

  private static final String ITEM_SKU_EMPTY = "item sku must not be empty";

  @Autowired
  private ItemService itemService;

  @Autowired
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private ObjectMapper objectMapper;

  @Value("${subscription.l5.flow}")
  private boolean subscriptionAtL5Flow;

  @KafkaListener(topics = ProductDomainEventName.BLIMART_SUBSCRIPTION_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("BLIMART_SUBSCRIPTION_EVENT listened for message : {}", message);
    try {
      BlimartSubscriptionChangeRequest blimartSubscriptionChangeRequest =
        this.objectMapper.readValue(message, BlimartSubscriptionChangeRequest.class);
      if (subscriptionAtL5Flow) {
        GdnPreconditions.checkArgument(StringUtils.isNotEmpty(blimartSubscriptionChangeRequest.getItemSku()),
            ITEM_SKU_EMPTY);
        GdnPreconditions.checkArgument(StringUtils.isNotEmpty(blimartSubscriptionChangeRequest.getPickupPointCode()),
            ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
        itemPickupPointService.updateSubscriptionFlag(blimartSubscriptionChangeRequest);
        log.info("Subscription flag updated, for payload : {} ", message);
      } else {
        GdnPreconditions.checkArgument(StringUtils.isNotEmpty(blimartSubscriptionChangeRequest.getItemSku()),
            ITEM_SKU_EMPTY);
        this.itemService.updateSubscriptionFlagByItemSku(blimartSubscriptionChangeRequest.getStoreId(),
            blimartSubscriptionChangeRequest.getItemSku(), blimartSubscriptionChangeRequest.isSubscribable(),
            blimartSubscriptionChangeRequest.getPreferredSubscriptionType());
      }
    } catch (Exception e) {
      log.error("Error caught while updating subscription flag, payload : {}, error - ", message,
        e);
    }
  }
}
