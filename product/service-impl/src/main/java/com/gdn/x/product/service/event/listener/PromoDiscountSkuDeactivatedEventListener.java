package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.partners.product.pricing.streaming.model.DomainEventName;
import com.gdn.partners.product.pricing.streaming.model.PromoAdjustmentChangeEvent;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.model.entity.Item;
import com.gdn.x.product.service.api.ItemCacheableService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.util.CommonUtil;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.Objects;

/**
 * Created by govind on 16/04/2019 AD.
 */
@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.partners.product.pricing.promo.adjustment.deactivation"
    + ".event.listener.enabled", havingValue = "true")
public class PromoDiscountSkuDeactivatedEventListener {

  @Autowired
  private ItemService itemService;

  @Autowired
  private ItemCacheableService itemCacheableService;

  @Autowired
  private ObjectMapper objectMapper;

  @Deprecated
  @KafkaListener(topics = DomainEventName.PROMO_ADJUSTMENT_DEACTIVATION_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Promo-Discount : consume event with promoAdjustmentChangeEvent : {}", message);
    try {
      PromoAdjustmentChangeEvent promoAdjustmentChangeEvent = this.objectMapper.readValue(message,
        PromoAdjustmentChangeEvent.class);
      CommonUtil.validateMerchantPromoDiscountPriceChange(promoAdjustmentChangeEvent);
      GdnPreconditions.checkArgument(!promoAdjustmentChangeEvent.isActivated(),
          ErrorMessages.PROMO_DISCOUNT_EVENT_STATUS_MUST_NOT_BE_ACTIVE);
      log.debug("Event listening  from Promo-Discount , item sku: {}", promoAdjustmentChangeEvent.getItemSku());
      Item item = itemCacheableService
          .findItemByStoreIdAndItemSku(promoAdjustmentChangeEvent.getStoreId(), promoAdjustmentChangeEvent.getItemSku(), Boolean.TRUE, Boolean.FALSE, false,
              null, false);
      GdnPreconditions.checkArgument(Objects.nonNull(item), ErrorMessages.ITEM_MUST_NOT_BE_NULL);
      item.getPrice().forEach(price -> price.setMerchantPromoDiscountPrice(null));
      this.itemService.updateItemMerchantDiscountPrice(promoAdjustmentChangeEvent.getStoreId(), item);
    } catch (Exception ex) {
      log.error(
          "Error while Event listening Promo-Discount Deactivation from Promo-Discount , payload "
            + ": {}, error - ", message, ex);
    }
  }
}
