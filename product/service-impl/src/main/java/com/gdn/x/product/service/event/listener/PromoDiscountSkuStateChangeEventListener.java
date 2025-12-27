package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.partners.product.pricing.streaming.model.DomainEventName;
import com.gdn.partners.product.pricing.streaming.model.PromoSkuStateChangeEvent;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.service.api.ItemService;
import lombok.extern.slf4j.Slf4j;

/**
 * Created by govind on 02/05/2019 AD.
 */
@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.partners.product.pricing.promo.sku.state.change.listener"
    + ".enabled", havingValue = "true")
public class PromoDiscountSkuStateChangeEventListener {

  @Autowired
  private ItemService itemService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = DomainEventName.PROMO_SKU_STATE_CHANGE_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("PromoSkuStateChangeEvent : consume event with promoSkuStateChangeEvent : {}", message);
    try {
      PromoSkuStateChangeEvent promoSkuStateChangeEvent = this.objectMapper.readValue(message,
        PromoSkuStateChangeEvent.class);
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(promoSkuStateChangeEvent.getStoreId()),
          ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(promoSkuStateChangeEvent.getItemSku()),
          ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(promoSkuStateChangeEvent.getPickupPointCode()),
          ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
      log.debug("Event listening  for PromoSkuStateChangeEvent , item sku: {}", promoSkuStateChangeEvent.getItemSku());
      this.itemService.updateMerchantPromoDiscountFlagByItemSkuAndPPCode(promoSkuStateChangeEvent.getStoreId(),
          promoSkuStateChangeEvent.getItemSku(), promoSkuStateChangeEvent.isPromoActive(),
          promoSkuStateChangeEvent.getPickupPointCode());
    } catch (Exception ex) {
      log.error("Error while Event listening for PromoSkuStateChangeEvent , itemsku: {}", message,
        ex);
    }
  }
}
