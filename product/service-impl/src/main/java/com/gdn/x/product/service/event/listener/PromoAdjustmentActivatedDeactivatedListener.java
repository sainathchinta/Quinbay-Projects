package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.partners.product.pricing.streaming.model.DomainEventName;
import com.gdn.partners.product.pricing.streaming.model.PromoAdjustmentActivatedDeactivatedEvent;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.model.entity.AdjustmentType;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.service.api.ItemService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.Objects;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.partners.product.pricing.promo.adjustment.activation"
    + ".deactivation.event.listener.enabled", havingValue = "true")
public class PromoAdjustmentActivatedDeactivatedListener {

  @Autowired
  private ItemService itemService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = DomainEventName.PROMO_ADJUSTMENT_ACTIVATION_DEACTIVATION_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Promo-Adjustment : consume activation/deactivation event with message : {} ", message);
    try {
      PromoAdjustmentActivatedDeactivatedEvent promoAdjustmentActivatedDeactivatedEvent =
          this.objectMapper.readValue(message, PromoAdjustmentActivatedDeactivatedEvent.class);
      validateMerchantPromoDiscountPriceChange(promoAdjustmentActivatedDeactivatedEvent);
      DiscountPrice discountPrice = null;
      if (promoAdjustmentActivatedDeactivatedEvent.isActivated()) {
        discountPrice = new DiscountPrice(promoAdjustmentActivatedDeactivatedEvent.getActivatedDetail().getPrice(),
            promoAdjustmentActivatedDeactivatedEvent.getActivatedDetail().getStartDate(),
            promoAdjustmentActivatedDeactivatedEvent.getActivatedDetail().getEndDate(),
            promoAdjustmentActivatedDeactivatedEvent.getActivatedDetail().getPromoAdjustmentName(),
            AdjustmentType.MERCHANT);
      }
      this.itemService.updateItemMerchantDiscountPriceByItemSkuAndPPCode(
          promoAdjustmentActivatedDeactivatedEvent.getStoreId(), promoAdjustmentActivatedDeactivatedEvent.getItemSku(),
          discountPrice, promoAdjustmentActivatedDeactivatedEvent.getPickupPointCode(),
          promoAdjustmentActivatedDeactivatedEvent.isActivated());
    } catch (Exception e) {
      log.error(
          "Exception caught while activating or deactivating merchant promo discount : " + "payload : {}, error - ",
          message, e);
    }
  }

  public static void validateMerchantPromoDiscountPriceChange(
      PromoAdjustmentActivatedDeactivatedEvent promoAdjustmentActivatedDeactivatedEvent) {
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(promoAdjustmentActivatedDeactivatedEvent.getStoreId()),
        ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(StringUtils.isNotEmpty(promoAdjustmentActivatedDeactivatedEvent.getItemSku()),
        ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    if (promoAdjustmentActivatedDeactivatedEvent.isActivated()) {
      GdnPreconditions.checkArgument(Objects.nonNull(promoAdjustmentActivatedDeactivatedEvent.getActivatedDetail()),
          ErrorMessages.ACTIVATION_DETAIL_CONNOT_BE_EMPTTY);
      GdnPreconditions.checkArgument(
          Objects.nonNull(promoAdjustmentActivatedDeactivatedEvent.getActivatedDetail().getStartDate())
              && Objects.nonNull(promoAdjustmentActivatedDeactivatedEvent.getActivatedDetail().getEndDate()),
          ErrorMessages.START_DATE_AND_END_DATE_SHOULD_NOT_BE_EMPTY);
    }
    GdnPreconditions.checkArgument(
        StringUtils.isNotEmpty(promoAdjustmentActivatedDeactivatedEvent.getPickupPointCode()),
        ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
  }
}
