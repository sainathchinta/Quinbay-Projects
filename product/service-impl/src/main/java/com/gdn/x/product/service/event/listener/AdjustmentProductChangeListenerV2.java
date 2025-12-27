package com.gdn.x.product.service.event.listener;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import com.gdn.x.product.enums.RetryPublishStatus;
import com.gdn.x.product.model.entity.ProductRetryEventPublish;
import com.gdn.x.product.service.api.ProductRetryEventPublishService;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Lazy;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.enums.SystemParameterNames;
import com.gdn.x.product.model.entity.AdjustmentType;
import com.gdn.x.product.model.entity.DiscountPrice;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ProductAndItemSolrIndexerService;
import com.gdn.x.product.service.api.SystemParameterService;
import com.gdn.x.promotion.domain.event.config.PromotionDomainEventName;
import com.gdn.x.promotion.domain.event.model.AdjustmentProductChange;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.x.promotion.adjustment.product.change.v2", havingValue = "true")
public class AdjustmentProductChangeListenerV2 {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  @Lazy
  private ItemPickupPointService itemPickupPointService;

  @Autowired
  private ProductAndItemSolrIndexerService productAndItemSolrIndexerService;

  @Autowired
  private SystemParameterService systemParameterService;

  @Autowired
  @Lazy
  private ProductRetryEventPublishService retryEventPublishService;

  @Value("${v2.enabled}")
  private boolean v2Enabled;

  @Value("${blacklist.seller.list.l3.reindex}")
  private String blackListSellersForL3Reindex;

  @Value("${blacklist.promo.adjustment.reindex.enabled}")
  private boolean blacklistPromoAdjustmentReindexEnabled;

  @KafkaListener(topics = PromotionDomainEventName.ADJUSTMENT_PRODUCT_CHANGE_EVENT_NAME_V2, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Listened to product adjustment event v2 : {}", message);
    AdjustmentProductChange adjustmentProductChange = null;
    try {
      if (isEventProcessingDisabled()) {
        log.info("V2 events are disabled : {}", message);
        return;
      }
       adjustmentProductChange =
        this.objectMapper.readValue(message, AdjustmentProductChange.class);
      processAdjustmentEventV2(adjustmentProductChange);
    } catch (Exception ex) {
      log.error("Error with Adjustment processing for change event ; {} , proceeding with retry",
        message, ex);
        ProductRetryEventPublish productRetryEventPublish =
          ProductRetryEventPublish.builder().clearCache(Boolean.FALSE).retryCount(0).identifier(
              Optional.ofNullable(adjustmentProductChange).map(AdjustmentProductChange::getProductSku)
                .orElse(null)).secondaryIdentifier(Optional.ofNullable(adjustmentProductChange)
              .map(AdjustmentProductChange::getPickupPointCode).orElse(null))
            .retryPublishStatus(RetryPublishStatus.PENDING)
            .topicName(PromotionDomainEventName.ADJUSTMENT_PRODUCT_CHANGE_EVENT_NAME_V2).build();
        this.retryEventPublishService.insertToRetryPublish(productRetryEventPublish);
        log.info("Added the adjustment Event for Retry with Model : {} ", productRetryEventPublish);
    }
  }

  public ItemPickupPoint processAdjustmentEventV2(AdjustmentProductChange adjustmentProductChange) throws Exception {
    validateAdjustmentProductChange(adjustmentProductChange);
    ItemPickupPoint itemPickupPoint;
    itemPickupPoint = getItemPickupPoint(adjustmentProductChange);
    if (Objects.nonNull(itemPickupPoint)) {
      if (adjustmentProductChange.isActivated()) {
        addAdjustmentToItemPickupPoint(adjustmentProductChange, itemPickupPoint);
      } else {
        removeAdjustmentFromItemPickupPoint(adjustmentProductChange, itemPickupPoint);
      }
      itemPickupPointService.saveItemPickupPoint(itemPickupPoint);
      Map<String, List<String>> productSkuMap = new HashMap<>();
      productSkuMap.put(itemPickupPoint.getProductSku(),
          new ArrayList<>(Arrays.asList(itemPickupPoint.getItemSku())));
      Map<String, String> productSkuToMerchantCodeMap = new HashMap<>();
      productSkuToMerchantCodeMap.put(itemPickupPoint.getProductSku(), itemPickupPoint.getMerchantCode());
      if (!blacklistPromoAdjustmentReindexEnabled || Stream.of(blackListSellersForL3Reindex.split(",")).noneMatch(
          blackListedSellerCode -> adjustmentProductChange.getProductSku().startsWith(blackListedSellerCode))) {
        this.productAndItemSolrIndexerService.updateSolrOnPromoFlagChangeByItemSkus(productSkuMap,
            adjustmentProductChange.isActivated(), null, productSkuToMerchantCodeMap);
      }
    }
    return itemPickupPoint;
  }

  private boolean isEventProcessingDisabled() {
    return !(v2Enabled && Boolean.parseBoolean(
        systemParameterService.findValueByStoreIdAndVariable(Constants.DEFAULT_STORE_ID,
            SystemParameterNames.LISTEN_TO_ADJUSTMENT_PRODUCT_CHANGE_EVENT).getValue()));
  }

  private ItemPickupPoint getItemPickupPoint(AdjustmentProductChange adjustmentProductChange) {
    ItemPickupPoint itemPickupPoint;
    if (isL5Event(adjustmentProductChange)) {
      itemPickupPoint = itemPickupPointService.findByItemSkuAndPickupPointCodeFromDb(Constants.DEFAULT_STORE_ID,
          adjustmentProductChange.getProductSku(), adjustmentProductChange.getPickupPointCode());
      itemPickupPoint = itemPickupPoint.isMarkForDelete() ? null : itemPickupPoint;
    } else {
      itemPickupPoint = itemPickupPointService.findByItemSkuAndDelivery(Constants.DEFAULT_STORE_ID,
          adjustmentProductChange.getProductSku());
    }
    return itemPickupPoint;
  }

  private void removeAdjustmentFromItemPickupPoint(AdjustmentProductChange adjustmentProductChange,
      ItemPickupPoint itemPickupPoint) {
    itemPickupPoint.getPrice().stream().map(Price::getListOfDiscountPrices).forEach(
        listOfDiscountPrices -> listOfDiscountPrices.removeAll(listOfDiscountPrices.stream()
            .filter(discountPrice -> isCampaignCodeSame(adjustmentProductChange, discountPrice))
            .collect(Collectors.toList())));
  }

  private void addAdjustmentToItemPickupPoint(AdjustmentProductChange adjustmentProductChange,
      ItemPickupPoint itemPickupPoint) {
    List<DiscountPrice> existingDiscounts =
        itemPickupPoint.getPrice().stream().map(Price::getListOfDiscountPrices).flatMap(List::stream)
            .filter(discountPrice -> isCampaignCodeSame(adjustmentProductChange, discountPrice))
            .collect(Collectors.toList());
    if (CollectionUtils.isEmpty(existingDiscounts)) {
      DiscountPrice newDiscountPrice = new DiscountPrice();
      setAdjustmentValuesInDiscountPrice(adjustmentProductChange, newDiscountPrice);
      itemPickupPoint.getPrice().forEach(price -> price.getListOfDiscountPrices().add(newDiscountPrice));
    } else {
      existingDiscounts.forEach(
          discountPrice -> setAdjustmentValuesInDiscountPrice(adjustmentProductChange, discountPrice));
    }
  }

  private static boolean isCampaignCodeSame(AdjustmentProductChange adjustmentProductChange,
      DiscountPrice discountPrice) {
    return StringUtils.equals(discountPrice.getCampaignCode(), adjustmentProductChange.getCampaignCode());
  }

  private static boolean isL5Event(AdjustmentProductChange adjustmentProductChange) {
    return StringUtils.isNotBlank(adjustmentProductChange.getPickupPointCode());
  }

  private static void validateAdjustmentProductChange(AdjustmentProductChange adjustmentProductChange) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(adjustmentProductChange.getProductSku()),
        ErrorMessages.ITEM_SKU_MUST_NOT_BE_BLANK);
    GdnPreconditions.checkArgument(Objects.nonNull(adjustmentProductChange.getStartDate()) && Objects.nonNull(
        adjustmentProductChange.getEndDate()), ErrorMessages.START_DATE_AND_END_DATE_SHOULD_NOT_BE_EMPTY);
  }

  private static void setAdjustmentValuesInDiscountPrice(AdjustmentProductChange adjustmentProductChange,
      DiscountPrice discountPrice) {
    discountPrice.setDiscountPrice(adjustmentProductChange.getValue());
    discountPrice.setStartDateTime(adjustmentProductChange.getStartDate());
    discountPrice.setEndDateTime(adjustmentProductChange.getEndDate());
    discountPrice.setAdjustmentName(adjustmentProductChange.getAdjustmentName());
    discountPrice.setCampaignCode(adjustmentProductChange.getCampaignCode());
    discountPrice.setAdjustmentType(AdjustmentType.BLIBLI);
    discountPrice.setPriority(adjustmentProductChange.getPriority());
    discountPrice.setExclusiveProduct(adjustmentProductChange.isExclusiveProduct());
  }

}
