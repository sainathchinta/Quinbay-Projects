package com.gdn.x.product.service.impl;

import com.gdn.x.product.model.entity.ItemBuyableSchedule;
import com.gdn.x.product.model.entity.ItemDiscoverableSchedule;
import com.gdn.x.product.model.entity.ItemPickupPoint;
import com.gdn.x.product.model.entity.ItemViewConfig;
import com.gdn.x.product.model.entity.Price;
import com.gdn.x.product.rest.web.model.response.AutoCreatePickupPointResponse;
import com.gdn.x.product.service.api.ChannelService;
import com.gdn.x.product.service.api.ItemPickupPointHelperService;
import com.gdn.x.product.service.api.ObjectConverterService;
import com.gdn.x.product.service.api.PriceHistoryService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;

@Slf4j
@Service
public class ItemPickupPointHelperServiceImpl implements ItemPickupPointHelperService {

  @Autowired
  private PriceHistoryService priceHistoryService;

  @Autowired
  private ObjectConverterService objectConverterService;

  @Autowired
  private ChannelService channelService;

  @Value("${cnc.for.warehouse.feature.switch}")
  private boolean cncForWarehouseFeatureSwitch;

  @Override
  public boolean isItemPickupPointPriceChange(ItemPickupPoint itemPickupPoint, Set<Price> prices) {
    return CollectionUtils.isNotEmpty(prices) && prices.stream().anyMatch(
      updatePrice -> itemPickupPoint.getPrice().stream().anyMatch(
        currentPrice -> isPriceChangedForMerchantPromo(itemPickupPoint, updatePrice, currentPrice)
          || isPriceChangedForNonMerchantPromo(itemPickupPoint, updatePrice, currentPrice)));
  }

  @Override
  public boolean isItemPickupPointPriceChangeForOriginalSellingPrice(ItemPickupPoint itemPickupPoint,
      Set<Price> prices) {
    return CollectionUtils.isNotEmpty(prices) && prices.stream().anyMatch(
        updatePrice -> itemPickupPoint.getPrice().stream()
            .anyMatch(currentPrice -> isPriceChangedForSellingPrice(updatePrice, currentPrice)));
  }

  @Override
  public ItemPickupPoint setItemPriceByChannel(ItemPickupPoint itemPickupPoint, Set<Price> prices,
    String username) {
    Date updateDate = new Date();
    for (Price updatePrice : prices) {
      boolean channelFound = false;
      updatePrice.setLastUpdatedBy(username);
      updatePrice.setLastUpdatedDate(updateDate);

      for (Price currentPrice : itemPickupPoint.getPrice()) {
        if (currentPrice.getChannel().equals(updatePrice.getChannel())) {
          channelFound = true;
          if (Double.compare(currentPrice.getListPrice(), updatePrice.getListPrice()) != 0
            || Double.compare(currentPrice.getOfferPrice(), updatePrice.getOfferPrice()) != 0) {
            this.priceHistoryService.savePriceHistory(
              this.objectConverterService.convertToPriceHistory(currentPrice, itemPickupPoint.getItemSku()));
            updatePrice.setListOfDiscountPrices(currentPrice.getListOfDiscountPrices());
            updatePrice.setMerchantPromoDiscountPrice(currentPrice.getMerchantPromoDiscountPrice());
            BeanUtils.copyProperties(updatePrice, currentPrice);
            if (channelService.getDefaultChannel().equals(currentPrice.getChannel())) {
              itemPickupPoint.setPriceUpdatedDate(new Date());
            }
          }
        }
      }
      if (!channelFound) {
        itemPickupPoint.getPrice().add(updatePrice);
      }
    }
    return itemPickupPoint;
  }

  private boolean isPriceChangedForMerchantPromo(ItemPickupPoint itemPickupPoint, Price updatePrice,
    Price currentPrice) {
    if (itemPickupPoint.isMerchantPromoDiscount() && currentPrice.getChannel()
      .equals(updatePrice.getChannel())) {
      if (Objects.isNull(currentPrice.getMerchantPromoDiscountPrice())) {
        return Double.compare(currentPrice.getOfferPrice(), updatePrice.getOfferPrice()) != 0;
      } else {
        return Double.compare(currentPrice.getMerchantPromoDiscountPrice().getDiscountPrice(), updatePrice
          .getOfferPrice()) != 0;
      }
    }
    return false;
  }

  private boolean isPriceChangedForNonMerchantPromo(ItemPickupPoint itemPickupPoint,
    Price updatePrice, Price currentPrice) {
    return !itemPickupPoint.isMerchantPromoDiscount() && currentPrice.getChannel()
      .equals(updatePrice.getChannel()) && (
      currentPrice.getListPrice() != updatePrice.getListPrice()
        || currentPrice.getOfferPrice() != updatePrice.getOfferPrice());
  }

  private boolean isPriceChangedForSellingPrice(Price updatePrice, Price currentPrice) {
    return currentPrice.getChannel().equals(updatePrice.getChannel()) && (
        currentPrice.getListPrice() != updatePrice.getListPrice()
            || currentPrice.getOfferPrice() != updatePrice.getOfferPrice());
  }

  @Override
  public boolean isPriceEditDisabled(ItemPickupPoint itemPickupPoint) {
    Date now = new Date();
    return itemPickupPoint.isMerchantPromoDiscount() || itemPickupPoint.getPrice().stream().anyMatch(
      price -> CollectionUtils.isNotEmpty(price.getListOfDiscountPrices()) && price
        .getListOfDiscountPrices().stream().anyMatch(
          discountPrice -> (StringUtils.isNotBlank(discountPrice.getCampaignCode()) && discountPrice
            .getStartDateTime().before(now) && discountPrice.getEndDateTime().after(now))));
  }

  @Override
  public boolean isItemViewConfigChangeForExistingChannel(ItemPickupPoint pickupPoint,
    Set<ItemViewConfig> itemViewConfigs) {
    for (ItemViewConfig itemViewConfig : itemViewConfigs) {
      ItemViewConfig singleItemViewConfigByChannel =
          pickupPoint.getSingleItemViewConfigByChannel(itemViewConfig.getChannel());
      if (Objects.isNull(singleItemViewConfigByChannel)) {
        return true;
      }
      if (this.isItemViewConfigChange(singleItemViewConfigByChannel,
        itemViewConfig)) {
        return true;
      }
    }
    return false;
  }

  private boolean isItemViewConfigChange(ItemViewConfig currItemViewConfig,
    ItemViewConfig itemViewConfig) {
    if (StringUtils.isBlank(itemViewConfig.getChannel())) {
      itemViewConfig.setChannel(this.channelService.getDefaultChannel());
    }
    return (currItemViewConfig.getChannel().equals(itemViewConfig.getChannel()) && !(
      currItemViewConfig.isBuyable() == itemViewConfig.isBuyable()
        && currItemViewConfig.isDiscoverable() == itemViewConfig.isDiscoverable()))
      || isItemBuyableScheduleChange(currItemViewConfig.getItemBuyableSchedules(),
      itemViewConfig.getItemBuyableSchedules()) || isItemDiscoverableScheduleChange(
      currItemViewConfig.getItemDiscoverableSchedules(),
      itemViewConfig.getItemDiscoverableSchedules());
  }

  private boolean isItemBuyableScheduleChange(ItemBuyableSchedule currentItemBuyableSchedule,
    ItemBuyableSchedule newItemBuyableSchedule) {
    return Objects.nonNull(currentItemBuyableSchedule) && !currentItemBuyableSchedule
      .equals(newItemBuyableSchedule);
  }

  private boolean isItemDiscoverableScheduleChange(
    ItemDiscoverableSchedule currentItemDiscoverableSchedule,
    ItemDiscoverableSchedule newItemDiscoverableSchedule) {
    return Objects.nonNull(currentItemDiscoverableSchedule) && !currentItemDiscoverableSchedule
      .equals(newItemDiscoverableSchedule);
  }

  public AutoCreatePickupPointResponse getAutoCreatePickupPointResponse(
      AutoCreatePickupPointResponse autoCreatePickupPointResponse,
      ItemPickupPoint itemPickupPoint) {
    autoCreatePickupPointResponse.setBuyable(itemPickupPoint.getAllItemViewConfigs().stream()
        .filter(itemViewConfig -> channelService.getDefaultChannel()
            .equalsIgnoreCase(itemViewConfig.getChannel())).findFirst().orElse(new ItemViewConfig())
        .isBuyable());
    autoCreatePickupPointResponse.setDiscoverable(itemPickupPoint.getAllItemViewConfigs().stream()
        .filter(itemViewConfig -> channelService.getDefaultChannel()
            .equalsIgnoreCase(itemViewConfig.getChannel())).findFirst().orElse(new ItemViewConfig())
        .isDiscoverable());
    autoCreatePickupPointResponse.setCncBuyable(itemPickupPoint.getAllItemViewConfigs().stream()
        .filter(itemViewConfig -> channelService.getCncChannel()
            .equalsIgnoreCase(itemViewConfig.getChannel())).findFirst().orElse(new ItemViewConfig())
        .isBuyable());
    autoCreatePickupPointResponse.setCncDiscoverable(
        itemPickupPoint.getAllItemViewConfigs().stream().filter(
                itemViewConfig -> channelService.getCncChannel()
                    .equalsIgnoreCase(itemViewConfig.getChannel())).findFirst()
            .orElse(new ItemViewConfig()).isDiscoverable());
    autoCreatePickupPointResponse.setPrice(
        itemPickupPoint.getPrice().stream().findFirst().orElse(new Price()).getOfferPrice());
    return autoCreatePickupPointResponse;
  }

  public void setItemViewConfig(ItemPickupPoint itemPickupPoint, boolean deliveryFlag,
      ItemPickupPoint resultMaxPriceItemPickupPoint) {
    if (cncForWarehouseFeatureSwitch) {
      Set<ItemViewConfig> itemViewConfigSet = new HashSet<>();
      ItemViewConfig defaultViewConfig;
      ItemViewConfig cncViewConfig;

      if (Boolean.FALSE.equals(deliveryFlag)) {
        defaultViewConfig = getItemViewConfig(channelService.getDefaultChannel(), false, false);
        itemViewConfigSet.add(defaultViewConfig);
        itemPickupPoint.setItemViewConfig(itemViewConfigSet);
      } else {
        defaultViewConfig = getItemViewConfig(channelService.getDefaultChannel(),
            resultMaxPriceItemPickupPoint.getItemViewConfig().stream().findFirst()
                .orElse(new ItemViewConfig()).isBuyable(),
            resultMaxPriceItemPickupPoint.getItemViewConfig().stream().findFirst()
                .orElse(new ItemViewConfig()).isDiscoverable());
        cncViewConfig = getItemViewConfig(channelService.getCncChannel(), false, false);
        itemViewConfigSet.add(defaultViewConfig);
        itemViewConfigSet.add(cncViewConfig);
        itemPickupPoint.setItemViewConfig(itemViewConfigSet);
      }
    } else {
      itemPickupPoint.setItemViewConfig(Collections.singleton(
          getItemViewConfig(channelService.getDefaultChannel(),
              resultMaxPriceItemPickupPoint.getItemViewConfig().stream().findFirst()
                  .orElse(new ItemViewConfig()).isBuyable(),
              resultMaxPriceItemPickupPoint.getItemViewConfig().stream().findFirst()
                  .orElse(new ItemViewConfig()).isDiscoverable())));
    }
  }

  private static ItemViewConfig getItemViewConfig(String channel, boolean buyable,
      boolean discoverable) {
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setBuyable(buyable);
    itemViewConfig.setDiscoverable(discoverable);
    itemViewConfig.setChannel(channel);
    return itemViewConfig;
  }


}
