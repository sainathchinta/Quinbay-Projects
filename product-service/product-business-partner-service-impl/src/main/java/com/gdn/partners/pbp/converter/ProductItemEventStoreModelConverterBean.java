package com.gdn.partners.pbp.converter;

import java.util.Date;
import java.util.HashSet;
import java.util.Set;

import org.springframework.stereotype.Component;

import com.gdn.partners.pbp.entity.eventstore.ProductItemEventStore;
import com.gdn.partners.pbp.util.ProductLevel3Util;
import com.gdn.x.product.domain.event.model.ItemChange;
import com.gdn.x.product.domain.event.model.ItemViewConfig;
import com.gdn.x.product.domain.event.model.MasterDataItem;
import com.gdn.x.product.domain.event.model.Price;
import com.gdn.x.product.enums.ChannelName;

@Component
public class ProductItemEventStoreModelConverterBean implements ProductItemEventStoreModelConverter {

  @Override
  public ProductItemEventStore convertToProductItemEventStore(ItemChange eventModel,
      String eventName) {
    ProductItemEventStore eventStore = new ProductItemEventStore();
    eventStore.setStoreId(eventModel.getStoreId());
    eventStore.setEventName(eventName);
    eventStore.setEventTimestamp(new Date(eventModel.getTimestamp()));
    eventStore.setItemSku(eventModel.getItemSku());
    eventStore.setProductSku(eventModel.getProductSku());
    eventStore.setMerchantSku(eventModel.getMerchantSku());
    eventStore.setPickupPointCode(eventModel.getPickupPointCode());
    eventStore.setMerchantCode(eventModel.getMerchantCode());
    eventStore.setSynchronized(eventModel.isSynchronized());
    eventStore.setMarkForDelete(eventModel.isMarkForDelete());
    eventStore.setArchived(eventModel.isArchived());
    eventStore.setO2oChannelActive(eventModel.isOff2OnChannelActive());

    if (eventModel.getMasterDataItem() != null) {
      eventStore.setItemName(eventModel.getMasterDataItem().getGeneratedItemName());
      eventStore.setSkuCode(eventModel.getMasterDataItem().getSkuCode());
    }

    Price itemPrice = ProductLevel3Util.getItemPrice(eventModel.getPrice(), ChannelName.DEFAULT);
    if (itemPrice != null) {
      eventStore.setListPrice(itemPrice.getListPrice());
      eventStore.setOfferPrice(itemPrice.getOfferPrice());
    }

    ItemViewConfig itemViewConfig =
        ProductLevel3Util.getItemViewConfig(eventModel.getItemViewConfigs(), ChannelName.DEFAULT);
    if (itemViewConfig != null) {
      eventStore.setBuyable(itemViewConfig.isBuyable());
      eventStore.setDiscoverable(itemViewConfig.isDiscoverable());
    }
    return eventStore;
  }

  @Override
  public ItemChange convertToItemChange(ProductItemEventStore eventModel) {
    ItemChange kafkaEventModel = new ItemChange();
    kafkaEventModel.setStoreId(eventModel.getStoreId());
    kafkaEventModel.setTimestamp(eventModel.getEventTimestamp().getTime());
    kafkaEventModel.setItemSku(eventModel.getItemSku());
    kafkaEventModel.setProductSku(eventModel.getProductSku());
    kafkaEventModel.setMerchantSku(eventModel.getMerchantSku());
    kafkaEventModel.setPickupPointCode(eventModel.getPickupPointCode());
    kafkaEventModel.setMerchantCode(eventModel.getMerchantCode());
    kafkaEventModel.setSynchronized(eventModel.isSynchronized());
    kafkaEventModel.setMarkForDelete(eventModel.isMarkForDelete());
    kafkaEventModel.setArchived(eventModel.isArchived());
    kafkaEventModel.setOff2OnChannelActive(eventModel.isO2oChannelActive());

    MasterDataItem masterDataItem = new MasterDataItem();
    masterDataItem.setGeneratedItemName(eventModel.getItemName());
    masterDataItem.setSkuCode(eventModel.getSkuCode());

    kafkaEventModel.setMasterDataItem(masterDataItem);

    Set<Price> itemPrices = new HashSet<>();
    Price itemPrice = new Price();
    itemPrice.setChannel(ChannelName.DEFAULT.name());
    itemPrice.setListPrice(eventModel.getListPrice());
    itemPrice.setOfferPrice(eventModel.getOfferPrice());
    itemPrices.add(itemPrice);
    kafkaEventModel.setPrice(itemPrices);

    Set<ItemViewConfig> itemViewConfigs = new HashSet<>();
    ItemViewConfig itemViewConfig = new ItemViewConfig();
    itemViewConfig.setChannel(ChannelName.DEFAULT.name());
    itemViewConfig.setBuyable(eventModel.isBuyable());
    itemViewConfig.setDiscoverable(eventModel.isDiscoverable());
    itemViewConfigs.add(itemViewConfig);
    kafkaEventModel.setItemViewConfigs(itemViewConfigs);

    return kafkaEventModel;
  }

}
