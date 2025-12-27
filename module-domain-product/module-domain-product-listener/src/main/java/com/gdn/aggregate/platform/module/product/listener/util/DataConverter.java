package com.gdn.aggregate.platform.module.product.listener.util;

import java.util.Objects;
import java.util.Optional;

import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Level2InventoryQuantityChangedEvent;
import com.gdn.aggregate.platform.module.product.listener.model.raw.StockUpdateSearchEvent;
import org.apache.commons.lang3.StringUtils;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.aggregate.modules.agp.engagement.common.util.service.TimeService;
import com.gdn.aggregate.modules.agp.engagement.common.util.util.DataUtil;
import com.gdn.aggregate.platform.module.product.listener.model.raw.InventoryInfoChange;
import com.gdn.aggregate.platform.module.product.listener.model.raw.Item;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPoint;
import com.gdn.aggregate.platform.module.product.listener.model.raw.PickupPointUpsertCombinedEventModel;
import com.gdn.aggregate.platform.module.product.listener.model.raw.RawItemCombinedUpsertEventModel;
import lombok.extern.slf4j.Slf4j;

@Component
@Slf4j
public class DataConverter extends DataUtil {

  private static final String HYPHEN = "-";
  public DataConverter(ObjectMapper objectMapper, TimeService timeService) {
    super(objectMapper, timeService);
  }

  public InventoryInfoChange toInventoryInfoChangeFromCombinedUpsertEventModel(ConsumerRecord<String, String> record,
      boolean newInventoryInfoEventEnabled) {
    return Optional.ofNullable(record.value()).map(value -> {
      try {
        return newInventoryInfoEventEnabled ?
            getObjectMapper().readValue(value, PickupPointUpsertCombinedEventModel.class).getInventoryInfoChange() :
            toData(record, InventoryInfoChange.class);
      } catch (Exception e) {
        log.error("Error deserializing InventoryInfoChange record value: {}", value, e);
        return null;
      }
    }).orElse(null);
  }

  public PickupPoint toPickupPointFromCombinedUpsertEventModel(ConsumerRecord<String, String> record,
      boolean newItemPickupPointUpsertEventEnabled) {
    PickupPoint pickupPoint = Optional.ofNullable(record.value()).map(value -> {
      try {
        return newItemPickupPointUpsertEventEnabled ?
            getObjectMapper().readValue(value, PickupPointUpsertCombinedEventModel.class).getPickupPoint() :
            getObjectMapper().readValue(value, PickupPoint.class);
      } catch (Exception e) {
        log.error("Error deserializing record value: {}", value, e);
        return null;
      }
    }).orElse(null);
    return pickupPoint;
  }

  public PickupPointUpsertCombinedEventModel getPickupPointUpsertCombinedEventModel(
      ConsumerRecord<String, String> record, boolean migration, boolean fullReconstruct,
      PickupPoint pickupPoint, InventoryInfoChange inventoryInfoChange,
      StockUpdateSearchEvent stockUpdateSearchEvent,
    Level2InventoryQuantityChangedEvent level2InventoryQuantityChangedEvent, String traceId) {
    PickupPointUpsertCombinedEventModel.PickupPointUpsertCombinedEventModelBuilder<?, ?> builder = initializeBuilder(
        record.topic(), migration, fullReconstruct, traceId);
    String productSku = fetchProductSku(pickupPoint, inventoryInfoChange, stockUpdateSearchEvent,
      level2InventoryQuantityChangedEvent);
    
    Optional.ofNullable(pickupPoint)
        .ifPresent(pp -> applyPickupPointFields(builder, pp));
    Optional.ofNullable(inventoryInfoChange)
        .ifPresent(inv -> applyInventoryInfoChangeFields(builder, inv));
    Optional.ofNullable(stockUpdateSearchEvent)
        .ifPresent(stockUpdateEvent -> constructPickupPointUpsertForStockUpdateEvent(stockUpdateEvent, builder));
    Optional.ofNullable(level2InventoryQuantityChangedEvent).ifPresent(
      inventoryRepublishEvent -> constructPickupPointUpsertForInventoryRepublish(
        inventoryRepublishEvent, builder));
    return builder.id(productSku).pickupPoint(pickupPoint).inventoryInfoChange(inventoryInfoChange)
      .stockUpdateSearchEvent(stockUpdateSearchEvent)
      .level2InventoryQuantityChangedEvent(level2InventoryQuantityChangedEvent).build();
  }

  private PickupPointUpsertCombinedEventModel.PickupPointUpsertCombinedEventModelBuilder<?, ?> initializeBuilder(
    String eventTrigger, boolean migration, boolean fullReconstruct, String traceId) {
    return PickupPointUpsertCombinedEventModel.builder().eventTrigger(eventTrigger)
      .migration(migration).fullReconstruct(fullReconstruct).traceId(traceId);
  }

  private String fetchProductSku(PickupPoint pickupPoint, InventoryInfoChange inventoryInfoChange,
    StockUpdateSearchEvent stockUpdateSearchEvent,
    Level2InventoryQuantityChangedEvent level2InventoryQuantityChangedEvent) {
    if (Objects.nonNull(pickupPoint)) {
      return pickupPoint.getProductSku();
    }
    if (Objects.nonNull(inventoryInfoChange) && StringUtils.isNotBlank(
      inventoryInfoChange.getItemSku())) {
      return extractProductSkuFromItemSku(inventoryInfoChange.getItemSku());
    }

    if (Objects.nonNull(stockUpdateSearchEvent) && StringUtils.isNotBlank(
      stockUpdateSearchEvent.getWebItemSku())) {
      return extractProductSkuFromItemSku(stockUpdateSearchEvent.getWebItemSku());
    }

    if (Objects.nonNull(level2InventoryQuantityChangedEvent) && StringUtils.isNotBlank(
      level2InventoryQuantityChangedEvent.getLevel2Id())) {
      return extractProductSkuFromItemSku(level2InventoryQuantityChangedEvent.getLevel2Id());
    }

    return StringUtils.EMPTY;
  }

  private static void applyPickupPointFields(
    PickupPointUpsertCombinedEventModel.PickupPointUpsertCombinedEventModelBuilder<?, ?> builder,
    PickupPoint pickupPoint) {
    builder.timestamp(pickupPoint.getTimestamp())
      .createdTimestamp(pickupPoint.getCreatedTimestamp())
      .updatedTimestamp(pickupPoint.getUpdatedTimestamp()).createdBy(pickupPoint.getCreatedBy())
      .updatedBy(pickupPoint.getUpdatedBy()).markForDelete(pickupPoint.isMarkForDelete())
      .expiryTime(pickupPoint.getExpiryTime());
  }

  private static void applyInventoryInfoChangeFields(
    PickupPointUpsertCombinedEventModel.PickupPointUpsertCombinedEventModelBuilder<?, ?> builder,
    InventoryInfoChange inventoryInfoChange) {
    builder.inventoryInfoChange(inventoryInfoChange).timestamp(inventoryInfoChange.getTimestamp())
      .createdTimestamp(inventoryInfoChange.getCreatedTimestamp())
      .updatedTimestamp(inventoryInfoChange.getUpdatedTimestamp())
      .createdBy(inventoryInfoChange.getCreatedBy()).updatedBy(inventoryInfoChange.getUpdatedBy())
      .markForDelete(inventoryInfoChange.isMarkForDelete())
      .expiryTime(inventoryInfoChange.getExpiryTime());
  }

  private static void constructPickupPointUpsertForStockUpdateEvent(
    StockUpdateSearchEvent stockUpdateSearchEvent,
    PickupPointUpsertCombinedEventModel.PickupPointUpsertCombinedEventModelBuilder<?, ?> builder) {

    long timeStamp =
      Optional.of(stockUpdateSearchEvent.getTimestamp()).orElse(System.currentTimeMillis());

    builder.stockUpdateSearchEvent(stockUpdateSearchEvent).timestamp(timeStamp)
      .updatedTimestamp(timeStamp).createdTimestamp(timeStamp).createdBy(
        Optional.ofNullable(stockUpdateSearchEvent.getCreatedBy())
          .orElse(Topics.INVENTORY_STOCK_UPDATE_EVENT)).markForDelete(false)
      .eventTrigger(Topics.INVENTORY_STOCK_UPDATE_EVENT)
      .expiryTime(stockUpdateSearchEvent.getExpiryTime());
  }

  private void constructPickupPointUpsertForInventoryRepublish(
    Level2InventoryQuantityChangedEvent inventoryRepublishEvent,
    PickupPointUpsertCombinedEventModel.PickupPointUpsertCombinedEventModelBuilder<?, ?> builder) {
    long timeStamp =
      Optional.of(inventoryRepublishEvent.getTimestamp()).orElse(System.currentTimeMillis());

    builder.level2InventoryQuantityChangedEvent(inventoryRepublishEvent).timestamp(timeStamp)
      .updatedTimestamp(timeStamp).createdTimestamp(timeStamp).createdBy(
        Optional.ofNullable(inventoryRepublishEvent.getCreatedBy())
          .orElse(Topics.INVENTORY_ONLINE_REPUBLISH_EVENT)).markForDelete(false)
      .eventTrigger(Topics.INVENTORY_ONLINE_REPUBLISH_EVENT)
      .expiryTime(inventoryRepublishEvent.getExpiryTime());

  }


  public String extractProductSkuFromItemSku(String itemSku) {
    return Optional.ofNullable(itemSku).filter(StringUtils::isNotBlank).map(s -> s.lastIndexOf(HYPHEN))
        .filter(index -> index != -1).map(index -> itemSku.substring(0, index)).orElse(null);
  }

  public  Item getItemFromItemUpdateEventModel(ConsumerRecord<String, String> record,
      boolean newItemUpdateQueueEnabled) {
    return Optional.ofNullable(record.value()).map(value -> {
      try {
        return newItemUpdateQueueEnabled ?
            getObjectMapper().readValue(value, RawItemCombinedUpsertEventModel.class).getItem() :
            getObjectMapper().readValue(value, Item.class);
      } catch (Exception e) {
        log.error("Error deserializing record value: {}", value, e);
        return null;
      }
    }).orElse(null);
  }

  public boolean getDirectSaveFlagFromRawItemUpdateEventModel(ConsumerRecord<String, String> record,
      boolean newItemUpdateQueueEnabled) {
    return Optional.ofNullable(record.value()).map(value -> {
      try {
        return newItemUpdateQueueEnabled && getObjectMapper().readValue(value, RawItemCombinedUpsertEventModel.class)
            .isDirectSave();
      } catch (Exception e) {
        log.error("Error deserializing record value: {}", value, e);
        return false;
      }
    }).orElse(false);
  }
}
