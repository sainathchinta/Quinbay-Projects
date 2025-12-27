package com.gdn.partners.pbp.converter;

import java.util.Date;

import org.springframework.stereotype.Component;

import com.gdn.mta.domain.event.modal.Level2InventoryMinimumStockAlertEvent;
import com.gdn.mta.domain.event.modal.Level2InventoryOosEvent;
import com.gdn.mta.domain.event.modal.Level2InventoryNonOosEvent;
import com.gdn.partners.pbp.entity.eventstore.Level2InventoryEventStore;
import com.gdn.partners.pbp.model.InventoryEventStatus;

@Component
public class Level2InventoryEventStoreModelConverterBean implements
    Level2InventoryEventStoreModelConverter {

  @Override
  public Level2InventoryEventStore convertToLevel2InventoryEventStore(
      Level2InventoryNonOosEvent eventModel, String eventName) {
    Level2InventoryEventStore level2InventoryEventStore = new Level2InventoryEventStore();
    level2InventoryEventStore.setEventName(eventName);
    level2InventoryEventStore.setEventTimestamp(new Date(eventModel.getTimestamp()));
    level2InventoryEventStore.setStoreId(eventModel.getStoreId());
    level2InventoryEventStore.setLevel2Id(eventModel.getLevel2Id());
    level2InventoryEventStore.setMerchantCode(eventModel.getLevel2MerchantCode());
    level2InventoryEventStore.setInventoryStatus(InventoryEventStatus.NON_OOS);
    return level2InventoryEventStore;
  }

  @Override
  public Level2InventoryEventStore convertToLevel2InventoryEventStore(
      Level2InventoryOosEvent eventModel, String eventName) {
    Level2InventoryEventStore level2InventoryEventStore = new Level2InventoryEventStore();
    level2InventoryEventStore.setEventName(eventName);
    level2InventoryEventStore.setEventTimestamp(new Date(eventModel.getTimestamp()));
    level2InventoryEventStore.setStoreId(eventModel.getStoreId());
    level2InventoryEventStore.setLevel2Id(eventModel.getLevel2Id());
    level2InventoryEventStore.setMerchantCode(eventModel.getLevel2MerchantCode());
    level2InventoryEventStore.setInventoryStatus(InventoryEventStatus.OOS);
    return level2InventoryEventStore;
  }

  @Override
  public Level2InventoryEventStore convertToLevel2InventoryEventStore(
      Level2InventoryMinimumStockAlertEvent eventModel, String eventName,
      boolean isBelowMinimumStock) {
    Level2InventoryEventStore level2InventoryEventStore = new Level2InventoryEventStore();
    level2InventoryEventStore.setEventName(eventName);
    level2InventoryEventStore.setEventTimestamp(new Date(eventModel.getTimestamp()));
    level2InventoryEventStore.setStoreId(eventModel.getStoreId());
    level2InventoryEventStore.setLevel2Id(eventModel.getGdnSku());
    level2InventoryEventStore.setMerchantCode(eventModel.getBusinessPartnerCode());
    level2InventoryEventStore.setInventoryStatus(isBelowMinimumStock ? InventoryEventStatus.BELOW_MINIMUM_STOCK
        : InventoryEventStatus.NON_BELOW_MINIMUM_STOCK);
    return level2InventoryEventStore;
  }

  @Override
  public Level2InventoryOosEvent convertToLevel2InventoryOosEvent(
      Level2InventoryEventStore eventModel) {
    Level2InventoryOosEvent kafkaEventModel = new Level2InventoryOosEvent();
    kafkaEventModel.setStoreId(eventModel.getStoreId());
    kafkaEventModel.setTimestamp(eventModel.getEventTimestamp().getTime());
    kafkaEventModel.setLevel2Id(eventModel.getLevel2Id());
    kafkaEventModel.setLevel2MerchantCode(eventModel.getMerchantCode());
    return kafkaEventModel;
  }

  @Override
  public Level2InventoryNonOosEvent convertToLevel2InventoryNonOosEvent(
      Level2InventoryEventStore eventModel) {
    Level2InventoryNonOosEvent kafkaEventModel = new Level2InventoryNonOosEvent();
    kafkaEventModel.setStoreId(eventModel.getStoreId());
    kafkaEventModel.setTimestamp(eventModel.getEventTimestamp().getTime());
    kafkaEventModel.setLevel2Id(eventModel.getLevel2Id());
    kafkaEventModel.setLevel2MerchantCode(eventModel.getMerchantCode());
    return kafkaEventModel;
  }

  @Override
  public Level2InventoryMinimumStockAlertEvent convertToLevel2InventoryMinimumStockAlertEvent(
      Level2InventoryEventStore eventModel) {
    Level2InventoryMinimumStockAlertEvent kafkaEventModel =
        new Level2InventoryMinimumStockAlertEvent();
    kafkaEventModel.setStoreId(eventModel.getStoreId());
    kafkaEventModel.setTimestamp(eventModel.getEventTimestamp().getTime());
    kafkaEventModel.setBusinessPartnerCode(eventModel.getMerchantCode());
    return kafkaEventModel;
  }
}
