package com.gdn.partners.pbp.converter;

import com.gdn.mta.domain.event.modal.Level2InventoryMinimumStockAlertEvent;
import com.gdn.mta.domain.event.modal.Level2InventoryNonOosEvent;
import com.gdn.mta.domain.event.modal.Level2InventoryOosEvent;
import com.gdn.partners.pbp.entity.eventstore.Level2InventoryEventStore;


public interface Level2InventoryEventStoreModelConverter {
  Level2InventoryEventStore convertToLevel2InventoryEventStore(
      Level2InventoryNonOosEvent eventModel, String eventName);

  Level2InventoryEventStore convertToLevel2InventoryEventStore(Level2InventoryOosEvent eventModel,
      String eventName);

  Level2InventoryEventStore convertToLevel2InventoryEventStore(
      Level2InventoryMinimumStockAlertEvent eventModel, String eventName,
      boolean isBelowMinimumStock);
  
  Level2InventoryOosEvent convertToLevel2InventoryOosEvent(Level2InventoryEventStore eventModel);

  Level2InventoryNonOosEvent convertToLevel2InventoryNonOosEvent(
      Level2InventoryEventStore eventModel);

  Level2InventoryMinimumStockAlertEvent convertToLevel2InventoryMinimumStockAlertEvent(
      Level2InventoryEventStore eventModel);

}
