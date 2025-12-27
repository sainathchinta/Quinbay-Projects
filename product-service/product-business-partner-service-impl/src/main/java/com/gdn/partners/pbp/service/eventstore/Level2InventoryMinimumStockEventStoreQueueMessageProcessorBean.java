package com.gdn.partners.pbp.service.eventstore;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import com.gdn.mta.domain.event.modal.Level2InventoryMinimumStockAlertEvent;
import com.gdn.partners.pbp.model.vo.EventStoreQueue;
import com.gdn.partners.pbp.repository.eventstore.EventStoreRepository;
import com.gdn.partners.pbp.repository.eventstore.Level2InventoryEventStoreRepository;
import com.gdn.partners.pbp.repository.eventstore.MerchantProductMVIndexingEventStoreRepository;
import com.gdn.partners.pbp.service.mv.updater.BaseMerchantProductMVInventoryUpdaterService;
import com.gdn.x.inventory.domain.event.config.DomainEventName;

@Component
public class Level2InventoryMinimumStockEventStoreQueueMessageProcessorBean implements
    EventStoreQueueMessageProcessor<Level2InventoryMinimumStockAlertEvent> {

  @Autowired
  @Qualifier("merchantProductMVLevel2InventoryNonMinimumStockUpdaterService")
  private BaseMerchantProductMVInventoryUpdaterService<Level2InventoryMinimumStockAlertEvent>
      merchantProductMVLevel2InventoryNonMinimumStockUpdaterService;

  @Autowired
  @Qualifier("merchantProductMVLevel2InventoryMinimumStockUpdaterService")
  private BaseMerchantProductMVInventoryUpdaterService<Level2InventoryMinimumStockAlertEvent> merchantProductMVLevel2InventoryMinimumStockUpdaterService;

  @Autowired
  private Level2InventoryEventStoreRepository level2InventoryEventStoreRepository;

  @Autowired
  private MerchantProductMVIndexingEventStoreRepository indexingEventStoreRepository;

  @Autowired
  private EventStoreRepository eventStoreRepository;

  @Override
  public void doProcessMessage(EventStoreQueue<Level2InventoryMinimumStockAlertEvent> message) {
    String eventName = message.getEventName();
    Level2InventoryMinimumStockAlertEvent eventModel = message.getEventModel();
    if (DomainEventName.STOCK_MINIMUM_STOCK_ALERT_NAME.equals(eventName)) {
      merchantProductMVLevel2InventoryMinimumStockUpdaterService.update(eventModel);
    } else if (DomainEventName.STOCK_NON_MINIMUM_STOCK_ALERT_NAME.equals(eventName)) {
      merchantProductMVLevel2InventoryNonMinimumStockUpdaterService.update(eventModel);
    }
  }


  @Override
  public void doIndexingProcess(EventStoreQueue<Level2InventoryMinimumStockAlertEvent> message) {
    eventStoreRepository.deleteById(message.getEventId());
  }
}
