package com.gdn.partners.pbp.service.eventstore;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import com.gdn.mta.domain.event.modal.Level2InventoryOosEvent;
import com.gdn.partners.pbp.model.vo.EventStoreQueue;
import com.gdn.partners.pbp.repository.eventstore.EventStoreRepository;
import com.gdn.partners.pbp.repository.eventstore.Level2InventoryEventStoreRepository;
import com.gdn.partners.pbp.repository.eventstore.MerchantProductMVIndexingEventStoreRepository;
import com.gdn.partners.pbp.service.mv.updater.BaseMerchantProductMVInventoryUpdaterService;

@Component
public class Level2InventoryOosEventStoreQueueMessageProcessorBean implements
    EventStoreQueueMessageProcessor<Level2InventoryOosEvent> {

  @Autowired
  @Qualifier("merchantProductMVLevel2InventoryOosEventUpdaterService")
  private BaseMerchantProductMVInventoryUpdaterService<Level2InventoryOosEvent> merchantProductMVLevel2InventoryOosEventUpdaterService;

  @Autowired
  private Level2InventoryEventStoreRepository level2InventoryEventStoreRepository;

  @Autowired
  private MerchantProductMVIndexingEventStoreRepository indexingEventStoreRepository;

  @Autowired
  private EventStoreRepository eventStoreRepository;

  @Override
  public void doProcessMessage(EventStoreQueue<Level2InventoryOosEvent> message) {
    Level2InventoryOosEvent inventoryEventModel = message.getEventModel();
    merchantProductMVLevel2InventoryOosEventUpdaterService.update(inventoryEventModel);
  }

  @Override
  public void doIndexingProcess(EventStoreQueue<Level2InventoryOosEvent> message) {
    eventStoreRepository.deleteById(message.getEventId());
  }

}
