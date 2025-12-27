package com.gdn.partners.pbp.service.eventstore;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import com.gdn.mta.domain.event.modal.Level2InventoryNonOosEvent;
import com.gdn.partners.pbp.model.vo.EventStoreQueue;
import com.gdn.partners.pbp.repository.eventstore.EventStoreRepository;
import com.gdn.partners.pbp.repository.eventstore.Level2InventoryEventStoreRepository;
import com.gdn.partners.pbp.repository.eventstore.MerchantProductMVIndexingEventStoreRepository;
import com.gdn.partners.pbp.service.mv.updater.BaseMerchantProductMVInventoryUpdaterService;

@Component
public class Level2InventoryNonOosEventStoreQueueMessageProcessorBean implements
    EventStoreQueueMessageProcessor<Level2InventoryNonOosEvent> {

  @Autowired
  @Qualifier("merchantProductMVLevel2InventoryNonOosEventUpdaterService")
  private BaseMerchantProductMVInventoryUpdaterService<Level2InventoryNonOosEvent>
      merchantProductMVLevel2InventoryNonOosEventUpdaterService;

  @Autowired
  private Level2InventoryEventStoreRepository level2InventoryEventStoreRepository;

  @Autowired
  private EventStoreRepository eventStoreRepository;

  @Autowired
  private MerchantProductMVIndexingEventStoreRepository indexingEventStoreRepository;

  @Override
  public void doProcessMessage(EventStoreQueue<Level2InventoryNonOosEvent> message) {
    Level2InventoryNonOosEvent inventoryEventModel = message.getEventModel();
    merchantProductMVLevel2InventoryNonOosEventUpdaterService.update(inventoryEventModel);

  }

  @Override
  public void doIndexingProcess(EventStoreQueue<Level2InventoryNonOosEvent> message) {
    eventStoreRepository.deleteById(message.getEventId());
  }
}
