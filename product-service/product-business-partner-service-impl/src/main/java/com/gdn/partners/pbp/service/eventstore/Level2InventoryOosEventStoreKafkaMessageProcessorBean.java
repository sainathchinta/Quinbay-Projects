package com.gdn.partners.pbp.service.eventstore;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.mta.domain.event.modal.Level2InventoryOosEvent;
import com.gdn.partners.pbp.converter.Level2InventoryEventStoreModelConverter;
import com.gdn.partners.pbp.entity.eventstore.EventStore;
import com.gdn.partners.pbp.entity.eventstore.Level2InventoryEventStore;
import com.gdn.partners.pbp.entity.eventstore.MerchantProductMVIndexingEventStore;
import com.gdn.partners.pbp.repository.eventstore.Level2InventoryEventStoreRepository;
import com.gdn.partners.pbp.repository.eventstore.MerchantProductMVIndexingEventStoreRepository;

@Component
public class Level2InventoryOosEventStoreKafkaMessageProcessorBean extends
    BaseEventStoreKafkaMessageProcessor<Level2InventoryOosEvent> {

  @Autowired
  private Level2InventoryEventStoreRepository level2InventoryEventStoreRepository;

  @Autowired
  private Level2InventoryEventStoreModelConverter modelConverter;

  @Autowired
  private MerchantProductMVIndexingEventStoreRepository indexingEventStoreRepository;

  @Override
  protected EventStore saveToEventStore(Level2InventoryOosEvent eventModel, String eventName) {
    Level2InventoryEventStore eventStore =
        modelConverter.convertToLevel2InventoryEventStore(eventModel, eventName);
    return level2InventoryEventStoreRepository.saveAndFlush(eventStore);
  }

  @Override
  protected void handleIndexingRunning(Level2InventoryOosEvent eventModel,
      EventStore savedEventStore) {
    MerchantProductMVIndexingEventStore indexingEventStore =
        new MerchantProductMVIndexingEventStore();
    indexingEventStore.setEvent(savedEventStore);
    indexingEventStoreRepository.save(indexingEventStore);
  }

  @Override
  public boolean allowedToRun() {
    return Boolean.valueOf(getSystemParameterService().getParameter(
        "sysparam.consumer.kafka.inventory-oos-event-store-processor-running"));
  }
}
