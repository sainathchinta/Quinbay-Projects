package com.gdn.partners.pbp.service.eventstore;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import com.gdn.mta.domain.event.modal.Level2InventoryNonOosEvent;
import com.gdn.partners.pbp.converter.Level2InventoryEventStoreModelConverter;
import com.gdn.partners.pbp.entity.eventstore.EventStore;
import com.gdn.partners.pbp.entity.eventstore.Level2InventoryEventStore;
import com.gdn.partners.pbp.entity.eventstore.MerchantProductMVIndexingEventStore;
import com.gdn.partners.pbp.repository.eventstore.Level2InventoryEventStoreRepository;
import com.gdn.partners.pbp.repository.eventstore.MerchantProductMVIndexingEventStoreRepository;
import com.gdn.partners.pbp.service.sysparam.SystemParameterService;

@Component
public class Level2InventoryNonOosEventStoreKafkaMessageProcessorBean extends
    BaseEventStoreKafkaMessageProcessor<Level2InventoryNonOosEvent> {

  @Autowired
  private Level2InventoryEventStoreRepository level2InventoryEventStoreRepository;

  @Autowired
  private Level2InventoryEventStoreModelConverter modelConverter;

  @Autowired
  private MerchantProductMVIndexingEventStoreRepository indexingEventStoreRepository;

  @Autowired
  @Qualifier("globalSystemParameterService")
  private SystemParameterService systemParameterService;

  @Override
  protected EventStore saveToEventStore(Level2InventoryNonOosEvent eventModel, String eventName)
      throws Exception {
    Level2InventoryEventStore eventStore =
        modelConverter.convertToLevel2InventoryEventStore(eventModel, eventName);
    return level2InventoryEventStoreRepository.saveAndFlush(eventStore);
  }

  @Override
  protected void handleIndexingRunning(Level2InventoryNonOosEvent eventModel,
      EventStore savedEventStore) {
    MerchantProductMVIndexingEventStore indexingEventStore =
        new MerchantProductMVIndexingEventStore();
    indexingEventStore.setEvent(savedEventStore);
    indexingEventStoreRepository.save(indexingEventStore);
  }

  @Override
  public boolean allowedToRun() {
    return Boolean.valueOf(getSystemParameterService().getParameter(
        "sysparam.consumer.kafka.inventory-non-oos-event-store-processor-running"));
  }
}
