package com.gdn.partners.pbp.service.eventstore;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pbp.converter.ProductItemEventStoreModelConverter;
import com.gdn.partners.pbp.entity.eventstore.EventStore;
import com.gdn.partners.pbp.entity.eventstore.MerchantProductMVIndexingEventStore;
import com.gdn.partners.pbp.entity.eventstore.ProductItemEventStore;
import com.gdn.partners.pbp.repository.eventstore.MerchantProductMVIndexingEventStoreRepository;
import com.gdn.partners.pbp.repository.eventstore.ProductItemEventStoreRepository;
import com.gdn.x.product.domain.event.model.ItemChange;

@Component
public class ItemChangeEventStoreKafkaMessageProcessorBean extends
    BaseEventStoreKafkaMessageProcessor<ItemChange> {

  @Autowired
  private ProductItemEventStoreRepository productItemEventStoreRepository;

  @Autowired
  private ProductItemEventStoreModelConverter modelConverter;

  @Autowired
  private MerchantProductMVIndexingEventStoreRepository indexingEventStoreRepository;

  @Override
  protected EventStore saveToEventStore(ItemChange eventModel, String eventName) {
    ProductItemEventStore eventStore =
        modelConverter.convertToProductItemEventStore(eventModel, eventName);
    return productItemEventStoreRepository.saveAndFlush(eventStore);
  }

  @Override
  protected void handleIndexingRunning(ItemChange eventModel, EventStore eventStore) {
    MerchantProductMVIndexingEventStore indexingEventStore =
        new MerchantProductMVIndexingEventStore();
    indexingEventStore.setEvent(eventStore);
    indexingEventStoreRepository.save(indexingEventStore);
  }

  @Override
  public boolean allowedToRun() {
    return Boolean.valueOf(getSystemParameterService().getParameter(
        "sysparam.consumer.kafka.item-change-event-store-processor-running"));
  }

}
