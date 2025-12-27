package com.gdn.partners.pbp.service.eventstore;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import com.gdn.partners.pbp.model.vo.EventStoreQueue;
import com.gdn.partners.pbp.repository.eventstore.EventStoreRepository;
import com.gdn.partners.pbp.repository.eventstore.MerchantProductMVIndexingEventStoreRepository;
import com.gdn.partners.pbp.repository.eventstore.ProductItemEventStoreRepository;
import com.gdn.partners.pbp.service.mv.updater.BaseMerchantProductMVProductItemUpdaterService;
import com.gdn.x.product.domain.event.model.ItemChange;

@Component
public class ItemChangeEventStoreQueueMessageProcessorBean implements
    EventStoreQueueMessageProcessor<ItemChange> {

  @Autowired
  @Qualifier("merchantProductMVProductItemUpdaterService")
  private BaseMerchantProductMVProductItemUpdaterService<ItemChange> merchantProductMVProductItemUpdaterService;
  @Autowired
  private ProductItemEventStoreRepository productItemEventStoreRepository;
  @Autowired
  private MerchantProductMVIndexingEventStoreRepository indexingEventStoreRepository;
  @Autowired
  private EventStoreRepository eventStoreRepository;

  @Override
  public void doProcessMessage(EventStoreQueue<ItemChange> message) {
    ItemChange eventModel = message.getEventModel();
    merchantProductMVProductItemUpdaterService.update(eventModel);
  }

  @Override
  public void doIndexingProcess(EventStoreQueue<ItemChange> message) {
    eventStoreRepository.deleteById(message.getEventId());
  }
}
