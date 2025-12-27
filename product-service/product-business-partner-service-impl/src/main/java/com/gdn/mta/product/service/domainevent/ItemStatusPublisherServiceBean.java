package com.gdn.mta.product.service.domainevent;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.mta.product.service.config.KafkaPublisher;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.ItemStatusDomainEvent;
import com.gdn.mta.product.service.ItemStatusPublisherService;

@Service
public class ItemStatusPublisherServiceBean implements ItemStatusPublisherService {

  private static final Logger LOGGER =
      LoggerFactory.getLogger(ItemStatusPublisherServiceBean.class);

  @Autowired
  private KafkaPublisher kafkaProducer;

  @Override
  public ItemStatusDomainEvent publishItemStatusDomainEvent(
      ItemStatusDomainEvent itemStatusDomainEvent) {
    LOGGER.info("Publishing item status event for itemSkus: {}, status :{}", itemStatusDomainEvent.getItemSkus(),
        itemStatusDomainEvent.getProductStatus());
    kafkaProducer.send(DomainEventName.ITEM_STATUS_EVENT, itemStatusDomainEvent);
    return itemStatusDomainEvent;
  }
}
