package com.gdn.mta.product.service;

import com.gdn.mta.domain.event.modal.ItemStatusDomainEvent;

public interface ItemStatusPublisherService {

  ItemStatusDomainEvent publishItemStatusDomainEvent(ItemStatusDomainEvent itemStatusDomainEvent);
}
