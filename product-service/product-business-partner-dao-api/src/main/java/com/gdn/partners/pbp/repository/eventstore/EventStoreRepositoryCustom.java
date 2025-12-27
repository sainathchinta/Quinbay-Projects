package com.gdn.partners.pbp.repository.eventstore;

import java.util.List;

import com.gdn.partners.pbp.entity.eventstore.EventStore;

public interface EventStoreRepositoryCustom {
  List<EventStore> findByLastUpdateEventAndGroupByItemSku();
}
