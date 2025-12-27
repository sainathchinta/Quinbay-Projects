package com.gdn.partners.pbp.service.eventstore;

import com.gdn.common.base.entity.GdnBaseDomainEventModel;

public interface EventStoreKafkaMessageProcessor<T extends GdnBaseDomainEventModel> {
  void process(T eventModel, String eventName);

  default boolean allowedToRun() {
    return true;
  }
}
