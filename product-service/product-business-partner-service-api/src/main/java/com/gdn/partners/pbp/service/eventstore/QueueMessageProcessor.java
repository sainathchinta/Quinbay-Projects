package com.gdn.partners.pbp.service.eventstore;

public interface QueueMessageProcessor<T> {
  void process(T message);
}
