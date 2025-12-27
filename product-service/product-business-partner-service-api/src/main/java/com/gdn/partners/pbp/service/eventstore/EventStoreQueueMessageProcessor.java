package com.gdn.partners.pbp.service.eventstore;

import com.gdn.partners.pbp.model.vo.EventStoreQueue;

public interface EventStoreQueueMessageProcessor<T> extends QueueMessageProcessor<EventStoreQueue<T>> {

  @Override
  public default void process(EventStoreQueue<T> message) {
    doProcessMessage(message);
    if (message.isForIndexing()) {
      doIndexingProcess(message);
    }
  }

  /**
   * Implementation of queue message processing
   * 
   * @param message
   */
  void doProcessMessage(EventStoreQueue<T> message);

  /**
   * Implementation of indexing process if message.isForIndexing = true
   * 
   * @param message
   */
  void doIndexingProcess(EventStoreQueue<T> message);


}
