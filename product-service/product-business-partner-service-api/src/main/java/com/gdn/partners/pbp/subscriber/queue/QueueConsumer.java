package com.gdn.partners.pbp.subscriber.queue;

public interface QueueConsumer<T> {

  void consume(T message);
}
