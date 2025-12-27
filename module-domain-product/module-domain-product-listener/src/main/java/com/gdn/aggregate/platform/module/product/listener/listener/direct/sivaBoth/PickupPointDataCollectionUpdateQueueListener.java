package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.service.helper.ListenerService;
import brave.Tracer;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component
public class PickupPointDataCollectionUpdateQueueListener {

  @Autowired
  private ListenerService listenerService;

  @Autowired
  private Tracer tracer;

  @KafkaListener(topics = Topics.NEW_PICKUP_POINT_UPSERT_EVENT, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_PICKUP_POINT)
  public void processNewItemDataUpdateEvent(ConsumerRecord<String, String> record) {
    listenerService.onPickupPointCombinedUpsertEvent(record);
  }
}
