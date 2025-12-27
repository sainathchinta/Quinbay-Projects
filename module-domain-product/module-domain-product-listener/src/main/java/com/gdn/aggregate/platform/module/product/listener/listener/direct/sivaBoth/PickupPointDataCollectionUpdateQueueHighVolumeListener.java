package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.platform.module.product.listener.constants.Enabler;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.service.helper.ListenerService;
import brave.Tracer;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component
@ConditionalOnProperty(name = Enabler.FLOW_PICKUP_POINT_CHANGE_COMBINED_EVENT_HIGH_VOLUME)
public class PickupPointDataCollectionUpdateQueueHighVolumeListener {

  @Autowired
  private ListenerService listenerService;

  @Autowired
  private Tracer tracer;

  @KafkaListener(topics = Topics.NEW_PICKUP_POINT_UPSERT_EVENT_HIGH_VOLUME, 
                 groupId = Topics.GROUP_ID_SIVA_BOTH_BY_PICKUP_POINT_HIGH_VOLUME)
  public void processNewItemDataUpdateEvent(ConsumerRecord<String, String> record) {
    listenerService.onPickupPointCombinedUpsertEvent(record);
  }
}
