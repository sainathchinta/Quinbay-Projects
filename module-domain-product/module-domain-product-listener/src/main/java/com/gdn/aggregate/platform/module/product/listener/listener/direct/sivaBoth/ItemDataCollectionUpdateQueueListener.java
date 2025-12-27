package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.platform.module.product.listener.configurations.TraceHelper;
import io.micrometer.tracing.Tracer;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.constants.Enabler;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.service.helper.ListenerService;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component
@ConditionalOnProperty(name = Enabler.FLOW_RAW_ITEM_COMBINED_UPSERT)
public class ItemDataCollectionUpdateQueueListener {

  @Autowired
  private ListenerService listenerService;

  @Autowired
  private Tracer tracer;

  @KafkaListener(topics = Topics.NEW_RAW_ITEM_COLLECTION_UPDATE_EVENT, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_ITEM)
  public void processNewItemDataUpdateEvent(ConsumerRecord<String, String> record) {
    listenerService.onNewItemEventV2(record, Topics.GROUP_ID_SIVA_BOTH_BY_ITEM, TraceHelper.getTraceId(tracer));
  }
}
