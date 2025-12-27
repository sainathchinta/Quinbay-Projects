package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.platform.module.product.listener.configurations.TraceHelper;
import io.micrometer.tracing.Tracer;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.constants.Enabler;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.service.helper.ListenerService;

@Slf4j
@Component
@ConditionalOnProperty(name = Enabler.FLOW_RAW_ITEM_COMBINED_UPSERT)
public class UpdateSivaBothByItemNewListener {

  @Autowired
  private ListenerService listenerService;

  @Autowired
  private Tracer tracer;

  @KafkaListener(topics = Topics.ITEM, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_ITEM)
  public void onItemEventSivaProduct(ConsumerRecord<String, String> record) throws Exception {
    log.info("Publishing new item data collection update event {} , message {} ",
        Topics.NEW_RAW_ITEM_COLLECTION_UPDATE_EVENT, record.value());
    listenerService.onItemUpdateNewEvent(record, false, TraceHelper.getTraceId(tracer));
  }

  @KafkaListener(topics = Topics.ALL_ITEM, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_ITEM)
  public void onAllItemEventSivaProduct(ConsumerRecord<String, String> record) throws Exception {
    log.info("Publishing new item data collection update event from all item {} message {} ",
        Topics.NEW_RAW_ITEM_COLLECTION_UPDATE_EVENT, record.value());
    listenerService.onItemUpdateNewEvent(record, true, TraceHelper.getTraceId(tracer));
  }
}
