package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.platform.module.product.listener.configurations.TraceHelper;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.constants.Enabler;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.service.helper.ListenerService;
import io.micrometer.tracing.Tracer;

@Component
@ConditionalOnProperty(name = Enabler.FLOW_INVENTORY_INFO_COMBINED_UPSERT, havingValue = "true")
public class DirectUpdateSivaBothByInventoryInfoNewListener {

  private static final Logger log = LoggerFactory.getLogger(DirectUpdateSivaBothByPickupPoint.class);

  @Autowired
  private ListenerService listenerService;

  @Autowired
  private Tracer tracer;

  @KafkaListener(topics = Topics.INVENTORY_INFO_CHANGE, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_INVENTORY_INFO_CHANGE)
  public void onInventoryInfoChangeEventSivaBoth(ConsumerRecord<String, String> record) throws Exception {
    log.info("Consumed new inventory info event {} , Topic : {} ", record, record.topic());
    listenerService.onInventoryInfoChangeNewEvent(record, TraceHelper.getTraceId(tracer));
  }
}
