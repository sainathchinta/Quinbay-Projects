package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.platform.module.product.listener.configurations.TraceHelper;
import com.gdn.aggregate.platform.module.product.listener.constants.Enabler;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.service.helper.ListenerService;
import io.micrometer.tracing.Tracer;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;

@Component
@ConditionalOnProperty(name = Enabler.FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_STOCK_REPUBLISH, havingValue = "true")
public class DirectUpdateSivaBothByInventoryStockRepublish {

  @Autowired
  private ListenerService listenerService;

  @Autowired
  private Tracer tracer;

  @KafkaListener(topics = Topics.INVENTORY_ONLINE_REPUBLISH_EVENT, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_INVENTORY_INFO_CHANGE)
  public void onInventoryInfoChangeEventSivaBoth(ConsumerRecord<String, String> record) throws Exception {
    listenerService.publishPickupPointUpsertEventOnInventoryRepublish(record, TraceHelper.getTraceId(tracer));
  }
}
