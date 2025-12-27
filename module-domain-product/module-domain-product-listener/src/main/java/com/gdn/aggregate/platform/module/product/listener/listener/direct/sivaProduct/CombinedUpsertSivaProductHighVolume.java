package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaProduct;

import com.gdn.aggregate.platform.module.product.listener.configurations.TraceHelper;
import com.gdn.aggregate.platform.module.product.listener.constants.Enabler;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.service.helper.ListenerService;
import io.micrometer.tracing.Tracer;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;

@Slf4j
@Component
@ConditionalOnProperty(name = Enabler.FLOW_SIVA_PRODUCT_COMBINED_UPSERT_HIGH_VOLUME)
public class CombinedUpsertSivaProductHighVolume {

  @Autowired
  private ListenerService listenerService;

  @Autowired
  private Tracer tracer;

  @KafkaListener(topics = Topics.SIVA_PRODUCT_COMBINED_UPSERT_HIGH_VOLUME, 
                 groupId = Topics.GROUP_ID_SIVA_PRODUCT_HIGH_VOLUME)
  public void onSivaProductCombinedUpsertEvent(ConsumerRecord<String, String> record) throws Exception {
    listenerService.onSivaProductCombinedUpsertEvent(record, TraceHelper.getTraceId(tracer));
  }
}
