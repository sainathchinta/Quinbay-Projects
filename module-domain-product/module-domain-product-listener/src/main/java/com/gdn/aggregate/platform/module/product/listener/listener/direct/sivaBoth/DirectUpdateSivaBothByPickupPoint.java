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
@ConditionalOnProperty(name = Enabler.FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_PICKUP_POINT)
public class DirectUpdateSivaBothByPickupPoint {

    private static final Logger log = LoggerFactory.getLogger(DirectUpdateSivaBothByPickupPoint.class);
    @Autowired
    private ListenerService listenerService;

    @Autowired
    private Tracer tracer;

    @KafkaListener(topics = Topics.PICKUP_POINT, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_PICKUP_POINT)
    public void onPickupPointEventSivaBoth(ConsumerRecord<String, String> record) throws Exception {
        listenerService.onPickupPointEventV2(record, Topics.GROUP_ID_SIVA_BOTH_BY_PICKUP_POINT,
          false, false, TraceHelper.getTraceId(tracer));
    }

    @KafkaListener(topics = Topics.ALL_PICKUP_POINT, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_PICKUP_POINT)
    public void onAllPickupPointEventSivaBoth(ConsumerRecord<String, String> record)
      throws Exception {
        listenerService.onPickupPointEventV2(record, Topics.GROUP_ID_SIVA_BOTH_BY_PICKUP_POINT,
          true, true, TraceHelper.getTraceId(tracer));
    }
}
