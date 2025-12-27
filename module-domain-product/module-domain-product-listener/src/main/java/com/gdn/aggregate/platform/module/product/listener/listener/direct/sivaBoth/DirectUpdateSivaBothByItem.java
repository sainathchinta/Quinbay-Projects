package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.platform.module.product.listener.configurations.TraceHelper;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;

import com.gdn.aggregate.platform.module.product.listener.constants.Enabler;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.service.helper.ListenerService;
import io.micrometer.tracing.Tracer;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component
@ConditionalOnProperty(name = Enabler.FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_ITEM)
public class DirectUpdateSivaBothByItem {

    @Autowired
    private ListenerService listenerService;

    @Autowired
    private Tracer tracer;

    @KafkaListener(topics = Topics.ITEM, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_ITEM)
    public void onItemEventSivaProduct(ConsumerRecord<String, String> record) throws Exception {
        listenerService.onItemEventV2(record, Topics.GROUP_ID_SIVA_BOTH_BY_ITEM, false,
          TraceHelper.getTraceId(tracer));
    }

    @KafkaListener(topics = Topics.ALL_ITEM, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_ITEM)
    public void onAllItemEventSivaProduct(ConsumerRecord<String, String> record) throws Exception {
        listenerService.onItemEventV2(record, Topics.GROUP_ID_SIVA_BOTH_BY_ITEM, true,
          TraceHelper.getTraceId(tracer));
    }

}
