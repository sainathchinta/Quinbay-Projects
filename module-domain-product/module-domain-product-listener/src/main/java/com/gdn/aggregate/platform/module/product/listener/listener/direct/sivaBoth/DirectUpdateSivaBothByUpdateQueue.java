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
@ConditionalOnProperty(name = Enabler.FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_UPDATE_QUEUE)
public class DirectUpdateSivaBothByUpdateQueue {

    @Autowired
    private ListenerService listenerService;

    @Autowired
    private Tracer tracer;

    @KafkaListener(topics = {Topics.UPDATE_QUEUE_DENPASAR, Topics.UPDATE_QUEUE_MODULE_PRODUCT}, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_UPDATE_QUEUE_L3)
    public void onUpdateQueueL3EventSivaBoth(ConsumerRecord<String, String> record) throws Exception {
        listenerService.onUpdateQueueEvent(record,Topics.GROUP_ID_SIVA_BOTH_BY_UPDATE_QUEUE_L3,false,3,
          TraceHelper.getTraceId(tracer));
    }

    @KafkaListener(topics = {Topics.UPDATE_QUEUE_DENPASAR, Topics.UPDATE_QUEUE_MODULE_PRODUCT}, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_UPDATE_QUEUE_L4)
    public void onUpdateQueueL4EventSivaBoth(ConsumerRecord<String, String> record) throws Exception {
        listenerService.onUpdateQueueEvent(record,Topics.GROUP_ID_SIVA_BOTH_BY_UPDATE_QUEUE_L4,false,4,
          TraceHelper.getTraceId(tracer));
    }

}
