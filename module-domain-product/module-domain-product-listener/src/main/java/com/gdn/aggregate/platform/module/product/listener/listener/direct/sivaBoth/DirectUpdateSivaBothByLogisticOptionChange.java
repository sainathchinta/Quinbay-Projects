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
@ConditionalOnProperty(name = Enabler.FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_LOGISTIC_OPTION_CHANGE)
public class DirectUpdateSivaBothByLogisticOptionChange {

    @Autowired
    private ListenerService listenerService;

    @Autowired
    private Tracer tracer;

    @KafkaListener(topics = Topics.DENPASAR_SHIPPING_LOGISTIC_OPTION_CHANGE, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_LOGISTIC_CHANGE)
    public void onDenpasarShippingLogisticOptionChangeEventSivaBoth(ConsumerRecord<String, String> record) throws Exception {
        listenerService.onDenpasarShippingLogisticOptionChangeEvent(record,
                Topics.GROUP_ID_SIVA_BOTH_BY_LOGISTIC_CHANGE, TraceHelper.getTraceId(tracer));
    }

    @KafkaListener(topics = Topics.DENPASAR_SEARCH_LOGISTIC_OPTION_CHANGE, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_LOGISTIC_CHANGE)
    public void onDenpasarSearchLogisticOptionChangeEventSivaBoth(ConsumerRecord<String, String> record) throws Exception {
        listenerService.onDenpasarSearchLogisticOptionChangeEvent(record,Topics.GROUP_ID_SIVA_BOTH_BY_LOGISTIC_CHANGE
                , TraceHelper.getTraceId(tracer));
    }

}
