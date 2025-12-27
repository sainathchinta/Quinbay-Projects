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
@ConditionalOnProperty(name = Enabler.FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_FBB_TAG)
public class DirectUpdateSivaBothByBusinessPartnerProfileChange {

    @Autowired
    private ListenerService listenerService;

    @Autowired
    private Tracer tracer;

    @KafkaListener(topics = Topics.BUSINESS_PARTNER_PROFILE_CHANGE, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_BUSINESS_PARTNER_PROFILE_CHANGE)
    public void onBusinessPartnerProfileChangeEventSivaBoth(ConsumerRecord<String, String> record) throws Exception {
        listenerService.onBusinessPartnerProfileChangeEvent(record,
                Topics.GROUP_ID_SIVA_BOTH_BY_BUSINESS_PARTNER_PROFILE_CHANGE, TraceHelper.getTraceId(tracer));
    }

    @KafkaListener(topics = Topics.FBB_CHANGE, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_FBB_CHANGE)
    public void onFbbChangeEventSivaBoth(ConsumerRecord<String, String> record) throws Exception {
        listenerService.onFbbChangeEvent(record,Topics.GROUP_ID_SIVA_BOTH_BY_FBB_CHANGE,
          TraceHelper.getTraceId(tracer));
    }

}
