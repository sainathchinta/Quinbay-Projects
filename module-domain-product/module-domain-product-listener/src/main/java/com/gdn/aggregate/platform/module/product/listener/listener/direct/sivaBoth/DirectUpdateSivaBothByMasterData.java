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
@ConditionalOnProperty(name = Enabler.FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_MASTER_DATA)
public class DirectUpdateSivaBothByMasterData {

    @Autowired
    private ListenerService listenerService;

    @Autowired
    private Tracer tracer;

    @KafkaListener(topics = Topics.MASTER_DATA, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_MASTER_DATA)
    public void onMasterDataEventSivaBoth(ConsumerRecord<String, String> record) throws Exception {
        listenerService.onMasterDataEvent(record,Topics.GROUP_ID_SIVA_BOTH_BY_MASTER_DATA,false,
                TraceHelper.getTraceId(tracer));
    }

    @KafkaListener(topics = Topics.ALL_MASTER_DATA, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_MASTER_DATA)
    public void onAllMasterDataEventSivaBoth(ConsumerRecord<String, String> record) throws Exception {
        listenerService.onMasterDataEvent(record,Topics.GROUP_ID_SIVA_BOTH_BY_MASTER_DATA,true,
          TraceHelper.getTraceId(tracer));
    }

}
