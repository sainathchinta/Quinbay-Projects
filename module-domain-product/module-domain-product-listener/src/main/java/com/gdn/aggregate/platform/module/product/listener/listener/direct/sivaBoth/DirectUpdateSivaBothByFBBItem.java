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
@ConditionalOnProperty(name = Enabler.FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_FBB_ITEM)
public class DirectUpdateSivaBothByFBBItem {

    @Autowired
    private ListenerService listenerService;

    @Autowired
    private Tracer tracer;

    @KafkaListener(topics = {Topics.FBB_ITEM_SAVE}, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_FBB_ITEM)
    public void onFBBItemEventSivaItem(ConsumerRecord<String, String> record) {
        listenerService.onFBBItemEvent(record,Topics.GROUP_ID_SIVA_BOTH_BY_FBB_ITEM,false,
                TraceHelper.getTraceId(tracer));
    }

}
