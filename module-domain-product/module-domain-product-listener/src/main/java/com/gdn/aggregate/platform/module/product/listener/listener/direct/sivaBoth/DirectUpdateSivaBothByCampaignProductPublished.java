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
@ConditionalOnProperty(name = Enabler.FLOW_DIRECT_UPDATE_SIVA_BOTH_BY_CAMPAIGN_PRODUCT_PUBLISHED)
public class DirectUpdateSivaBothByCampaignProductPublished {

    @Autowired
    private ListenerService listenerService;

    @Autowired
    private Tracer tracer;

    @KafkaListener(topics = Topics.CAMPAIGN_PRODUCT_PUBLISHED, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_CAMPAIGN_PRODUCT_PUBLISHED)
    public void onCampaignProductPublishedEventSivaProduct(ConsumerRecord<String, String> record) throws Exception {
        listenerService.onCampaignProductPublishedEvent(record,
                Topics.GROUP_ID_SIVA_BOTH_BY_CAMPAIGN_PRODUCT_PUBLISHED, TraceHelper.getTraceId(tracer));
    }

    @KafkaListener(topics = Topics.CAMPAIGN_PRODUCT_TAG_LABEL_CHANGE, groupId = Topics.GROUP_ID_SIVA_BOTH_BY_CAMPAIGN_PRODUCT_TAG_LABEL_CHANGE)
    public void onCampaignProductTagLabelChangeEventSivaProduct(ConsumerRecord<String, String> record) throws Exception {
        listenerService.onCampaignProductTagLabelEvent(record,
                Topics.GROUP_ID_SIVA_BOTH_BY_CAMPAIGN_PRODUCT_TAG_LABEL_CHANGE,TraceHelper.getTraceId(tracer));
    }

}
