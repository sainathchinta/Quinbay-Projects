package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaCampaignProduct;

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
import lombok.extern.slf4j.Slf4j;

@Component
@ConditionalOnProperty(name = Enabler.FLOW_DIRECT_UPDATE_SIVA_CAMPAIGN_PRODUCT)
@Slf4j
public class DirectUpdateSivaCampaignProductListener {

  @Autowired
  private ListenerService listenerService;

  @Autowired
  private Tracer tracer;

    @KafkaListener(topics = Topics.CAMPAIGN_PRODUCT_LIVE, groupId = Topics.GROUP_ID_SIVA_CAMPAIGN_PRODUCT)
    public void onCampaignProductLiveEventSivaCampaignProduct(ConsumerRecord<String, String> record) throws Exception {
      listenerService.onCampaignProductLiveEventSivaCampaignProduct(record,Topics.GROUP_ID_SIVA_CAMPAIGN_PRODUCT, TraceHelper.getTraceId(tracer));
    }

    @KafkaListener(topics = Topics.CAMPAIGN_PRODUCT_ENDED, groupId = Topics.GROUP_ID_SIVA_CAMPAIGN_PRODUCT_BY_CAMPAIGN_PRODUCT_ENDED)
    public void onCampaignProductEndedEventSivaCampaignProduct(ConsumerRecord<String, String> record) throws Exception {
      listenerService.onCampaignProductEndedEventSivaCampaignProduct(record,Topics.GROUP_ID_SIVA_CAMPAIGN_PRODUCT_BY_CAMPAIGN_PRODUCT_ENDED, TraceHelper.getTraceId(tracer));
    }

    @KafkaListener(topics = Topics.CAMPAIGN_TEASER_LIVE, groupId = Topics.GROUP_ID_SIVA_CAMPAIGN_PRODUCT_BY_CAMPAIGN_TEASER_LIVE)
    public void onCampaignTeaserLiveEventSivaCampaignProduct(ConsumerRecord<String, String> record) throws Exception {
      listenerService.onCampaignTeaserLiveEventSivaCampaignProduct(record,Topics.GROUP_ID_SIVA_CAMPAIGN_PRODUCT_BY_CAMPAIGN_TEASER_LIVE, TraceHelper.getTraceId(tracer));
    }

    @KafkaListener(topics = Topics.SCHEDULE_DELETE_ALL_EXPIRED_SIVA_CAMPAIGN_PRODUCT, groupId = Topics.GROUP_ID_SIVA_CAMPAIGN_PRODUCT_BY_ALL_EXPIRED)
    public void onCampaignProductDeleteAllExpiredEventSivaCampaignProduct(ConsumerRecord<String, String> record) throws Exception {
      listenerService.onCampaignProductDeleteAllExpiredEventSivaCampaignProduct(record,Topics.GROUP_ID_SIVA_CAMPAIGN_PRODUCT_BY_ALL_EXPIRED, TraceHelper.getTraceId(tracer));
    }

}
