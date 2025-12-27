package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaFlashsaleGroup;

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
@ConditionalOnProperty(name = Enabler.FLOW_DIRECT_UPDATE_SIVA_FLASHSALE_GROUP)
public class DirectUpdateSivaFlashsaleGroupListener {

    @Autowired
    private ListenerService listenerService;

    @Autowired
    private Tracer tracer;

    @KafkaListener(topics = Topics.FLASHSALE_GROUP, groupId = Topics.GROUP_ID_SIVA_FLASHSALE_GROUP)
    public void onFlashsaleGroupEventSivaFlashsaleGroup(ConsumerRecord<String, String> record)
      throws Exception {
        listenerService.onFlashsaleGroupEventSivaFlashsaleGroup(record,
          Topics.GROUP_ID_SIVA_FLASHSALE_GROUP, TraceHelper.getTraceId(tracer));
    }

    @KafkaListener(topics = Topics.CLEAN_FLASHSALE_GROUP, groupId = Topics.GROUP_ID_SIVA_FLASHSALE_GROUP_BY_CLEAN)
    public void onCleanEventSivaFlashsaleGroup(ConsumerRecord<String, String> record)
      throws Exception {
        listenerService.onCleanEventSivaFlashsaleGroup(record,
          Topics.GROUP_ID_SIVA_FLASHSALE_GROUP_BY_CLEAN, TraceHelper.getTraceId(tracer));
    }

    @KafkaListener(topics = Topics.DEACTIVATE_FLASHSALE_GROUP, groupId = Topics.GROUP_ID_SIVA_FLASHSALE_GROUP_BY_DEACTIVATE)
    public void onDeactivateEventSivaFlashsaleGroup(ConsumerRecord<String, String> record)
      throws Exception {
        listenerService.onDeactivateEventSivaFlashsaleGroup(record,
          Topics.GROUP_ID_SIVA_FLASHSALE_GROUP_BY_DEACTIVATE, TraceHelper.getTraceId(tracer));
    }

    @KafkaListener(topics = Topics.SCHEDULE_DELETE_ALL_EXPIRED_SIVA_FLASHSALE_GROUP, groupId = Topics.GROUP_ID_SIVA_FLASHSALE_GROUP_BY_ALL_EXPIRED)
    public void onFlashsaleGroupDeleteAllExpiredEventSivaFlashsaleGroup(
      ConsumerRecord<String, String> record) throws Exception {
        listenerService.onDeleteAllExpiredEventSivaFlashsaleGroup(record,
          Topics.GROUP_ID_SIVA_FLASHSALE_GROUP_BY_ALL_EXPIRED, TraceHelper.getTraceId(tracer));
    }

}
