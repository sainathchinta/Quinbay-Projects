package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaFlashsaleSchedule;

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
@ConditionalOnProperty(name = Enabler.FLOW_DIRECT_UPDATE_SIVA_FLASHSALE_SCHEDULE)
public class DirectUpdateSivaFlashsaleScheduleListener {

    @Autowired
    private ListenerService listenerService;

    @Autowired
    private Tracer tracer;

    @KafkaListener(topics = Topics.FLASHSALE_SCHEDULE, groupId = Topics.GROUP_ID_SIVA_FLASHSALE_SCHEDULE)
    public void onFlashsaleScheduleEventSivaFlashsaleSchedule(ConsumerRecord<String, String> record) throws Exception {
      listenerService.onFlashsaleScheduleEventSivaFlashsaleSchedule(record,Topics.GROUP_ID_SIVA_FLASHSALE_SCHEDULE,TraceHelper.getTraceId(tracer));
    }

    @KafkaListener(topics = Topics.CLEAN_FLASHSALE_SCHEDULE, groupId = Topics.GROUP_ID_SIVA_FLASHSALE_SCHEDULE_BY_CLEAN)
    public void onCleanEventSivaFlashsaleSchedule(ConsumerRecord<String, String> record) throws Exception {
      listenerService.onCleanEventSivaFlashsaleSchedule(record,Topics.GROUP_ID_SIVA_FLASHSALE_SCHEDULE_BY_CLEAN,TraceHelper.getTraceId(tracer));
    }

    @KafkaListener(topics = Topics.DEACTIVATE_FLASHSALE_SCHEDULE, groupId = Topics.GROUP_ID_SIVA_FLASHSALE_SCHEDULE_BY_DEACTIVATE)
    public void onDeactivateEventSivaFlashsaleSchedule(ConsumerRecord<String, String> record) throws Exception {
      listenerService.onDeactivateEventSivaFlashsaleSchedule(record,Topics.GROUP_ID_SIVA_FLASHSALE_SCHEDULE_BY_DEACTIVATE,TraceHelper.getTraceId(tracer));
    }

    @KafkaListener(topics = Topics.SCHEDULE_DELETE_ALL_EXPIRED_SIVA_FLASHSALE_SCHEDULE, groupId = Topics.GROUP_ID_SIVA_FLASHSALE_SCHEDULE_BY_ALL_EXPIRED)
    public void onDeleteAllExpiredEventSivaFlashsaleSchedule(ConsumerRecord<String, String> record) throws Exception {
        listenerService.onDeleteAllExpiredEventSivaFlashsaleSchedule(record,Topics.GROUP_ID_SIVA_FLASHSALE_SCHEDULE_BY_ALL_EXPIRED,
          TraceHelper.getTraceId(tracer));
    }

}
