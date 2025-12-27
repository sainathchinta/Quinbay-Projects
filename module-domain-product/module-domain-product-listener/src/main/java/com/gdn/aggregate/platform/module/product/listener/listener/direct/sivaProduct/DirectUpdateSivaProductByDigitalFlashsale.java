package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaProduct;

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
@ConditionalOnProperty(name = Enabler.FLOW_DIRECT_UPDATE_SIVA_PRODUCT_BY_DIGITAL_FLASHALE)
public class DirectUpdateSivaProductByDigitalFlashsale {

    @Autowired
    private ListenerService listenerService;

    @Autowired
    private Tracer tracer;

    @KafkaListener(topics = Topics.DIGITAL_FLASHSALE, groupId = Topics.GROUP_ID_SIVA_PRODUCT_BY_DIGITAL_FLASHSALE)
    public void onDigitalFlashsaleEventSivaProduct(ConsumerRecord<String, String> record)
      throws Exception {
        listenerService.onDigitalFlashsaleEvent(record,
          Topics.GROUP_ID_SIVA_PRODUCT_BY_DIGITAL_FLASHSALE, false, TraceHelper.getTraceId(tracer));
    }

    @KafkaListener(topics = Topics.ALL_DIGITAL_FLASHSALE, groupId = Topics.GROUP_ID_SIVA_PRODUCT_BY_DIGITAL_FLASHSALE)
    public void onAllDigitalFlashsaleEventSivaProduct(ConsumerRecord<String, String> record)
      throws Exception {
        listenerService.onDigitalFlashsaleEvent(record,
          Topics.GROUP_ID_SIVA_PRODUCT_BY_DIGITAL_FLASHSALE, true, TraceHelper.getTraceId(tracer));
    }

}
