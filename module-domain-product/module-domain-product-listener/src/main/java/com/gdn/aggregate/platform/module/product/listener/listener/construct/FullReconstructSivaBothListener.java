package com.gdn.aggregate.platform.module.product.listener.listener.construct;

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
@ConditionalOnProperty(name = Enabler.FLOW_FULL_RECONSTRUCT)
public class FullReconstructSivaBothListener {

  @Autowired
  private ListenerService listenerService;

  @Autowired
  private Tracer tracer;

  @KafkaListener(topics = Topics.PRODUCT_DENPASAR, groupId = Topics.GROUP_ID_FULL_RECONSTRUCT_BY_PRODUCT_SKU)
  public void onDirectConstructSivaProductByProductSku(ConsumerRecord<String, String> record) throws Exception {
    listenerService.fullReconstructByProductSku(record,Topics.GROUP_ID_FULL_RECONSTRUCT_BY_PRODUCT_SKU,true,
      TraceHelper.getTraceId(tracer));
  }

  @KafkaListener(topics = Topics.ITEM_DENPASAR, groupId = Topics.GROUP_ID_FULL_RECONSTRUCT_BY_ITEM_SKU)
  public void onDirectConstructSivaProductByItemSku(ConsumerRecord<String, String> record) throws Exception {
    listenerService.fullReconstructByItemSku(record,Topics.GROUP_ID_FULL_RECONSTRUCT_BY_ITEM_SKU,true,
            TraceHelper.getTraceId(tracer));
  }

}
