package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.platform.module.product.listener.configurations.TraceHelper;
import com.gdn.aggregate.platform.module.product.listener.constants.Enabler;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.service.helper.ListenerService;
import io.micrometer.tracing.Tracer;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;

import java.util.List;

@Slf4j
@Component
@ConditionalOnProperty(name = Enabler.FLOW_PERMANENT_DELETE_DATA)
public class ProductElasticSearchDeletionListener {

  @Autowired
  private ListenerService listenerService;

  @Autowired
  private Tracer tracer;

  @KafkaListener(topics = Topics.PRODUCT_ES_DELETION_EVENT, groupId = Topics.GROUP_ID_PERMANENT_DELETE_DATA_FROM_AGP,
    containerFactory = "permanentDeletionContainerFactory")
  public void onPermanentDeleteDataEvent(List<ConsumerRecord<String, String>> records) {
    String traceId = TraceHelper.getTraceId(tracer);
    log.info("Received {} ES deletion event record(s), traceId={}",records, traceId);
    
    for (ConsumerRecord<String, String> record : records) {
      try {
        log.debug("Processing ES deletion record: key={}, value={}, traceId={}", 
          record.key(), record.value(), traceId);
        listenerService.onElasticSearchDeletionEvent(record, traceId);
      } catch (Exception e) {
        log.error("Failed to process record [{}] traceId: {} error: {}", record.key(), traceId,
          e.getMessage(), e);
      }
    }
  }
}
