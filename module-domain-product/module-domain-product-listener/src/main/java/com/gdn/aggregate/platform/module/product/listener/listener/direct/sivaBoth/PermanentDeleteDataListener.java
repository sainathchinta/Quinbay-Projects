package com.gdn.aggregate.platform.module.product.listener.listener.direct.sivaBoth;

import com.gdn.aggregate.platform.module.product.listener.configurations.TraceHelper;
import com.gdn.aggregate.platform.module.product.listener.constants.Enabler;
import com.gdn.aggregate.platform.module.product.listener.constants.Topics;
import com.gdn.aggregate.platform.module.product.listener.service.helper.ListenerService;
import com.gdn.aggregate.platform.module.product.listener.util.ModuleProductUtil;
import io.micrometer.tracing.Tracer;
import lombok.extern.slf4j.Slf4j;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import java.time.Duration;
import java.util.List;

@Slf4j
@Component
@ConditionalOnProperty(name = Enabler.FLOW_PERMANENT_DELETE_DATA)
public class PermanentDeleteDataListener {

  @Value("${permanent-delete.processing.bulk-size}")
  private int bulkSize;

  @Autowired
  private ListenerService listenerService;

  @Autowired
  private Tracer tracer;

  @KafkaListener(
    topics = Topics.PERMANENT_DELETE_DATA_FROM_AGP,
    groupId = Topics.GROUP_ID_PERMANENT_DELETE_DATA_FROM_AGP,
    containerFactory = "permanentDeletionContainerFactory"
  )
  public void onPermanentDeleteDataEvent(List<ConsumerRecord<String, String>> records) {

    String traceId = TraceHelper.getTraceId(tracer);
    int batchSize = records.size();
    long startTime = System.currentTimeMillis();

    log.info("Received PermanentDeleteData batch: size={}, traceId={}", batchSize, traceId);
    List<List<ConsumerRecord<String, String>>> batches =
      ModuleProductUtil.partitionConsumerRecords(records, bulkSize);

    // Increased parallelism: use at least 4 threads, or bulkSize/5 for larger bulks
    int parallelism = Math.min(2, Math.max(1, batches.size()));
    try {
      Flux.fromIterable(batches)
        .flatMap(batch -> processBatch(batch, traceId), parallelism)
        .doOnTerminate(() -> {
          long elapsed = System.currentTimeMillis() - startTime;
          log.info(
            "Completed processing PermanentDeleteData batch: size={}, timeTaken={}ms, traceId={}",
            batchSize, elapsed, traceId);
        })
        .then()
        .block(Duration.ofMinutes(15));
    } catch (Exception e) {
      log.error("Error processing PermanentDeleteData batch: size={}, traceId={}", batchSize, traceId, e);
      throw e;
    }
  }

  private Mono<Void> processBatch(List<ConsumerRecord<String, String>> recordBatch, String traceId) {
    return Mono.fromRunnable(() -> {
      int size = recordBatch.size();
      long start = System.currentTimeMillis();
      log.info("Processing sub-batch: size={}, traceId={}", size, traceId);

      listenerService.onPermanentDeleteDataEvent(recordBatch, traceId);

      long elapsed = System.currentTimeMillis() - start;
      log.info("Finished sub-batch: size={}, timeTaken={}ms, traceId={}", size, elapsed, traceId);
    });
  }

}
