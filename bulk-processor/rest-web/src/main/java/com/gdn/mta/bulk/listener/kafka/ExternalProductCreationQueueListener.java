package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkProcessExternalUploadRequest;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.gdn.mta.bulk.service.ExternalProductCreationService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.Objects;

@Service
@Slf4j
@ConditionalOnProperty(value = "external.bulk.other.marketplace.upload.enabled", havingValue = "true")
public class ExternalProductCreationQueueListener {

  @Autowired
  private ExternalProductCreationService externalProductCreationService;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkExternalCreateEvent()}",
    autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Consume event {} with message {} ", kafkaTopicProperties.getBulkExternalCreateEvent(),
      message);
    BulkProcessExternalUploadRequest bulkProcessExternalUploadRequest = null;
    try {
      bulkProcessExternalUploadRequest =
        objectMapper.readValue(message, BulkProcessExternalUploadRequest.class);
      externalProductCreationService.process(bulkProcessExternalUploadRequest);
    } catch (Exception e) {
      if (Objects.nonNull(bulkProcessExternalUploadRequest)) {
        log.error(
          "Error processing Kafka message | topic={} | requestId={} | bulkProcessCode={} | error={}",
          kafkaTopicProperties.getBulkExternalCreateEvent(),
          bulkProcessExternalUploadRequest.getRequestId(),
          bulkProcessExternalUploadRequest.getBulkProcessCode(), e.getMessage(), e);
        bulkProcessService.abortBulkProcess(bulkProcessExternalUploadRequest.getStoreId(),
          bulkProcessExternalUploadRequest.getBulkProcessCode());
      }
    }
  }
}
