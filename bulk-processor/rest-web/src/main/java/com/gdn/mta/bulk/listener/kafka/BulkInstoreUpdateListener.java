package com.gdn.mta.bulk.listener.kafka;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.service.BulkInstoreUpdateService;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class BulkInstoreUpdateListener {

  @Autowired
  private BulkInstoreUpdateService bulkInstoreUpdateService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkUploadInstoreUpdate()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    try {
      BulkUpdateEventModel bulkUpdateEventModel = objectMapper.readValue(message, BulkUpdateEventModel.class);
      log.info("On domain event consumed for instore update : {}, bulkUpdateEventModel : {} ",
          kafkaTopicProperties.getBulkUploadInstoreUpdate(), bulkUpdateEventModel);
      bulkInstoreUpdateService.processInstoreUpdateEvent(bulkUpdateEventModel);
    } catch (Exception e) {
      log.error("Error counght while instore update. message : {}", message, e);
    }
  }
}
