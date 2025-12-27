package com.gdn.mta.bulk.listener.kafka;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.BulkDataDeletionModel;
import com.gdn.mta.bulk.service.BulkProcessService;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class BulkDataDeletionListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkDataDeletionEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    try {
      log.info("Consumed event {}, payload : {} ", kafkaTopicProperties.getBulkDataDeletionEvent(), message);
      BulkDataDeletionModel bulkDataDeletionModel = objectMapper.readValue(message, BulkDataDeletionModel.class);
      bulkProcessService.deleteBulkProcessDataByBulkProcessCode(bulkDataDeletionModel.getStoreId(),
          bulkDataDeletionModel.getBulkProcessCode());
    } catch (Exception e) {
      log.error("Error while listening event {} ", kafkaTopicProperties.getBulkDataDeletionEvent(), e);
    }
  }
}
