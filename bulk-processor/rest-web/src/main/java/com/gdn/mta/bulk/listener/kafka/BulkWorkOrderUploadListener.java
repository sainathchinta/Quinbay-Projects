package com.gdn.mta.bulk.listener.kafka;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.WorkOrderEventModel;
import com.gdn.mta.bulk.service.BulkWorkOrderServiceWrapper;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class BulkWorkOrderUploadListener {
  @Autowired
  private BulkWorkOrderServiceWrapper bulkWorkOrderServiceWrapper;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkWorkOrderUploadEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    try {
      WorkOrderEventModel workOrderEventModel = objectMapper.readValue(message, WorkOrderEventModel.class);
      log.info("On domain event consumed for work order upload : {}, workOrderEventModel : {} ",
          kafkaTopicProperties.getBulkWorkOrderUploadEvent(), workOrderEventModel);
      bulkWorkOrderServiceWrapper.processBulkWorkOrderUpload(workOrderEventModel);
    } catch (Exception e) {
      log.error("Error caught while work order upload. message : {}", message, e);
    }
  }
}
