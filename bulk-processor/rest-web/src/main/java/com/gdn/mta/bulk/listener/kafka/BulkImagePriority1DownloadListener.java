package com.gdn.mta.bulk.listener.kafka;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.product.BulkImageDownloadEventModel;
import com.gdn.mta.bulk.service.BulkProcessService;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class BulkImagePriority1DownloadListener {

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkCreateDownloadImagePriority1()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    BulkImageDownloadEventModel bulkImageDownloadEventModel =
        objectMapper.readValue(message, BulkImageDownloadEventModel.class);
    log.info("Download image {} with payload {}", kafkaTopicProperties.getBulkCreateDownloadImagePriority1(), message);
    try {
      bulkProcessService.downloadImages(bulkImageDownloadEventModel.getBulkProcessCode(),
          bulkImageDownloadEventModel.getImageDownloadList());
    } catch (Exception e) {
      log.error("Error when listening to event {} : {} ", kafkaTopicProperties.getBulkCreateDownloadImagePriority1(),
          message, e);
    }
  }
}
