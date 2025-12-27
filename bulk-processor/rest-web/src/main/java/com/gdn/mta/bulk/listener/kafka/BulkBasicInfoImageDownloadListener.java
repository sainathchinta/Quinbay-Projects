package com.gdn.mta.bulk.listener.kafka;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.product.BulkImageDownloadEventModel;
import com.gdn.mta.bulk.service.BulkBasicInfoUpdateService;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class BulkBasicInfoImageDownloadListener {

  @Autowired
  private BulkBasicInfoUpdateService bulkProcessService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkBasicInfoDownloadImageEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    try {
      BulkImageDownloadEventModel bulkImageDownloadEventModel =
          objectMapper.readValue(message, BulkImageDownloadEventModel.class);
      log.info("Download image {} with payload {}", kafkaTopicProperties.getBulkBasicInfoDownloadImageEvent(), message);
      bulkProcessService.downloadImages(bulkImageDownloadEventModel.getBulkProcessCode(),
          bulkImageDownloadEventModel.getImageDownloadList());
    } catch (Exception e) {
      log.error("Error when listening to event {} : {} ", kafkaTopicProperties.getBulkBasicInfoDownloadImageEvent(),
          message, e);
    }
  }

  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkBasicInfoDownloadImagePriority1()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedPriority1(String message) throws Exception {
    try {
      BulkImageDownloadEventModel bulkImageDownloadEventModel =
          objectMapper.readValue(message, BulkImageDownloadEventModel.class);
      log.info("Download image {} with payload {}", kafkaTopicProperties.getBulkBasicInfoDownloadImagePriority1(),
          message);
      bulkProcessService.downloadImages(bulkImageDownloadEventModel.getBulkProcessCode(),
          bulkImageDownloadEventModel.getImageDownloadList());
    } catch (Exception e) {
      log.error("Error when listening to event {} : {} ", kafkaTopicProperties.getBulkBasicInfoDownloadImagePriority1(),
          message, e);
    }
  }

  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkBasicInfoDownloadImagePriority2()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedPriority2(String message) throws Exception {
    try {
      BulkImageDownloadEventModel bulkImageDownloadEventModel =
          objectMapper.readValue(message, BulkImageDownloadEventModel.class);
      log.info("Download image {} with payload {}", kafkaTopicProperties.getBulkBasicInfoDownloadImagePriority2(),
          message);
      bulkProcessService.downloadImages(bulkImageDownloadEventModel.getBulkProcessCode(),
          bulkImageDownloadEventModel.getImageDownloadList());
    } catch (Exception e) {
      log.error("Error when listening to event {} : {} ", kafkaTopicProperties.getBulkBasicInfoDownloadImagePriority2(),
          message, e);
    }
  }
}
