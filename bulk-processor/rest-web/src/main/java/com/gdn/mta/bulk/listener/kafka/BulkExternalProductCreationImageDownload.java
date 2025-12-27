package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.product.BulkImageDownloadEventModel;
import com.gdn.mta.bulk.service.BulkProcessService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class BulkExternalProductCreationImageDownload {

  @Autowired
  private BulkProcessService bulkProcessService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkExternalCreateDownloadImageEvent()}",
    autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    BulkImageDownloadEventModel bulkImageDownloadEventModel =
      objectMapper.readValue(message, BulkImageDownloadEventModel.class);
    log.info("Download image {} with payload {} ",
      kafkaTopicProperties.getBulkExternalCreateDownloadImageEvent(), message);
    try {
      bulkProcessService.downloadImages(bulkImageDownloadEventModel.getBulkProcessCode(),
        bulkImageDownloadEventModel.getImageDownloadList());
    } catch (Exception e) {
      log.error("Error when listening to event {},  message : {} ",
        kafkaTopicProperties.getBulkExternalCreateDownloadImageEvent(), message, e);
    }
  }
}
