package com.gdn.mta.bulk.listener.kafka;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.product.BulkBasicInfoVideoDownloadResponseModel;
import com.gdn.mta.bulk.service.BulkBasicInfoUpdateService;
import com.gdn.partners.bulk.util.Constant;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class BulkBasicInfoVideoDownloadListener {

  @Autowired
  private BulkBasicInfoUpdateService bulkBasicInfoUpdateService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Value("${video.client.id}")
  private String videoClientId;
  
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkBasicInfoDownloadVideoResponseEvent()}",
      autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    try {
      BulkBasicInfoVideoDownloadResponseModel bulkBasicInfoVideoDownloadResponseModel =
          objectMapper.readValue(message, BulkBasicInfoVideoDownloadResponseModel.class);
      log.info("Download video event received {} with payload {}",
          kafkaTopicProperties.getBulkBasicInfoDownloadVideoResponseEvent(), message);
      if (videoClientId.equalsIgnoreCase(bulkBasicInfoVideoDownloadResponseModel.getClientId())) {
        bulkBasicInfoUpdateService.processBulkProcessVideoUpdate(Constant.STORE_ID,
          bulkBasicInfoVideoDownloadResponseModel);
      }
    } catch (Exception e) {
      log.error("Error when listening to event {} : {} ",
          kafkaTopicProperties.getBulkBasicInfoDownloadVideoResponseEvent(), message, e);
    }
  }
}
