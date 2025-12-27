package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.service.BulkBasicInfoUpdateService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class BulkBasicInfoUpdateListenerServiceBean {

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private BulkBasicInfoUpdateService bulkBasicInfoUpdateService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkBasicInfoUpdateEvent()}", autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onBasicInfoUpdateEvent(String message) throws Exception {
    log.info("Consume event {} with message {} ",
        kafkaTopicProperties.getBulkBasicInfoUpdateEvent(), message);
    try {
      BulkUpdateEventModel bulkUpdateEventModel =
          objectMapper.readValue(message, BulkUpdateEventModel.class);
      bulkBasicInfoUpdateService.processBulkBasicInfoUpdate(bulkUpdateEventModel);
    } catch (Exception e) {
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
          "Error while listening and processing "
              + kafkaTopicProperties.getBulkBasicInfoUpdateEvent() + "message : " + message
              + ", error is : " + e.getMessage(), e);
    }
  }

  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkBasicInfoUpdatePriority1Event()}",
                 autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void bulkBasicInfoUpdatePriority1Event(String message) throws Exception {
    log.info("Consume event {} with message {} ",
        kafkaTopicProperties.getBulkBasicInfoUpdatePriority1Event(), message);
    try {
      BulkUpdateEventModel bulkUpdateEventModel =
          objectMapper.readValue(message, BulkUpdateEventModel.class);
      bulkBasicInfoUpdateService.processBulkBasicInfoUpdate(bulkUpdateEventModel);
    } catch (Exception e) {
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
          "Error while listening and processing "
              + kafkaTopicProperties.getBulkBasicInfoUpdatePriority1Event() + "message : " + message
              + ", error is : " + e.getMessage(), e);
    }
  }

  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkBasicInfoUpdatePriority2Event() }",
                 autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void bulkBasicInfoUpdatePriority2Event(String message) throws Exception {
    log.info("Consume event {} with message {} ",
        kafkaTopicProperties.getBulkBasicInfoUpdatePriority2Event(), message);
    try {
      BulkUpdateEventModel bulkUpdateEventModel =
          objectMapper.readValue(message, BulkUpdateEventModel.class);
      bulkBasicInfoUpdateService.processBulkBasicInfoUpdate(bulkUpdateEventModel);
    } catch (Exception e) {
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
          "Error while listening and processing "
              + kafkaTopicProperties.getBulkBasicInfoUpdatePriority2Event() + "message : " + message
              + ", error is : " + e.getMessage(), e);
    }
  }
} 