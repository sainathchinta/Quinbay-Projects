package com.gdn.mta.bulk.listener.kafka;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkBasicInfoRequest;
import com.gdn.mta.bulk.service.BulkBasicInfoUpdateService;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@ConditionalOnProperty(value = "bulk.basic.info.update.feature.switch", havingValue = "true")
public class BulkUploadBasicInfoListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private BulkBasicInfoUpdateService bulkBasicInfoUpdateService;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkBasicInfoUploadPriority1Event()}",
      autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Consume event {} with message {} ", kafkaTopicProperties.getBulkBasicInfoUploadPriority1Event(), message);
    BulkBasicInfoRequest bulkBasicInfoRequest = objectMapper.readValue(message, BulkBasicInfoRequest.class);
    String userName = StringUtils.EMPTY;
    try {
      userName = bulkBasicInfoRequest.getUpdatedBy();
      bulkBasicInfoUpdateService.processBulkUpdate(bulkBasicInfoRequest);
      log.info("Successfully processed bulk update for priority 1. userName: {}, bulkProcessCode: {}", userName,
          bulkBasicInfoRequest.getBulkProcessCode());
    } catch (Exception e) {
      log.error("Failed to process bulk update for priority 1. userName: {}, bulkProcessCode: {}", userName,
          bulkBasicInfoRequest.getBulkProcessCode(), e);
    }
  }

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkBasicInfoUploadPriority2Event()}",
      autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedForPriority1(String message) throws Exception {
    log.info("Consume event {} with message {} ", kafkaTopicProperties.getBulkBasicInfoUploadPriority2Event(), message);
    BulkBasicInfoRequest bulkBasicInfoRequest = objectMapper.readValue(message, BulkBasicInfoRequest.class);
    String userName = StringUtils.EMPTY;
    try {
      userName = bulkBasicInfoRequest.getUpdatedBy();
      bulkBasicInfoUpdateService.processBulkUpdate(bulkBasicInfoRequest);
      log.info("Successfully processed bulk update for priority 2. userName: {}, bulkProcessCode: {}", userName,
          bulkBasicInfoRequest.getBulkProcessCode());
    } catch (Exception e) {
      log.error("Failed to process bulk update for priority 2. userName: {}, bulkProcessCode: {}", userName,
          bulkBasicInfoRequest.getBulkProcessCode(), e);
    }
  }
}
