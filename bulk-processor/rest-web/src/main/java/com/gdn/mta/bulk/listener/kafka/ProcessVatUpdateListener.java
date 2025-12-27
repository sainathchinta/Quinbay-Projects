package com.gdn.mta.bulk.listener.kafka;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.service.BulkUpdateService;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProcessVatUpdateListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private BulkUpdateService productLevel3BulkUpdateServiceBean;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkVatUpdateEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Consume event {} with message {} ", kafkaTopicProperties.getBulkVatUpdateEvent(), message);
    BulkUpdateEventModel bulkUpdateEventModel = objectMapper.readValue(message, BulkUpdateEventModel.class);
    try {
      productLevel3BulkUpdateServiceBean.processEventForVatUpdate(bulkUpdateEventModel);
    } catch (Exception e) {
      log.error("Error while listening and procesing event = {} ,bulkUpdateEventModel = {} ",
          kafkaTopicProperties.getBulkVatUpdateEvent(), bulkUpdateEventModel.toString());
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
          "Error while listening and processing " + "BULK_UPLOAD_CAMPAIGN_ITEM bulkUpdateEventModel : "
              + bulkUpdateEventModel.toString() + ", error is : " + e.getMessage(), e);
    }
  }
}