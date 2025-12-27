package com.gdn.mta.bulk.listener.kafka;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.InternalBulkUploadDataDomainEventModel;
import com.gdn.mta.bulk.service.MasterDataBulkUpdateService;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class InternalBulkUploadEventListener {

  @Autowired
  private MasterDataBulkUpdateService masterDataBulkUpdateService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getInternalBulkUploadDetails()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("onDomainEventConsumed : {}, with message : {} ", kafkaTopicProperties.getInternalBulkUploadDetails(),
        message);
    InternalBulkUploadDataDomainEventModel internalBulkUploadData =
        objectMapper.readValue(message, InternalBulkUploadDataDomainEventModel.class);
    try {
      masterDataBulkUpdateService.processInternalBulkUploadEvent(internalBulkUploadData.getStoreId(),
          internalBulkUploadData.getUpdatedBy(), internalBulkUploadData.getProcessType(),
          internalBulkUploadData.getInternalProcessDataRequestId());
    } catch (Exception e) {
      log.info("error caught on onDomainEventConsumed. event : {} ",
          kafkaTopicProperties.getInternalBulkUploadDetails(), e);
    }
  }
}
