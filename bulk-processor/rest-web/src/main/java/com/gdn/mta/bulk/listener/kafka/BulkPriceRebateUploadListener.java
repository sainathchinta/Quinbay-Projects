package com.gdn.mta.bulk.listener.kafka;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.InternalBulkUploadDataDomainEventModel;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class BulkPriceRebateUploadListener {


  @Autowired
  private InternalProcessServiceWrapper internalProcessServiceWrapper;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkPriceRebateUpload()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Consumed Bulk Rebate event {} , message : {} ", kafkaTopicProperties.getBulkPriceRebateUpload(), message);
    try {
      InternalBulkUploadDataDomainEventModel internalProcessDataDomainEventModel =
          objectMapper.readValue(message, InternalBulkUploadDataDomainEventModel.class);
      internalProcessServiceWrapper.processBulkRebateUpload(internalProcessDataDomainEventModel.getStoreId(),
          internalProcessDataDomainEventModel.getInternalProcessDataRequestId());
    } catch (Exception e) {
      log.info("error caught on Bulk Rebate event consumption : {} ", kafkaTopicProperties.getBulkPriceRebateUpload(),
          e);
    }
  }
}
