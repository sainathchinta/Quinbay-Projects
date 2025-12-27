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

@Slf4j
@Service
public class BulkInternalPriceUpdateListener {

  @Autowired
  private InternalProcessServiceWrapper internalProcessServiceWrapper;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkInternalPriceUpdateEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("onDomainEventConsumed : {} : {} ",
        kafkaTopicProperties.getBulkInternalPriceUpdateEvent(), message);
    try {
      InternalBulkUploadDataDomainEventModel internalProcessDataDomainEventModel =
          objectMapper.readValue(message, InternalBulkUploadDataDomainEventModel.class);
      internalProcessServiceWrapper.processBulkInternalBulkPriceUpdate(internalProcessDataDomainEventModel, false);
    } catch (Exception e) {
      log.info("error caught on onDomainEventConsumed. event : {} ",
          kafkaTopicProperties.getBulkInternalPriceUpdateEvent(), e);
    }
  }
}
