package com.gdn.mta.bulk.listener.kafka;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.InternalProcessDataDomainEventModel;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class UpdateSalesCategoryEventListener {

  @Autowired
  private InternalProcessServiceWrapper internalProcessServiceWrapper;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getUpdateSalesCategoryDetails()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("onDomainEventConsumed : {}, with message : {} ", kafkaTopicProperties.getUpdateSalesCategoryDetails(),
        message);
    InternalProcessDataDomainEventModel internalProcessDataDomainEventModel =
        objectMapper.readValue(message, InternalProcessDataDomainEventModel.class);
    try {
      internalProcessServiceWrapper.processUpdateSalesCategoryEvent(internalProcessDataDomainEventModel.getStoreId(),
          internalProcessDataDomainEventModel.getProcessType(), internalProcessDataDomainEventModel.getParentCode(),
          internalProcessDataDomainEventModel.getInternalProcessRequestId());
    } catch (Exception e) {
      log.info("error caught on onDomainEventConsumed. event : {} ",
          kafkaTopicProperties.getUpdateSalesCategoryDetails(), e);
    }
  }
}
