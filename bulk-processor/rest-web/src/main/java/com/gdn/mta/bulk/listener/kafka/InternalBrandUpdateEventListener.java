package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.InternalBrandUpdateEventModel;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class InternalBrandUpdateEventListener {

  @Autowired
  private InternalProcessServiceWrapper internalProcessServiceWrapper;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @KafkaListener(topics = "#{kafkaTopicProperties.getInternalBrandUpdateEvent()}", autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("onDomainEventConsumed : {} message : {} ",
        kafkaTopicProperties.getInternalBrandUpdateEvent(), message);
    try {
      InternalBrandUpdateEventModel internalBrandUpdateEventModel =
          objectMapper.readValue(message, InternalBrandUpdateEventModel.class);
      internalProcessServiceWrapper.processInternalBrandUpdateEvent(internalBrandUpdateEventModel);
    } catch (Exception e) {
      log.info("error caught on onDomainEventConsumed. event : {} message : {} ",
          kafkaTopicProperties.getInternalBrandUpdateEvent(), message, e);
    }
  }
}
