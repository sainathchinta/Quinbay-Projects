package com.gdn.mta.bulk.listener.kafka;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.RestrictedKeywordProcessModel;
import com.gdn.mta.bulk.service.RestrictedKeywordService;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class RestrictedKeywordProcessListener {

  @Autowired
  private RestrictedKeywordService restrictedKeywordService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getRestrictedKeywordBulkProcess()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Event consumed for topic : {} , payload : {} ", kafkaTopicProperties.getRestrictedKeywordBulkProcess(),
        message);
    try {
      RestrictedKeywordProcessModel restrictedKeywordProcessModel =
          objectMapper.readValue(message, RestrictedKeywordProcessModel.class);
      restrictedKeywordService.processRestrictedKeywordBulkOperation(restrictedKeywordProcessModel.getStoreId(),
          restrictedKeywordProcessModel);
    } catch (Exception e) {
      log.error("Error in consumption of topic : {} , payload :{} ",
          kafkaTopicProperties.getRestrictedKeywordBulkProcess(), message, e);
    }
  }

}
