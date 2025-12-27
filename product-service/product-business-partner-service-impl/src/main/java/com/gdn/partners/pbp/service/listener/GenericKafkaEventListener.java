package com.gdn.partners.pbp.service.listener;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.gdn.mta.product.service.config.KafkaTopicProperties;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class GenericKafkaEventListener {

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getGenericKafkaEventName()}", groupId = "#{kafkaTopicProperties.getGenericKafkaGroupId()}", concurrency = "#{kafkaTopicProperties.getGenericKafkaEventConcurrency()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Event listened for topic : {} , and groupId : {} , concurrency : {}",
        kafkaTopicProperties.getGenericKafkaEventName(), kafkaTopicProperties.getGenericKafkaGroupId(),
        kafkaTopicProperties.getGenericKafkaEventConcurrency());
  }
}