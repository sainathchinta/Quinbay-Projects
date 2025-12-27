package com.gdn.partners.pbp.service.listener;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;

import com.gdn.mta.domain.event.modal.AddDeleteVariantRetryPublishEventModel;

import com.gdn.mta.product.service.config.KafkaTopicProperties;

import com.gdn.mta.product.service.domainevent.publisher.AddDeleteVariantRetryPublishService;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class AddDeleteVariantRetryPublishListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private AddDeleteVariantRetryPublishService addDeleteVariantRetryPublishService;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getAddDeleteVariantRetryPublishEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    AddDeleteVariantRetryPublishEventModel addDeleteVariantRetryPublishEventModel =
        objectMapper.readValue(message, AddDeleteVariantRetryPublishEventModel.class);
    log.info("Received message from Topic: {}, Message: {}",
        kafkaTopicProperties.getAddDeleteVariantRetryPublishEvent(), addDeleteVariantRetryPublishEventModel);
    addDeleteVariantRetryPublishService.processAddDeleteVariantRetryEvents(addDeleteVariantRetryPublishEventModel);
  }
}
