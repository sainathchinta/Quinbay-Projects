package com.gdn.x.mta.distributiontask.inbound.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.domain.event.model.IPRHistoryEventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.service.api.IprService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;


@Service
@Slf4j
@RequiredArgsConstructor
public class IprHistoryListener {
  private static final String STORE_ID = "10001";

  private final ObjectMapper objectMapper;

  private final KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  private final IprService iprService;

  @KafkaListener(topics = "#{kafkaTopicPropertiesConsumer.getPublishHistoryForIprEvent()}", autoStartup = "#{kafkaTopicPropertiesConsumer.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Consumed event : {}, message : {} ",
      kafkaTopicPropertiesConsumer.getPublishHistoryForIprEvent(), message);
    try {
      IPRHistoryEventModel iprHistoryEventModel =
        objectMapper.readValue(message, IPRHistoryEventModel.class);
      iprService.updateIprHistoryForProduct(STORE_ID, iprHistoryEventModel);
    } catch (Exception e) {
      log.error("Error while processing event : {} , message : {} ",
        kafkaTopicPropertiesConsumer.getPublishHistoryForIprEvent(), message, e);
    }
  }
}
