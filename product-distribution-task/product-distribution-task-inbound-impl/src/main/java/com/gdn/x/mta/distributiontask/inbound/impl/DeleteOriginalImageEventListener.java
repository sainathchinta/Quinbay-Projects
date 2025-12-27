package com.gdn.x.mta.distributiontask.inbound.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.domain.event.model.DeleteOriginalImageEventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.service.api.FileStorageService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@RequiredArgsConstructor
public class DeleteOriginalImageEventListener {

  private final ObjectMapper objectMapper;
  private final KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;
  private final FileStorageService fileStorageService;

  @KafkaListener(topics = "#{kafkaTopicPropertiesConsumer.getDeleteOriginalImageEvent()}",
                 autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Consumed event : {}, message : {} ",
        kafkaTopicPropertiesConsumer.getDeleteOriginalImageEvent(), message);
    try {
      DeleteOriginalImageEventModel deleteOriginalImageEventModel =
          objectMapper.readValue(message, DeleteOriginalImageEventModel.class);
      fileStorageService.deleteOriginalImages(deleteOriginalImageEventModel.getLocationPath());
    } catch (Exception e) {
      log.error("Error while processing event : {} , message : {} ",
          kafkaTopicPropertiesConsumer.getDeleteOriginalImageEvent(), message, e);
    }
  }
}