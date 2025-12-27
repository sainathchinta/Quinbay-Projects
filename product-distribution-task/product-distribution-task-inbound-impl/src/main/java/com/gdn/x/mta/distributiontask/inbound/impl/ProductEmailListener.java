package com.gdn.x.mta.distributiontask.inbound.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductEmailEventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.service.api.ProductEmailService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@RequiredArgsConstructor
public class ProductEmailListener {

  private final ObjectMapper objectMapper;
  private final KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;
  private final ProductEmailService productEmailService;

  @KafkaListener(topics = "#{kafkaTopicPropertiesConsumer.getAddProductMailEvent()}", autoStartup = "#{kafkaTopicPropertiesConsumer.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Consumed event : {}, message : {} ",
      kafkaTopicPropertiesConsumer.getAddProductMailEvent(), message);
    try {
      ProductEmailEventModel productEmailEventModel =
        objectMapper.readValue(message, ProductEmailEventModel.class);
      productEmailService.addProductToEmailProcess(productEmailEventModel);
    } catch (Exception e) {
      log.error("Error while processing event : {} , message : {} ",
        kafkaTopicPropertiesConsumer.getAddProductMailEvent(), message, e);
    }
  }
}
