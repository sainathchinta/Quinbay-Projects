package com.gdn.x.product.service.event.listener;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.model.DeleteTerminatedSellerProductEventModel;
import com.gdn.x.product.service.api.DeleteTerminatedSellerProductService;
import com.gdn.x.product.service.properties.KafkaTopicProperties;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@ConditionalOnProperty(value = "com.gdn.product.analytics.permanent.delete.product.xproduct"
    + ".listener.enabled", havingValue = "true")
public class DeleteTerminatedSellerProductEventListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private DeleteTerminatedSellerProductService deleteTerminatedSellerProductService;

  @KafkaListener(topics = "#{kafkaTopicProperties.getDeleteTerminatedSellerProductEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Event consumed for topic : {} , payload : {} ",
        kafkaTopicProperties.getDeleteTerminatedSellerProductEvent(), message);
    try {
      DeleteTerminatedSellerProductEventModel deleteTerminatedSellerProductEventModel =
          objectMapper.readValue(message, DeleteTerminatedSellerProductEventModel.class);
      deleteTerminatedSellerProductService.deleteTerminatedSellerProductData(deleteTerminatedSellerProductEventModel);
    } catch (Exception e) {
      log.error("Error while processing event : {} , message : {} ",
          kafkaTopicProperties.getDeleteTerminatedSellerProductEvent(), message, e);
    }
  }

}
