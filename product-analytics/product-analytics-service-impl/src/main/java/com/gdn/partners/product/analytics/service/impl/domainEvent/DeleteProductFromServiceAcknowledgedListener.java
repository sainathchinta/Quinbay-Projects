package com.gdn.partners.product.analytics.service.impl.domainEvent;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.service.TerminatedSellerDeletionService;
import lombok.extern.slf4j.Slf4j;
import model.DeleteProductResultEventModel;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.io.IOException;

@Service
@Slf4j
public class DeleteProductFromServiceAcknowledgedListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private TerminatedSellerDeletionService terminatedSellerDeletionService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @KafkaListener(topics = "#{kafkaTopicProperties.getPermanentDeleteProductEventName()}",
                 autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Consume event {} with message {} ",
        kafkaTopicProperties.getPermanentDeleteProductEventName(), message);
    try {
      DeleteProductResultEventModel deleteProductResultEventModel =
          objectMapper.readValue(message, DeleteProductResultEventModel.class);
      terminatedSellerDeletionService.updateStatusForParticularService(
          deleteProductResultEventModel.getProductCode(),
          deleteProductResultEventModel.getSellerCode(),
          deleteProductResultEventModel.getService(),
          deleteProductResultEventModel.getResult());
      log.info("Event {} processed successfully {} ",
          kafkaTopicProperties.getPermanentDeleteProductEventName(), message);
    } catch (Exception e) {
      log.error("Error occurred while listening to event , message = {} error - ", message, e);
    }
  }
}
