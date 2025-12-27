package com.gdn.partners.product.analytics.service.impl.domainEvent;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import com.gdn.partners.product.analytics.service.ImageDeleteService;
import lombok.extern.slf4j.Slf4j;
import model.ImageDeleteEventModel;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class PermanentImageDeleteEventListener {

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ImageDeleteService imageDeleteService;

  @KafkaListener(topics = "#{kafkaTopicProperties.getProductImageDeleteEventName()}",
                 autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Listened message from topic : {} , message : {} ",
      kafkaTopicProperties.getProductImageDeleteEventName(), message);
    try {
      ImageDeleteEventModel imageDeleteEventModel =
        objectMapper.readValue(message, ImageDeleteEventModel.class);
      if (StringUtils.isNotBlank(imageDeleteEventModel.getProductCode())) {
        imageDeleteService.updateImageCollectionForProductDelete(
          imageDeleteEventModel.getProductCode());
      }
    } catch (Exception e) {
      log.error("Error occurred while listening to event , message = {} error - ", message, e);
    }
  }
}
