package com.gdn.x.mta.distributiontask.inbound.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import com.gdn.x.productcategorybase.domain.event.model.ImagePathUpdateDomainEventModel;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductAndItemImagePathUpdateEventListener {

  @Autowired
  private ProductService productService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = DomainEventName.PRODUCT_AND_ITEM_IMAGE_PATH_UPDATE_EVENT, autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Event consumed for topic : {} , message : {} ", DomainEventName.PRODUCT_AND_ITEM_IMAGE_PATH_UPDATE_EVENT,
        message);
    try {
      ImagePathUpdateDomainEventModel imagePathUpdateDomainEventModel =
          objectMapper.readValue(message, ImagePathUpdateDomainEventModel.class);
      productService.productAndItemImagePathUpdate(imagePathUpdateDomainEventModel);
    } catch (Exception e) {
      log.error("Exception caught while processing event. topic : {} , message : {} , error - ",
          DomainEventName.PRODUCT_AND_ITEM_IMAGE_PATH_UPDATE_EVENT, message, e);
    }
  }

}
