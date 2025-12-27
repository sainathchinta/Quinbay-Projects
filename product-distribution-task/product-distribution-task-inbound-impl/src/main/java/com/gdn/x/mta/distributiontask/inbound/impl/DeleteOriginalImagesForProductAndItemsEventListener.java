package com.gdn.x.mta.distributiontask.inbound.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.model.Product;
import com.gdn.x.mta.distributiontask.service.api.ProductService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Slf4j
@Service
public class DeleteOriginalImagesForProductAndItemsEventListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductService productService;

  @KafkaListener(topics = DomainEventName.DELETE_ORIGINAL_IMAGES_FOR_PRODUCT_AND_ITEMS_EVENT,
                 autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void deleteOriginalImages(String message) {
    log.info("Event consumed for topic : {} , message : {} ",
        DomainEventName.DELETE_ORIGINAL_IMAGES_FOR_PRODUCT_AND_ITEMS_EVENT, message);
    try {
      Product product = objectMapper.readValue(message, Product.class);
      productService.deleteOriginalImagesForProductAndItems(product);
    } catch (Exception e) {
      log.error("Exception caught while processing event. topic : {} , message : {} , error - ",
          DomainEventName.DELETE_ORIGINAL_IMAGES_FOR_PRODUCT_AND_ITEMS_EVENT, message, e);
    }
  }
}
