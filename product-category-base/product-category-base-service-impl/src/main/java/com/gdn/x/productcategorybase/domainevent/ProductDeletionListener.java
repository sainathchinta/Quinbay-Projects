package com.gdn.x.productcategorybase.domainevent;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.ProductCodeDomainEventModel;
import com.gdn.x.productcategorybase.service.ProductDeletionWrapperService;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductDeletionListener {

  @Autowired
  private ProductDeletionWrapperService productDeletionWrapperService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = DomainEventName.DELETE_REJECTED_PRODUCT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    try {
      log.info("Message consumed on topic: {}, payload: {} ", DomainEventName.DELETE_REJECTED_PRODUCT,
          message);
      ProductCodeDomainEventModel productCodeDomainEventModel =
          objectMapper.readValue(message, ProductCodeDomainEventModel.class);
      productDeletionWrapperService.archiveAndDeleteProductData(productCodeDomainEventModel.getStoreId(), productCodeDomainEventModel.getProductCode());
    } catch (Exception e) {
      log.error("Error while processing event: {}, payload: {} ",
          DomainEventName.DELETE_REJECTED_PRODUCT, message, e);
    }
  }
}
