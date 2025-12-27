package com.gdn.x.product.service.event.listener;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.service.api.ArchiveAndDeleteRejectedMerchantProductDataService;
import com.gdn.x.productcategorybase.domain.event.model.ProductCodeDomainEventModel;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.x.productcategorybase.delete.rejected.merchant.product"
    + ".listener.enabled", havingValue = "true")
public class DeleteRejectedMerchantProductEventListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ArchiveAndDeleteRejectedMerchantProductDataService archiveAndDeleteRejectedMerchantProductData;

  @KafkaListener(topics = ProductDomainEventName.DELETE_REJECTED_MERCHANT_PRODUCT_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Listened to event : {} , payload : {}", ProductDomainEventName.DELETE_REJECTED_MERCHANT_PRODUCT_EVENT,
        message);
    try {
      ProductCodeDomainEventModel productCodeDomainEventModel =
          this.objectMapper.readValue(message, ProductCodeDomainEventModel.class);
      archiveAndDeleteRejectedMerchantProductData.archiveAndDeleteRejectedMerchantProductData(
          productCodeDomainEventModel);
    } catch (Exception e) {
      log.error("Error on process of this event : {} , payload : {} , error - ",
          ProductDomainEventName.DELETE_REJECTED_MERCHANT_PRODUCT_EVENT, message, e);
    }
  }
}
