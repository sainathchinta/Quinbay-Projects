package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.CategoryProductSkuMappingRequest;
import com.gdn.x.product.service.api.ProductService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

/**
 * Created by hardikbohra on 31/05/18.
 */
@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.mta.bulk.category.to.product.sku.mapping.listener"
    + ".enabled", havingValue = "true")
public class CategoryToProductSkuMappingRequestListener {

  @Autowired
  private ProductService productService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = ProductDomainEventName.CATEGORY_TO_PRODUCT_SKU_MAPPING_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("CategoryToProductSkuMappingRequestListener consume event with message : {}", message);
    try {
      CategoryProductSkuMappingRequest recategorizationRequest =
        this.objectMapper.readValue(message, CategoryProductSkuMappingRequest.class);
      productService.processCategoryToProductSkuMapping(recategorizationRequest);
    } catch (Exception ex) {
      log.error("Error while Event listening {} for payload : {}, error is : ",
        ProductDomainEventName.CATEGORY_TO_PRODUCT_SKU_MAPPING_EVENT, message, ex);
    }
  }
}
