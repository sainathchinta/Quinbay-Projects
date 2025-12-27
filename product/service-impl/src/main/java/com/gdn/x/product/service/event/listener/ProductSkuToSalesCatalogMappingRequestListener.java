package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.ProductSkuToSalesCatalogMappingRequest;
import com.gdn.x.product.service.api.ProductService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

/**
 * Created by hardikbohra on 03/06/18.
 */
@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.mta.bulk.product.sku.to.sales.catalog.mapping.listener"
    + ".enabled", havingValue = "true")
public class ProductSkuToSalesCatalogMappingRequestListener {

  @Autowired
  private ProductService productService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = ProductDomainEventName.PRODUCT_SKU_TO_SALES_CATALOG_MAPPING_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("ProductSkuToSalesCatalogMappingRequestListener consume event with message : {}",
      message);
    try {
      ProductSkuToSalesCatalogMappingRequest mappingRequest = this.objectMapper.readValue(message,
        ProductSkuToSalesCatalogMappingRequest.class);
      productService.processProductSkuToSalesCatalogMapping(mappingRequest);
    } catch (Exception ex) {
      log.error("Error while Event listening {} for payload : {}, error is : ",
        ProductDomainEventName.PRODUCT_SKU_TO_SALES_CATALOG_MAPPING_EVENT, message, ex);
    }
  }
}
