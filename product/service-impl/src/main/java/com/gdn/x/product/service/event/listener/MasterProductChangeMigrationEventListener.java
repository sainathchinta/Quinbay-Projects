package com.gdn.x.product.service.event.listener;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.client_sdk.shade.org.apache.commons.lang3.StringUtils;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.ProductDomainEventModel;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@ConditionalOnProperty(value = "master.product.change.event.enabled", havingValue = "true")
public class MasterProductChangeMigrationEventListener {

  @Value("${productcategorybase.argument.storeId}")
  private String storeId;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductService productService;


  @KafkaListener(topics = DomainEventName.PRODUCT_MASTER_DATA_MIGRATION, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Consume MasterProductChangeMigration event with topic : {} message : {}",
        DomainEventName.PRODUCT_MASTER_DATA_MIGRATION, message);
    try {
      ProductDomainEventModel productDomainEventModel =
          this.objectMapper.readValue(message, ProductDomainEventModel.class);
      if (StringUtils.isEmpty(productDomainEventModel.getProductCode())) {
        log.error("error while processing event for topic : {} , product code is empty",
            DomainEventName.PRODUCT_MASTER_DATA_MIGRATION);
        return;
      }
      productService.updateProductAndItemDetails(productDomainEventModel, true);
    } catch (Exception ex) {
      log.error(
          "Error while processing event listening when master product migration from PCB , topic : {} payload - {}, error - ",
          DomainEventName.PRODUCT_MASTER_DATA_MIGRATION, message, ex);
    }
  }
}
