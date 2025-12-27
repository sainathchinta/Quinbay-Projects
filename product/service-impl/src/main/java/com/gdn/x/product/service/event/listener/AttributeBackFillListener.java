package com.gdn.x.product.service.event.listener;

import com.gdn.x.product.service.api.MasterDataCacheService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.domain.event.model.XProdAttributeMigrationEventModel;
import com.gdn.x.product.outbound.api.ProductCategoryBaseOutbound;
import com.gdn.x.product.service.api.ProductServiceV2;
import com.gdn.x.product.service.properties.KafkaTopicProperties;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.attribute.back.fill.listener.enabled", havingValue = "true")
public class AttributeBackFillListener {

  public static final String SUCCESS = "SUCCESS";
  public static final String FAILED = "FAILED";

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private ProductServiceV2 productServiceV2;

  @Autowired
  private ProductCategoryBaseOutbound productCategoryBaseOutbound;

  @Autowired
  private MasterDataCacheService masterDataCacheService;


  @KafkaListener(topics = "#{kafkaTopicProperties.getXProductAttributeMigrationEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws JsonProcessingException {
    log.info("Received x-product attribute migration event: {} with message: {} ",
        kafkaTopicProperties.getXProductAttributeMigrationEvent(), message);
    XProdAttributeMigrationEventModel xProdAttributeMigrationEventModel =
        objectMapper.readValue(message, XProdAttributeMigrationEventModel.class);
    try {
      masterDataCacheService.evictMasterDataProduct(
        xProdAttributeMigrationEventModel.getProductCode());
      if (xProdAttributeMigrationEventModel.isSkuValue()) {
        productServiceV2.backFillSpecialAttributesInProduct(
          xProdAttributeMigrationEventModel.getProductCode(),
          xProdAttributeMigrationEventModel.getAttributeCode(),
          xProdAttributeMigrationEventModel.getAttributeName(),
          xProdAttributeMigrationEventModel.getAttributeValue());
      }
      productCategoryBaseOutbound.updateStatusInPCBForBackFillAttributes(
          xProdAttributeMigrationEventModel.getProductCode(), SUCCESS, null);
    } catch (Exception exp) {
      log.error("Exception caught while processing event {} ",
          kafkaTopicProperties.getXProductAttributeMigrationEvent(), exp);
      productCategoryBaseOutbound.updateStatusInPCBForBackFillAttributes(
          xProdAttributeMigrationEventModel.getProductCode(), FAILED, exp.getMessage());
    }
  }
}
