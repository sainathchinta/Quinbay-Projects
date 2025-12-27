package com.gdn.x.product.service.event.listener;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.MasterSkuMappingEventModel;
import com.gdn.x.product.enums.RetryPublishStatus;
import com.gdn.x.product.model.entity.ProductRetryEventPublish;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.api.ProductRetryEventPublishService;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.msku.item.mapping.demapping", havingValue = "true")
public class MasterSkuMappingListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ItemService itemService;

  @Autowired
  private ProductRetryEventPublishService productRetryEventPublishService;

  @Value("${allow.retry.for.master.sku.mapping.failed.skus}")
  private boolean allowRetryForMasterSkuMappingFailedSkus;

  @KafkaListener(topics = ProductDomainEventName.MASTER_SKU_MAPPING_DEMAPPING, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws JsonProcessingException {
    try {
      log.info("Consuming event {} for master sku mapping, message : {} ",
          ProductDomainEventName.MASTER_SKU_MAPPING_DEMAPPING, message);
      MasterSkuMappingEventModel masterSkuMapping =
          this.objectMapper.readValue(message, MasterSkuMappingEventModel.class);
      itemService.updateMasterSku(masterSkuMapping);
    } catch (ApplicationRuntimeException e) {
      log.error("Exception caught while consuming event for master sku mapping, message : {} ", message, e);
    } catch (Exception e) {
      log.error("Exception caught while consuming event for master sku mapping, message : {} ", message, e);
      if  (allowRetryForMasterSkuMappingFailedSkus) {
        ProductRetryEventPublish productRetryEventPublish = ProductRetryEventPublish.builder()
            .clearCache(Boolean.TRUE).retryCount(0).identifier(message)
            .retryPublishStatus(RetryPublishStatus.PENDING)
            .topicName(ProductDomainEventName.MASTER_SKU_MAPPING_DEMAPPING).build();
        log.info("Adding the master sku event for retry : {} ", productRetryEventPublish);
        productRetryEventPublishService.insertToRetryPublish(productRetryEventPublish);
      }
    }

  }

}
