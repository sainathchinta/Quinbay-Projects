package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.service.api.MasterDataCacheService;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.CategoryDomainEventModel;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.x.productcategorybase.category.publish.listener.enabled",
                       havingValue = "true")
public class CategoryChangeEventListener {

  @Autowired
  private MasterDataCacheService cacheService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = DomainEventName.CATEGORY_PUBLISH, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("#categoryChangeEvent-listened with payload: {}", message);
    try {
      CategoryDomainEventModel categoryDomainEventModel = this.objectMapper.readValue(message,
        CategoryDomainEventModel.class);
      if (CollectionUtils.isNotEmpty(categoryDomainEventModel.getAttributeCodes())) {
        for (String attributeCode : categoryDomainEventModel.getAttributeCodes()) {
          cacheService.evictCategoryCodesByAttributeCodeCache(Constants.DEFAULT_STORE_ID,
              attributeCode);
        }
      }
      cacheService.evictAllCategoryTrees();
    } catch (Exception e) {
      log.error("Error on onDomainEventConsumed with payload : {}, error - ", message, e);
    }
  }
}
