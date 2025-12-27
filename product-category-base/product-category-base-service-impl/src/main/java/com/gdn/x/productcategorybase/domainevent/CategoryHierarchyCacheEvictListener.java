package com.gdn.x.productcategorybase.domainevent;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.model.CategoryHierarchyCacheEvictEventModel;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import com.gdn.x.productcategorybase.service.impl.ApplicationCacheServiceBean;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class CategoryHierarchyCacheEvictListener {

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private ApplicationCacheServiceBean applicationCacheServiceBean;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = "#{kafkaTopicProperties.getCategoryHierarchyCaffeineCacheEvictEvent()}"
      , groupId = "#{ T(java.util.UUID).randomUUID().toString() }", autoStartup =
                     "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Consume event {} with message {} ",
        kafkaTopicProperties.getCategoryHierarchyCaffeineCacheEvictEvent(), message);
    try {
      CategoryHierarchyCacheEvictEventModel categoryHierarchyCacheEvictEventModel =
          objectMapper.readValue(message, CategoryHierarchyCacheEvictEventModel.class);
      applicationCacheServiceBean.evictCategoryHierarchyByByStoreIdAndCategoryCodeFromCaffeine(
          categoryHierarchyCacheEvictEventModel.getStoreId(),
          categoryHierarchyCacheEvictEventModel.getCategoryCode());
    } catch (Exception e) {
      log.error("Error while consuming the event : {} and the payload : {} ",
          kafkaTopicProperties.getCategoryHierarchyCaffeineCacheEvictEvent(), message, e);
    }
  }
}
