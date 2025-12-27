package com.gdn.mta.bulk.listener.kafka;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.entity.KafkaEventLog;
import com.gdn.mta.bulk.repository.KafkaEventLogRepository;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.CategoryDomainEventModel;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class CategoryDetailListener {

  @Autowired
  private KafkaEventLogRepository kafkaEventLogRepository;

  @Autowired
  private ObjectMapper objectMapper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.CATEGORY_PUBLISH, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("onDomainEventConsumed : Category Detail Event consumed {} ", message);
    CategoryDomainEventModel categoryDomainEventModel = objectMapper.readValue(message, CategoryDomainEventModel.class);
    try {
      KafkaEventLog kafkaEventLog = getKafkaEventLog(categoryDomainEventModel);
      kafkaEventLogRepository.save(kafkaEventLog);
    } catch (Exception e) {
      log.error("onDomainEventConsumed : Error occurred on category Detail Event consumption {}", message, e);
    }
  }

  private KafkaEventLog getKafkaEventLog(CategoryDomainEventModel message) throws Exception {
    ObjectMapper objectMapper = new ObjectMapper();
    KafkaEventLog kafkaEventLog =
        new KafkaEventLog(objectMapper.writeValueAsString(message), DomainEventName.CATEGORY_PUBLISH);
    kafkaEventLog.setStoreId(Constant.STORE_ID);
    return kafkaEventLog;
  }
}