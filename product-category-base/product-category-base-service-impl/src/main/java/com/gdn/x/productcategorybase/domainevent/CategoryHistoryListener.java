package com.gdn.x.productcategorybase.domainevent;

import java.util.Objects;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.util.GdnMandatoryParameterUtil;
import com.gdn.x.productcategorybase.domain.event.model.CategoryHistoryEventModel;
import com.gdn.x.productcategorybase.service.CategoryHistoryService;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class CategoryHistoryListener {

  @Autowired
  private CategoryHistoryService categoryHistoryService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @KafkaListener(topics = "#{kafkaTopicProperties.getCategoryUpdateHistoryEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Message received, from topic {}, message : {}",
      kafkaTopicProperties.getCategoryUpdateHistoryEvent(), message);
    try {
      CategoryHistoryEventModel categoryHistoryEventModel =
        objectMapper.readValue(message, CategoryHistoryEventModel.class);
      if (Objects.nonNull(categoryHistoryEventModel)) {
        MDC.put(GdnMandatoryParameterUtil.USERNAME_KEY_PARAMETER,
          categoryHistoryEventModel.getUserName());
        categoryHistoryService.saveCategoryHistory(categoryHistoryEventModel);
      }
    } catch (Exception ex) {
      log.error("error while listening '{}', error is : ",
        kafkaTopicProperties.getCategoryUpdateHistoryEvent(), ex);
    }
  }
}
