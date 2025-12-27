package com.gdn.partners.pbp.service.listener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.CategoryUserMappingRequest;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.product.service.RecategorizationService;
import com.newrelic.api.agent.Trace;

/**
 * Created by hardikbohra on 31/05/18.
 */
@Service
public class SaveCategoryUserMappingListener {

  private static final Logger LOGGER = LoggerFactory.getLogger(SaveCategoryUserMappingListener.class);

  @Autowired
  private RecategorizationService recategorizationService;

  @Autowired
  private ObjectMapper objectMapper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.CATEGORY_TO_BUSINESS_PARTNER_MAPPING_SAVE_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    CategoryUserMappingRequest saveRequest = objectMapper.readValue(message, CategoryUserMappingRequest.class);
    try {
      recategorizationService.saveCategoryToUserMapping(saveRequest);
    } catch (Exception ex) {
      LOGGER.error("error while listening '{}', error is : ", DomainEventName
          .CATEGORY_TO_BUSINESS_PARTNER_MAPPING_SAVE_EVENT, ex);
    }
  }
}
