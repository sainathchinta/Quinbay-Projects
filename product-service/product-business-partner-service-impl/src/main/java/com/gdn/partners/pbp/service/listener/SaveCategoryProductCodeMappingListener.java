package com.gdn.partners.pbp.service.listener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.CategoryProductCodeMappingRequest;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.product.entity.CategoryProductCodeMapping;
import com.gdn.mta.product.service.RecategorizationService;
import com.newrelic.api.agent.Trace;

/**
 * Created by hardikbohra on 01/06/18.
 */
@Service
public class SaveCategoryProductCodeMappingListener {

  private static final Logger LOGGER = LoggerFactory.getLogger(SaveCategoryProductCodeMappingListener.class);

  @Autowired
  private RecategorizationService recategorizationService;

  @Autowired
  private ObjectMapper objectMapper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.CATEGORY_TO_PRODUCT_CODE_MAPPING_SAVE_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    CategoryProductCodeMappingRequest saveRequest =
        objectMapper.readValue(message, CategoryProductCodeMappingRequest.class);
    try {
      recategorizationService.saveCategoryToProductCodeMapping(saveRequest);
    } catch (Exception ex) {
      LOGGER.error("error while listening '{}', error is : ", DomainEventName
          .CATEGORY_TO_PRODUCT_CODE_MAPPING_SAVE_EVENT, ex);
    }
  }
}