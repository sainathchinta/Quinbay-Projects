package com.gdn.partners.pbp.service.listener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.CategoryProductSkuMappingRequest;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.product.service.RecategorizationService;
import com.newrelic.api.agent.Trace;

/**
 * Created by hardikbohra on 01/06/18.
 */
@Service
public class SaveCategoryProductSkuMappingListener {

  private static final Logger LOGGER = LoggerFactory.getLogger(SaveCategoryProductSkuMappingListener.class);

  @Autowired
  private RecategorizationService recategorizationService;

  @Autowired
  private ObjectMapper objectMapper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.CATEGORY_TO_PRODUCT_SKU_MAPPING_SAVE_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    CategoryProductSkuMappingRequest saveRequest =
        objectMapper.readValue(message, CategoryProductSkuMappingRequest.class);
    try {
      recategorizationService.saveCategoryToProductSkuMapping(saveRequest);
    } catch (Exception ex) {
      LOGGER.error("error while listening '{}', error is : ", DomainEventName
          .CATEGORY_TO_BUSINESS_PARTNER_MAPPING_SAVE_EVENT, ex);
    }
  }
}