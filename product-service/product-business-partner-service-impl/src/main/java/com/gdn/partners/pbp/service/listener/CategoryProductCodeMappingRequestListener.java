package com.gdn.partners.pbp.service.listener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.BulkDataForRecategorizationRequest;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.product.service.RecategorizationService;
import com.newrelic.api.agent.Trace;

/**
 * Created by hardikbohra on 01/06/18.
 */
@Service
public class CategoryProductCodeMappingRequestListener {

  private static final Logger LOGGER = LoggerFactory.getLogger(CategoryProductCodeMappingRequestListener.class);

  @Autowired
  private RecategorizationService recategorizationService;

  @Autowired
  private ObjectMapper objectMapper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.CATEGORY_TO_PRODUCT_CODE_MAPPING_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    BulkDataForRecategorizationRequest recategorizationRequest =
        objectMapper.readValue(message, BulkDataForRecategorizationRequest.class);
    LOGGER.debug("BusinessPartnerChangeEventListener consume event with message : {}", recategorizationRequest);
    try {
      recategorizationService.processCategoryToProductCodeMapping(recategorizationRequest);
    } catch (Exception ex) {
      LOGGER.error("Error while Event listening {} for recategorizationId : {}, error is : ", DomainEventName
          .CATEGORY_TO_PRODUCT_CODE_MAPPING_EVENT, recategorizationRequest.getRecatId(), ex);
    }
  }
}
