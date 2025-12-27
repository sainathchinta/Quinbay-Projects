package com.gdn.partners.pbp.service.listener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.ProductSkuToSalesCatalogMappingRequest;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.product.service.RecategorizationService;
import com.newrelic.api.agent.Trace;

/**
 * Created by hardikbohra on 03/06/18.
 */
@Service
public class SaveProductSkuToSalesCatalogMappingListener {

  private static final Logger LOGGER = LoggerFactory.getLogger(SaveProductSkuToSalesCatalogMappingListener.class);

  @Autowired
  private RecategorizationService recategorizationService;

  @Autowired
  private ObjectMapper objectMapper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.PRODUCT_SKU_TO_SALES_CATALOG_SAVE_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    ProductSkuToSalesCatalogMappingRequest saveRequest =
        objectMapper.readValue(message, ProductSkuToSalesCatalogMappingRequest.class);
    try {
      recategorizationService.saveProductSkuToSalesCatalogMapping(saveRequest);
    } catch (Exception ex) {
      LOGGER.error("error while listening '{}', error is : ", DomainEventName
          .PRODUCT_SKU_TO_SALES_CATALOG_SAVE_EVENT, ex);
    }
  }
}