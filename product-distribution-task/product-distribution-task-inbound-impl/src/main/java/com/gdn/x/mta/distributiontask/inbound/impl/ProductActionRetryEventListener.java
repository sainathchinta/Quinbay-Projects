package com.gdn.x.mta.distributiontask.inbound.impl;

import java.util.UUID;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.ProductActionRetryEvent;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.service.api.ProductActionRetryService;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductActionRetryEventListener {

  private static final String DEFAULT_USERNAME = "PDT";

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductActionRetryService productActionRetryService;

  @KafkaListener(topics = DomainEventName.ADD_PRODUCT_TO_PDT_RETRY, autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    ProductActionRetryEvent productActionRetryEvent = objectMapper.readValue(message, ProductActionRetryEvent.class);
    log.info("Received message for event {} - message: {}", DomainEventName.ADD_PRODUCT_TO_PDT_RETRY,
        productActionRetryEvent);
    setMandatoryParameters(productActionRetryEvent);
    try {
      productActionRetryService.upsertProductActionRetry(productActionRetryEvent);
    } catch (Exception e) {
      log.error("Error when consume product Auto need Revision Event {}, Error {} ",
          productActionRetryEvent.getProductCode(), e.getMessage(), e);
    }
  }

  private void setMandatoryParameters(ProductActionRetryEvent productActionRetryEvent) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, productActionRetryEvent.getStoreId());
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, UUID.randomUUID().toString());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constants.DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constants.DEFAULT_CHANNEL_ID);
  }
}
