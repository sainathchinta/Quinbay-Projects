package com.gdn.partners.pbp.service.listener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.ProductApprovalDetailStatusEvent;
import com.gdn.partners.pbp.service.productlevel1.ProductLevel1CollectionService;
import com.newrelic.api.agent.Trace;

/**
 * Created by akshay.bhatt on 13/04/18
 */
@Service
public class ProductApprovalDetailStatusSubscriber {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final Logger LOGGER = LoggerFactory.getLogger(ProductApprovalDetailStatusSubscriber.class);

  @Autowired
  ProductLevel1CollectionService productLevel1CollectionService;

  @Autowired
  private ObjectMapper objectMapper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.PRODUCT_STATUS_UPDATE_TRACKER, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    ProductApprovalDetailStatusEvent productApprovalDetailStateEvent =
        objectMapper.readValue(message, ProductApprovalDetailStatusEvent.class);
    try {
      if (productApprovalDetailStateEvent != null) {
        LOGGER.debug("Retreive message: {}, event = {}", productApprovalDetailStateEvent,
            productApprovalDetailStateEvent.getStatus().getEvent());
        this.productLevel1CollectionService.updateProductStatus(this.DEFAULT_STORE_ID, productApprovalDetailStateEvent);
      }
    } catch (Exception e) {
      LOGGER.error("Error while updating state for {}", productApprovalDetailStateEvent, e);
    }
  }
}
