package com.gdn.mta.product.service.domainevent;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.WholesalePriceActivatedOrDeactivatedEvent;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class WholesalePriceActivatedOrDeactivatedSubscriber {

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private ObjectMapper objectMapper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = ProductDomainEventName.WHOLESALE_PRICE_ACTIVATED_DEACTIVATED_BY_SCHEDULER, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    WholesalePriceActivatedOrDeactivatedEvent wholesalePriceActivatedOrDeactivatedEvent =
        objectMapper.readValue(message, WholesalePriceActivatedOrDeactivatedEvent.class);
    log.info("Consume event com.gdn.wholesale.price.scheduler.activated.change with message : {}",
        wholesalePriceActivatedOrDeactivatedEvent);
    this.productServiceWrapper
        .updateProductHistoryOnWholesaleChangesByScheduler(wholesalePriceActivatedOrDeactivatedEvent.getMerchantCode(),
            wholesalePriceActivatedOrDeactivatedEvent.getItemSku(),
            wholesalePriceActivatedOrDeactivatedEvent.isWholesalePriceActivated());
  }
}
