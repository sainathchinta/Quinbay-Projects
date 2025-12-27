package com.gdn.mta.bulk.listener.kafka;

import java.io.IOException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.service.DormantSellerService;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.ResignSellerDomainEvent;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ResignSellerEventListener {

  @Autowired
  private DormantSellerService dormantSellerService;

  @Autowired
  private ObjectMapper objectMapper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.RESIGN_SELLER_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws IOException {
    log.info("Consume event {} with message {} ", DomainEventName.RESIGN_SELLER_EVENT, message);
    ResignSellerDomainEvent resignSellerDomainEvent = objectMapper.readValue(message, ResignSellerDomainEvent.class);
    log.info("Received event : {} for topic : {} ", resignSellerDomainEvent,
      DomainEventName.RESIGN_SELLER_EVENT);
    try {
      this.dormantSellerService.processResignSellerEvent(resignSellerDomainEvent.getStoreId(),
        resignSellerDomainEvent.getBusinessPartnerCode());
    } catch (Exception e) {
      log.error("Error processing event : {}, error - ", resignSellerDomainEvent, e);
    }
  }
}
