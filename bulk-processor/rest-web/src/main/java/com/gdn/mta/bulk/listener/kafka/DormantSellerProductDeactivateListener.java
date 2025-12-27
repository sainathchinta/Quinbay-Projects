package com.gdn.mta.bulk.listener.kafka;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;
import com.gdn.mta.bulk.SellerProcessType;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.DormantSellerProductDeactivation;
import com.gdn.mta.bulk.service.DormantSellerService;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class DormantSellerProductDeactivateListener {

  @Autowired
  private DormantSellerService dormantSellerService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getDormantSellerProductDeactivateEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("onDomainEventConsumed : Dormant seller product deactivation event : {} consumed with message : {} ",
        kafkaTopicProperties.getDormantSellerProductDeactivateEvent(), message);
    DormantSellerProductDeactivation dormantSellerProductDeactivation =
        objectMapper.readValue(message, DormantSellerProductDeactivation.class);
    try {
      dormantSellerService.processSellerDeactivate(dormantSellerProductDeactivation.getBusinessPartnerCode(),
          SellerProcessType.DORMANT.name());
    } catch (Exception e) {
      log.error(
          "onDomainEventConsumed : Error occured on Dormant seller product deactivation Event consumed : {} error message : {}",
          dormantSellerProductDeactivation, e.getMessage(), e);
    }
  }
}
