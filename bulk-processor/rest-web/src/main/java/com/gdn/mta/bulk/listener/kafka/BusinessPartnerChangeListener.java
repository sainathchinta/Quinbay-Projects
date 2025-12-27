package com.gdn.mta.bulk.listener.kafka;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.service.CacheEvictService;
import com.gdn.x.businesspartner.domain.event.model.BusinessPartnerChange;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@ConditionalOnProperty(name = "business.partner.cache.listener.enabled", havingValue = "true")
public class BusinessPartnerChangeListener {

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private CacheEvictService cacheEvictService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = "#{kafkaTopicProperties.getBusinessPartnerChangeEvent()}",
      groupId = "#{ T(java.util.UUID).randomUUID().toString() }", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Consume event {} with message {} ", kafkaTopicProperties.getBusinessPartnerChangeEvent(), message);
    try {
      BusinessPartnerChange businessPartnerChange = objectMapper.readValue(message, BusinessPartnerChange.class);
      cacheEvictService.evictBusinessPartnerCache(businessPartnerChange.getBusinessPartnerCode());
    } catch (Exception e) {
      log.error("Error while consuming the event : {} and the payload : {} ",
          kafkaTopicProperties.getBusinessPartnerChangeEvent(), message, e);
    }
  }
}
