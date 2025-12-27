package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.gdn.x.businesspartner.domain.event.config.BusinessPartnerDomainEventName;
import com.gdn.x.businesspartner.domain.event.model.BusinessPartnerChange;
import com.gdn.x.product.service.api.BusinessPartnerService;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@ConditionalOnProperty(value = "com.gdn.x.businesspartner.profile.update.all.listener.enabled",
                       havingValue = "true")
public class BusinessPartnerUpdateEventListener {

  @Autowired
  private BusinessPartnerService businessPartnerService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = BusinessPartnerDomainEventName.UPDATE_ALL, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("BusinessPartnerUpdateEventListener consume event for payload: {}", message);
    try {
      BusinessPartnerChange businessPartnerChange = this.objectMapper.readValue(message,
        BusinessPartnerChange.class);
      businessPartnerService.upsertBusinessPartner(businessPartnerChange);
    } catch (Exception ex) {
      log.error("Error while Event listening BusinessPartner Update from X-BP, businessPartnerUpdate: {}",
          message, ex);
    }
  }
}
