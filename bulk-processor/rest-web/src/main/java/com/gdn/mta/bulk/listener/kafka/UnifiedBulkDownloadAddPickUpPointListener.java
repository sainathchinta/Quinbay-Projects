package com.gdn.mta.bulk.listener.kafka;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.service.UnifiedBulkDownloadService;
import com.gdn.x.businesspartner.domain.event.config.PickupPointDomainEventName;
import com.gdn.x.businesspartner.domain.event.model.BusinessPartnerAddPickupPoint;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class UnifiedBulkDownloadAddPickUpPointListener {

  @Autowired
  private UnifiedBulkDownloadService unifiedBulkDownloadService;

  @Autowired
  private ObjectMapper objectMapper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = PickupPointDomainEventName.ADD, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("onDomainEventConsumed : add pickup point event consumed {} ", message);
    BusinessPartnerAddPickupPoint businessPartnerAddPickupPoint =
        objectMapper.readValue(message, BusinessPartnerAddPickupPoint.class);
    try {
      unifiedBulkDownloadService.updatePickUpPointFlag(businessPartnerAddPickupPoint.getBusinessPartnerCode());
    } catch (Exception e) {
      log.error("onDomainEventConsumed : Error occured on add pickup point Domain Event consumed {} ",
          businessPartnerAddPickupPoint, e);
    }
  }
}
