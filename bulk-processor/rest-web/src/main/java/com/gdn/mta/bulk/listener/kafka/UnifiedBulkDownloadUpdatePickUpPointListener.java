package com.gdn.mta.bulk.listener.kafka;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.service.UnifiedBulkDownloadService;
import com.gdn.x.businesspartner.domain.event.config.PickupPointDomainEventName;
import com.gdn.x.businesspartner.domain.event.model.PickupPointChange;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class UnifiedBulkDownloadUpdatePickUpPointListener {

  @Autowired
  private UnifiedBulkDownloadService unifiedBulkDownloadService;

  @Autowired
  private ObjectMapper objectMapper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = PickupPointDomainEventName.UPDATE_ALL, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
      public void onDomainEventConsumed(String message) throws Exception {
    log.info("onDomainEventConsumed : update pickup point event consumed {} ", message);
    PickupPointChange pickupPointChange = objectMapper.readValue(message, PickupPointChange.class);
    try {
      unifiedBulkDownloadService.updatePickUpPointFlag(pickupPointChange.getBusinessPartnerCode());
    } catch (Exception e) {
      log.error("onDomainEventConsumed : Error occured on update pickup point Domain Event consumed {} ",
          message, e);
    }
  }
}
