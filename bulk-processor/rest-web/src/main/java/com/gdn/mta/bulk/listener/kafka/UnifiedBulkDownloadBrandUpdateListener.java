package com.gdn.mta.bulk.listener.kafka;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.service.UnifiedBulkDownloadService;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.BrandApprovedOrRejectedDomainEventModel;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class UnifiedBulkDownloadBrandUpdateListener {

  @Autowired
  private UnifiedBulkDownloadService unifiedBulkDownloadService;

  @Autowired
  private ObjectMapper objectMapper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.BRAND_APPROVED_OR_REJECTED_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("onDomainEventConsumed : Brand approved or rejected event consumed {} ", message);
    BrandApprovedOrRejectedDomainEventModel brandApprovedOrRejectedDomainEventModel =
        objectMapper.readValue(message, BrandApprovedOrRejectedDomainEventModel.class);
    try {
      unifiedBulkDownloadService.updateBrandFlag(brandApprovedOrRejectedDomainEventModel.getBusinessPartnerCode());
    } catch (Exception e) {
      log.error(
          "onDomainEventConsumed : Error occurred to update brand flag on consuming BRAND_APPROVED_OR_REJECTED_EVENT with request : {} ",
          brandApprovedOrRejectedDomainEventModel, e);
    }
  }
}
