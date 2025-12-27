package com.gdn.mta.bulk.listener.kafka;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.service.UnifiedBulkDownloadService;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.BrandDomainEventModel;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class UnifiedBulkDownloadBrandCreateListener {

  @Autowired
  private UnifiedBulkDownloadService unifiedBulkDownloadService;

  @Autowired
  private ObjectMapper objectMapper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.BRAND_CREATED, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("onDomainEventConsumed : Brand created event consumed {} ", message);
    BrandDomainEventModel brandDomainEventModel = objectMapper.readValue(message, BrandDomainEventModel.class);
    try {
      unifiedBulkDownloadService.updateBrandFlag(brandDomainEventModel.getBusinessPartnerCode());
    } catch (Exception e) {
      log.error(
          "onDomainEventConsumed : Error occurred to update brand flag on consuming BRAND_CREATED with request : {}",
          brandDomainEventModel, e);
    }
  }
}
