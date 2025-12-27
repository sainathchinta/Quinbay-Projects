package com.gdn.mta.product.service.domainevent;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.product.service.ProductServiceWrapper;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.BrandApprovedOrRejectedDomainEventModel;
import com.newrelic.api.agent.Trace;

@Service
public class BrandApprovedOrRejectedSubscriber {

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private ObjectMapper objectMapper;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.BRAND_APPROVED_OR_REJECTED_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message)
      throws Exception {
    BrandApprovedOrRejectedDomainEventModel brandApprovedOrRejectedDomainEventModel =
        objectMapper.readValue(message, BrandApprovedOrRejectedDomainEventModel.class);
    this.productServiceWrapper.changeBrandCodeAndBrandApprovalStatusInScreeningProducts(
        brandApprovedOrRejectedDomainEventModel.getBrandCode(),
        brandApprovedOrRejectedDomainEventModel.getBrandRequestCode(),
        brandApprovedOrRejectedDomainEventModel.getBrandApprovalStatus(),
        brandApprovedOrRejectedDomainEventModel.getBrandName());
  }
}
