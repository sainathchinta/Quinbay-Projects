package com.gdn.x.mta.distributiontask.inbound.impl;

import java.util.UUID;

import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.BrandApprovedOrRejectedDomainEventModel;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class BrandApprovedOrRejectedSubscriberEventListener {

  private static final String STORE_ID = "10001";

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductWrapperService productWrapperService;

  @KafkaListener(topics = DomainEventName.BRAND_APPROVED_OR_REJECTED_EVENT, autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    try {
      BrandApprovedOrRejectedDomainEventModel brandApprovedOrRejectedDomainEventModel =
          objectMapper.readValue(message, BrandApprovedOrRejectedDomainEventModel.class);
      setMandatoryParameters();
      log.info("Consume brand approve/reject event message : {}", brandApprovedOrRejectedDomainEventModel);
      productWrapperService
          .updateBrandApprovalStatusAndUpdateSolr(brandApprovedOrRejectedDomainEventModel);
    } catch (Exception e) {
      log.info("Error while consuming brand approve/reject event message : {} ",
          message, e);
    }
  }

  private void setMandatoryParameters() {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, UUID.randomUUID().toString());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constants.DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constants.DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constants.DEFAULT_CHANNEL_ID);
  }
}
