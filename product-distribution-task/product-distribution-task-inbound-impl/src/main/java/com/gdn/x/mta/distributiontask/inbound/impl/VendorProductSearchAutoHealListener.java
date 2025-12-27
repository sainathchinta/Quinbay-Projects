package com.gdn.x.mta.distributiontask.inbound.impl;

import java.util.UUID;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.VendorSearchAutoHealEventModel;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@ConditionalOnProperty(value = "vendor.product.search.auto.heal.listener.enabled", havingValue = "true")
public class VendorProductSearchAutoHealListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductWrapperService productWrapperService;

  @KafkaListener(topics = DomainEventName.VENDOR_SEARCH_AUTO_HEAL_EVENT_NAME, autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    try {
      setMandatoryParameters();
      VendorSearchAutoHealEventModel vendorSearchAutoHealEventModel =
          objectMapper.readValue(message, VendorSearchAutoHealEventModel.class);
      productWrapperService.processVendorSearchAutoHealProduct(vendorSearchAutoHealEventModel.getStoreId(),
          vendorSearchAutoHealEventModel.getProductCode());
    } catch (Exception e) {
      log.error("Error while processing event : {}, payload : {} ", DomainEventName.VENDOR_SEARCH_AUTO_HEAL_EVENT_NAME,
          message, e);
    }
  }

  private void setMandatoryParameters() {
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, UUID.randomUUID().toString());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constants.DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constants.DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constants.DEFAULT_CHANNEL_ID);
  }
}
