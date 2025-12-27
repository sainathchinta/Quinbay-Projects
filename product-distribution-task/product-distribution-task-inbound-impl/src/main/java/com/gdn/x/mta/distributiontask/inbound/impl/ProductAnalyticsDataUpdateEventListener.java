package com.gdn.x.mta.distributiontask.inbound.impl;

import java.util.UUID;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.response.AutoQcConfigChangeDto;
import com.gdn.x.mta.distributiontask.service.api.ProductAutoApprovalService;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductAnalyticsDataUpdateEventListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductAutoApprovalService productAutoApprovalService;

  @KafkaListener(topics = DomainEventName.SELLER_AUTO_QC_DATA_UPDATE, autoStartup =
      "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Event consumed for topic : {}, message : {} ", DomainEventName.SELLER_AUTO_QC_DATA_UPDATE, message);
    try {
      setMandatoryParameters();
      AutoQcConfigChangeDto autoQcConfigChangeDto = objectMapper.readValue(message, AutoQcConfigChangeDto.class);
      productAutoApprovalService.saveAutoQcConfigChanges(autoQcConfigChangeDto);
    } catch (Exception e) {
      log.error("Exception caught while processing event topic : {}, message {} ",
          DomainEventName.SELLER_AUTO_QC_DATA_UPDATE, message, e);
    }
  }

  private void setMandatoryParameters() {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, Constants.DEFAULT_STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, UUID.randomUUID().toString());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constants.DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constants.DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constants.DEFAULT_CHANNEL_ID);
  }

}
