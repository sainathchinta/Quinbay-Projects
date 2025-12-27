package com.gdn.x.mta.distributiontask.inbound.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.domain.event.model.PDTProductsPermanentDeleteEventModel;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@Slf4j
public class PDTProductsPermanentDeleteEventListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private ProductWrapperService productWrapperService;

  @Autowired
  private KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;

  @KafkaListener(topics = "#{kafkaTopicPropertiesConsumer.getProductAnalyticsPermanentDeleteEvent"
      + "()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    try {
      setMandatoryParameters();
      PDTProductsPermanentDeleteEventModel pdtProductsPermanentDeleteEventModel =
        objectMapper.readValue(message, PDTProductsPermanentDeleteEventModel.class);
      log.info("Received message for event {} - message: {}",
        kafkaTopicPropertiesConsumer.getProductAnalyticsPermanentDeleteEvent(),
        pdtProductsPermanentDeleteEventModel);
      if (StringUtils.isNotBlank(pdtProductsPermanentDeleteEventModel.getProductCode())) {
        productWrapperService.processProductsPermanentDelete(
          pdtProductsPermanentDeleteEventModel.getProductCode(),
          pdtProductsPermanentDeleteEventModel.getSellerCode());
      }
    } catch (Exception e) {
      log.error("Error when consume event : {}, message : {} ",
        kafkaTopicPropertiesConsumer.getProductAnalyticsPermanentDeleteEvent(), message, e);
    }
  }

  private void setMandatoryParameters() {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, Constants.DEFAULT_STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
      UUID.randomUUID().toString());
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constants.DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constants.DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
      Constants.DEFAULT_CHANNEL_ID);
  }
}
