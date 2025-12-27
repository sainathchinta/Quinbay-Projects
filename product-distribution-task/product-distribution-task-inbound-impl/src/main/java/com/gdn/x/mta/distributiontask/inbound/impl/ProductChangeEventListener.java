package com.gdn.x.mta.distributiontask.inbound.impl;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.domain.event.model.ProductChange;
import com.gdn.x.mta.distributiontask.inbound.config.KafkaTopicPropertiesConsumer;
import com.gdn.x.mta.distributiontask.model.Constants;
import com.gdn.x.mta.distributiontask.service.api.IprWrapperService;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;
import com.gdn.x.mta.distributiontask.util.GdnMandatoryRequestParameterUtil;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.slf4j.MDC;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import java.util.UUID;

@Service
@Slf4j
@RequiredArgsConstructor
public class ProductChangeEventListener {
  private final KafkaTopicPropertiesConsumer kafkaTopicPropertiesConsumer;
  private final IprWrapperService iprWrapperService;
  private final ProductWrapperService productWrapperService;
  private final ObjectMapper objectMapper;

  @KafkaListener(topics = "#{kafkaTopicPropertiesConsumer.getProductChangeEvent()}", autoStartup = "#{kafkaTopicPropertiesConsumer.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    try {
      log.info("Event consumed {} for product change , message : {} ",
        kafkaTopicPropertiesConsumer.getProductChangeEvent(), message);
      setMandatoryParameters();
      ProductChange productChangeEventModel = objectMapper.readValue(message, ProductChange.class);
      productWrapperService.updateDistributionMappingStatusOnChange(productChangeEventModel);
      iprWrapperService.updateProductOnStateChange(productChangeEventModel);
    } catch (Exception e) {
      log.error("Error while processing event : {}, payload : {} ",
        kafkaTopicPropertiesConsumer.getProductChangeEvent(), message, e);
    }
  }

  private void setMandatoryParameters() {
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER,
      UUID.randomUUID().toString());
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constants.X_PRODUCT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER,
      Constants.DEFAULT_CHANNEL_ID);
  }
}
