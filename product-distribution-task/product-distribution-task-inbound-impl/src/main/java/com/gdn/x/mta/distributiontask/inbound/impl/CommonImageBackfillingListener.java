package com.gdn.x.mta.distributiontask.inbound.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.mta.distributiontask.domain.event.config.DomainEventName;
import com.gdn.x.mta.distributiontask.domain.event.model.CommonImageBackfillingEventModel;
import com.gdn.x.mta.distributiontask.service.api.ProductWrapperService;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class CommonImageBackfillingListener {

  @Autowired
  private ProductWrapperService productWrapperService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = DomainEventName.PRODUCT_AND_ITEM_COMMON_IMAGE_BACKFILLING, autoStartup
      = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    try {
      log.info("Message consumed on topic: {}, payload: {} ", DomainEventName.PRODUCT_AND_ITEM_COMMON_IMAGE_BACKFILLING,
          message);
      CommonImageBackfillingEventModel commonImageBackfillingEventModel =
          objectMapper.readValue(message, CommonImageBackfillingEventModel.class);
      productWrapperService.backfillCommonImageFlagInProductAndItemImages(commonImageBackfillingEventModel.getStoreId(),
          commonImageBackfillingEventModel.getProductCode());
    } catch (Exception e) {
      log.error("Error while processing event: {}, payload: {} ",
          DomainEventName.PRODUCT_AND_ITEM_COMMON_IMAGE_BACKFILLING, message, e);
    }
  }

}
