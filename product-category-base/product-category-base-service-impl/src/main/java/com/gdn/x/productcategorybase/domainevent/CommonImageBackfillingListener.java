package com.gdn.x.productcategorybase.domainevent;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.CommonImageBackfillingEventModel;
import com.gdn.x.productcategorybase.service.ProductServiceWrapper;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class CommonImageBackfillingListener {

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = DomainEventName.PRODUCT_AND_ITEM_COMMON_IMAGE_BACKFILLING, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    try {
      log.info("Message consumed on topic: {}, payload: {} ", DomainEventName.PRODUCT_AND_ITEM_COMMON_IMAGE_BACKFILLING,
          message);
      CommonImageBackfillingEventModel commonImageBackfillingEventModel =
          objectMapper.readValue(message, CommonImageBackfillingEventModel.class);
      productServiceWrapper.backfillCommonImageFlagInProductAndItemImages(commonImageBackfillingEventModel.getStoreId(),
          commonImageBackfillingEventModel.getProductCode(), commonImageBackfillingEventModel.getMigrationType());
    } catch (Exception e) {
      log.error("Error while processing event: {}, payload: {} ",
          DomainEventName.PRODUCT_AND_ITEM_COMMON_IMAGE_BACKFILLING, message, e);
    }
  }

}
