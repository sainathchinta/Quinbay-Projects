package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.domain.event.config.ProductDomainEventName;
import com.gdn.x.product.domain.event.model.BackFillFbbFlagRequest;
import com.gdn.x.product.service.api.BackFillFbbFlagService;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
@ConditionalOnProperty(value = "fbb.event.listener.enabled", havingValue = "true")
public class BackFillFbbFlagListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private BackFillFbbFlagService backFillFbbFlagService;

  @KafkaListener(topics = ProductDomainEventName.ACTIVE_PRODUCT_BACK_FILL_FBB_FLAG, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Received event to back fill fbb flag for payload : {} ", message);
    try {
      BackFillFbbFlagRequest backFillFbbFlagRequest =
        this.objectMapper.readValue(message, BackFillFbbFlagRequest.class);
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(backFillFbbFlagRequest.getIdentifier()),
        ErrorMessages.PRODUCT_SKU_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(
        StringUtils.isNotEmpty(backFillFbbFlagRequest.getPickupPointCode()),
        ErrorMessages.PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(backFillFbbFlagRequest.getStoreId()),
        ErrorMessages.STORE_ID_MUST_NOT_BE_BLANK);
        backFillFbbFlagService.backFillFbbFlag(backFillFbbFlagRequest);
    } catch (Exception e) {
      log.error("Error on process of back fill fbb flag for payload : {}, error - ", message, e);
    }
  }
}
