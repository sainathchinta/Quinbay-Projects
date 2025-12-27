package com.gdn.partners.pbp.service.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.ProductFbbMigrationEventModel;
import com.gdn.mta.product.service.BackFillFbbFlagService;
import com.gdn.mta.product.service.ErrorMessages;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class BackFillFbbFlagListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private BackFillFbbFlagService backFillFbbFlagService;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.INACTIVE_PRODUCT_BACK_FILL_FBB_FLAG, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Received event to back fill fbb flag for payload : {} ", message);
    try {
      ProductFbbMigrationEventModel backFillFbbFlagRequest =
        this.objectMapper.readValue(message, ProductFbbMigrationEventModel.class);
      GdnPreconditions.checkArgument(StringUtils.isNotEmpty(backFillFbbFlagRequest.getIdentifier()),
        ErrorMessages.ID_LIST_MUST_NOT_BE_NULL_OR_EMPTY);
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
