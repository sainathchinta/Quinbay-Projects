package com.gdn.partners.pbp.service.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.domain.event.config.DomainEventName;
import com.gdn.mta.domain.event.modal.ProductFbbMigrationEventModel;
import com.gdn.mta.product.service.BackFillFbbFlagService;
import com.gdn.mta.product.service.ErrorMessages;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Slf4j
@Service
@ConditionalOnProperty(value = "fbb.pp.code.change.event.listener.enabled", havingValue = "true")
public class FbbPickupPointMigrationListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private BackFillFbbFlagService backFillFbbFlagService;

  @Trace(dispatcher = true)
  @KafkaListener(topics = DomainEventName.FBB_PICKUP_POINT_MIGRATION, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Received event to migrate fbb pickup point for payload : {} ", message);
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
      setMandatoryParameters();
      backFillFbbFlagService.updateFbbPickupPoint(backFillFbbFlagRequest);
    } catch (Exception e) {
      log.error("Error on process of back fill fbb flag for payload : {}, error - ", message, e);
    }
  }
  private void setMandatoryParameters() {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, Constants.DEFAULT_STORE_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, Constants.DEFAULT_REQUEST_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constants.DEFAULT_USERNAME);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constants.DEFAULT_CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constants.DEFAULT_CHANNEL_ID);
  }
}
