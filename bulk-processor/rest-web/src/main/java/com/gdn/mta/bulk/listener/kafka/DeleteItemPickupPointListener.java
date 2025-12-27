package com.gdn.mta.bulk.listener.kafka;

import static com.google.common.base.Preconditions.checkArgument;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.BulkProcessValidationErrorMessages;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.service.PickupPointDeleteService;
import com.gdn.partners.bulk.util.Constant;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class DeleteItemPickupPointListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private PickupPointDeleteService pickupPointDeleteService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkDeleteItemPickupPoint()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Received event for topic : {} with message : {}", kafkaTopicProperties.getBulkDeleteItemPickupPoint(), message);
    try {
      BulkUpdateEventModel bulkUpdateEventModel = this.objectMapper.readValue(message, BulkUpdateEventModel.class);
      setMandatoryParameters(bulkUpdateEventModel.getStoreId());
      checkArgument(StringUtils.isNotBlank(bulkUpdateEventModel.getBusinessPartnerCode()),
          BulkProcessValidationErrorMessages.BUSINESS_PARTNER_CODE_CANNOT_BE_BLANK);
      checkArgument(StringUtils.isNotBlank(bulkUpdateEventModel.getBulkProcessCode()),
          BulkProcessValidationErrorMessages.BULK_PROCESS_CODE_MUST_NOT_BE_BLANK);
      checkArgument(StringUtils.isNotBlank(bulkUpdateEventModel.getStoreId()),
          BulkProcessValidationErrorMessages.STORE_ID_CANNOT_BE_BLANK);
      pickupPointDeleteService.processDeleteItemPickupPointEvent(bulkUpdateEventModel);
    } catch (Exception e) {
      log.error("Error while process bulk delete item pickup event with payload : {}, error - ", message, e);
    }
  }

  private void setMandatoryParameters(String storeId) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, Constant.CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, Constant.USER_NAME);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constant.CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constant.CHANNEL_ID);
  }
}