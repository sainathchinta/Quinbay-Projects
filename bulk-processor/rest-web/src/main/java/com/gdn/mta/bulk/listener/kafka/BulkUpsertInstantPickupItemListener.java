package com.gdn.mta.bulk.listener.kafka;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.service.BulkUpsertService;
import com.gdn.partners.bulk.util.Constant;
import com.newrelic.api.agent.Trace;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class BulkUpsertInstantPickupItemListener {

  @Autowired
  private BulkUpsertService bulkUpsertService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkUpsertInstantPickupItemEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Received event for topic : {} with message : {}",
        kafkaTopicProperties.getBulkUpsertInstantPickupItemEvent(), message);
    try {
      BulkUpdateEventModel bulkUpdateEventModel = this.objectMapper.readValue(message, BulkUpdateEventModel.class);
      setMandatoryParameters(bulkUpdateEventModel.getStoreId());
      bulkUpsertService.processEvent(bulkUpdateEventModel);
    } catch (Exception e) {
      log.error("Error while process bulk upsert instant pickup event with payload : {}, error - ", message, e);
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
          "Error while listening and processing " + "BULK_UPSERT_INSTANT_PICKUP_ITEM_EVENT bulkUpdateEventModel : "
              + message + ", error is : " + e.getMessage(), e);
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
