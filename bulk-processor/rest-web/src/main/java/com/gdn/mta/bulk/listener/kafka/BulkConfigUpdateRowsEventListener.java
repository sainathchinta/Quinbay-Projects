package com.gdn.mta.bulk.listener.kafka;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkInternalEventModel;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerChannel;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.service.BulkConfigurationUpdateService;
import com.gdn.partners.bulk.util.Constant;
import com.newrelic.api.agent.Trace;

@Service
public class BulkConfigUpdateRowsEventListener {

  @Autowired
  private BulkConfigurationUpdateService bulkConfigurationUpdateService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  private static final Logger LOG = LoggerFactory.getLogger(BulkConfigUpdateRowsEventListener.class);

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkConfigurationUpdateRows()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    LOG.info("Consume event {} with message {} ", kafkaTopicProperties.getBulkConfigurationUpdateRows(), message);
    BulkInternalEventModel bulkUpdateEventModel = objectMapper.readValue(message, BulkInternalEventModel.class);
    LoggerAttributeModel loggerModel = null;
    try {
      loggerModel = new LoggerAttributeModel(this, "onDomainEventConsumed", null, null, null, null, null,
          LoggerChannel.KAFKA.getValue(), kafkaTopicProperties.getBulkConfigurationUpdateRows(),
          String.valueOf(bulkUpdateEventModel));
      LOG.info(LoggerStandard.getLogInfoTemplate(loggerModel));
      setMandatoryParameters(bulkUpdateEventModel.getStoreId());
      bulkConfigurationUpdateService.processConfigUpdateEvent(bulkUpdateEventModel);
    } catch (Exception e) {
      LOG.error(
          LoggerStandard.convertErrorTemplate(this, "onDomainEventConsumed", loggerModel, e, bulkUpdateEventModel), e);
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
          "Error while listening and processing " + "BULK_CONFIGURATION_UPDATE_ROWS bulkUpdateEventModel : "
              + bulkUpdateEventModel.toString() + ", error is : " + e.getMessage(), e);
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
