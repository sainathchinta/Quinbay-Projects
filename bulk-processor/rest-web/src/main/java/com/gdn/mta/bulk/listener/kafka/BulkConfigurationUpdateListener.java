package com.gdn.mta.bulk.listener.kafka;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.product.BulkConfigurationUpdateRequest;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerChannel;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.service.BulkConfigurationUpdateService;
import com.gdn.partners.bulk.util.Constant;
import com.newrelic.api.agent.Trace;

@Service
public class BulkConfigurationUpdateListener {

  private static final Logger LOG = LoggerFactory.getLogger(BulkConfigurationUpdateListener.class);

  @Autowired
  private BulkConfigurationUpdateService bulkConfigurationUpdateService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkConfigurationUpdate()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    LOG.info("Consume event {} with message {}", kafkaTopicProperties.getBulkConfigurationUpdate(), message);
    BulkConfigurationUpdateRequest bulkConfigurationUpdateRequest =
        objectMapper.readValue(message, BulkConfigurationUpdateRequest.class);
    LoggerAttributeModel loggerModel = null;
    try {
      loggerModel =
          new LoggerAttributeModel(this, "onDomainEventConsumed", null, bulkConfigurationUpdateRequest.getUpdatedBy(),
              bulkConfigurationUpdateRequest.getRequestId(), bulkConfigurationUpdateRequest.getStoreId(), null,
              LoggerChannel.KAFKA.getValue(), bulkConfigurationUpdateRequest.getFilePath(),
              String.valueOf(bulkConfigurationUpdateRequest));
      LOG.info(LoggerStandard.getLogInfoTemplate(loggerModel));
      setMandatoryParameters(bulkConfigurationUpdateRequest.getStoreId(),
          bulkConfigurationUpdateRequest.getUpdatedBy());
      bulkConfigurationUpdateService.processConfigurationUpdate(bulkConfigurationUpdateRequest);
    } catch (Exception e) {
      LOG.error(LoggerStandard
          .convertErrorTemplate(this, "onDomainEventConsumed", loggerModel, e, bulkConfigurationUpdateRequest), e);
    }
  }

  private void setMandatoryParameters(String storeId, String username) {
    MDC.put(GdnMandatoryRequestParameterUtil.STORE_ID_KEY_PARAMETER, storeId);
    MDC.put(GdnMandatoryRequestParameterUtil.REQUEST_ID_KEY_PARAMETER, Constant.CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER, username);
    MDC.put(GdnMandatoryRequestParameterUtil.CLIENT_ID_KEY_PARAMETER, Constant.CLIENT_ID);
    MDC.put(GdnMandatoryRequestParameterUtil.CHANNEL_ID_KEY_PARAMETER, Constant.CHANNEL_ID);
  }
}