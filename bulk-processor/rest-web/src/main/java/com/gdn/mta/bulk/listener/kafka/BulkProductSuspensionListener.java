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
import com.gdn.mta.bulk.dto.product.BulkProductSuspensionRequest;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerChannel;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.service.BulkProductSuspensionService;
import com.gdn.partners.bulk.util.Constant;
import com.newrelic.api.agent.Trace;

@Service
public class BulkProductSuspensionListener {

  private static final Logger LOG = LoggerFactory.getLogger(BulkProductSuspensionListener.class);

  @Autowired
  private BulkProductSuspensionService bulkProductSuspensionService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkProductSuspension()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    LOG.info("Consume event {} with message {} ", kafkaTopicProperties.getBulkProductSuspension(), message);
    BulkProductSuspensionRequest bulkProductSuspensionRequest =
        objectMapper.readValue(message, BulkProductSuspensionRequest.class);
    LoggerAttributeModel loggerModel = null;
    try {
      loggerModel =
          new LoggerAttributeModel(this, "onDomainEventConsumed", null, bulkProductSuspensionRequest.getUpdatedBy(),
              bulkProductSuspensionRequest.getRequestId(), bulkProductSuspensionRequest.getStoreId(), null,
              LoggerChannel.KAFKA.getValue(), bulkProductSuspensionRequest.getFilePath(),
              String.valueOf(bulkProductSuspensionRequest));
      LOG.info(LoggerStandard.getLogInfoTemplate(loggerModel));
      setMandatoryParameters(bulkProductSuspensionRequest.getStoreId(), bulkProductSuspensionRequest.getUpdatedBy());
      bulkProductSuspensionService.process(bulkProductSuspensionRequest);
    } catch (Exception e) {
      LOG.error(LoggerStandard
          .convertErrorTemplate(this, "onDomainEventConsumed", loggerModel, e, bulkProductSuspensionRequest), e);
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