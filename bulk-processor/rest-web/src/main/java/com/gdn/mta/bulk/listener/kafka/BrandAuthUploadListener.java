package com.gdn.mta.bulk.listener.kafka;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkBrandAuthUploadModel;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerChannel;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;
import com.gdn.partners.bulk.util.Constant;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class BrandAuthUploadListener {
  @Autowired
  private ObjectMapper objectMapper;
  @Autowired
  private InternalProcessServiceWrapper internalProcessServiceWrapper;
  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkBrandAuthUploadEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Consume event {} with message {} ", kafkaTopicProperties.getBulkBrandAuthUploadEvent(), message);
    BulkBrandAuthUploadModel bulkBrandAuthUploadModel = null;
    LoggerAttributeModel loggerModel = null;
    try {
      bulkBrandAuthUploadModel = objectMapper.readValue(message, BulkBrandAuthUploadModel.class);
      loggerModel =
          new LoggerAttributeModel(this, "onDomainEventConsumed", bulkBrandAuthUploadModel.getBulkProcessCode(),
              bulkBrandAuthUploadModel.getCreatedBy(), bulkBrandAuthUploadModel.getRequestId(),
              bulkBrandAuthUploadModel.getStoreId(), null, LoggerChannel.KAFKA.getValue(),
              bulkBrandAuthUploadModel.getFilePath(), String.valueOf(bulkBrandAuthUploadModel));
      log.info(LoggerStandard.getLogInfoTemplate(loggerModel));
      setMandatoryParameters(bulkBrandAuthUploadModel.getStoreId(), bulkBrandAuthUploadModel.getCreatedBy());
      internalProcessServiceWrapper.uploadBulkBrandAuthProcess(bulkBrandAuthUploadModel.getStoreId(),
          bulkBrandAuthUploadModel);
    } catch (Exception e) {
      log.error(
          LoggerStandard.convertErrorTemplate(this, "onDomainEventConsumed", loggerModel, e, bulkBrandAuthUploadModel),
          e);
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
