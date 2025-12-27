package com.gdn.mta.bulk.listener.kafka;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.BulkRestrictedKeywordUploadModel;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerChannel;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.service.InternalProcessServiceWrapper;
import com.gdn.partners.bulk.util.Constant;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class RestrictedKeywordUploadListener {

  @Autowired
  private ObjectMapper objectMapper;
  @Autowired
  private InternalProcessServiceWrapper internalProcessServiceWrapper;
  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkRestrictedKeywordUploadEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("Consume event {} with message {} ", kafkaTopicProperties.getBulkRestrictedKeywordUploadEvent(), message);
    BulkRestrictedKeywordUploadModel bulkRestrictedKeywordUploadModel = null;
    LoggerAttributeModel loggerModel = null;
    try {
      bulkRestrictedKeywordUploadModel = objectMapper.readValue(message, BulkRestrictedKeywordUploadModel.class);
      loggerModel = new LoggerAttributeModel(this, "onDomainEventConsumed",
          bulkRestrictedKeywordUploadModel.getBulkProcessCode(), bulkRestrictedKeywordUploadModel.getUpdatedBy(),
          bulkRestrictedKeywordUploadModel.getRequestId(), bulkRestrictedKeywordUploadModel.getStoreId(), null,
          LoggerChannel.KAFKA.getValue(), bulkRestrictedKeywordUploadModel.getFilePath(),
          String.valueOf(bulkRestrictedKeywordUploadModel));
      log.info(LoggerStandard.getLogInfoTemplate(loggerModel));
      setMandatoryParameters(bulkRestrictedKeywordUploadModel.getStoreId(),
          bulkRestrictedKeywordUploadModel.getUpdatedBy());
      internalProcessServiceWrapper.uploadBulkRestrictedKeywordProcess(bulkRestrictedKeywordUploadModel.getStoreId(),
          bulkRestrictedKeywordUploadModel);
    } catch (Exception e) {
      log.error(LoggerStandard.convertErrorTemplate(this, "onDomainEventConsumed", loggerModel, e,
          bulkRestrictedKeywordUploadModel), e);
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
