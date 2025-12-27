package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerChannel;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.service.EANProductLevel4BulkUpdateService;
import com.gdn.mta.bulk.service.util.GdnMandatoryRequestParameterUtil;
import com.gdn.partners.bulk.util.Constant;
import com.newrelic.api.agent.Trace;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@ConditionalOnProperty(value = "bulk.update.ean.listener.switch", havingValue = "true")
public class BulkUploadUpdateEANItemListener {

  @Autowired
  private EANProductLevel4BulkUpdateService eanProductLevel4BulkUpdateService;

  private static final Logger LOG = LoggerFactory.getLogger(BulkUploadUpdateEANItemListener.class);

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkUploadUpdateEANItem()}",
      autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    BulkUpdateEventModel bulkUpdateEventModel = objectMapper.readValue(message, BulkUpdateEventModel.class);
    LoggerAttributeModel loggerModel = null;
    try {
      loggerModel = new LoggerAttributeModel(this, "onDomainEventConsumed", null, null, null, null, null,
          LoggerChannel.KAFKA.getValue(), kafkaTopicProperties.getBulkUploadUpdateEANItem(),
          String.valueOf(bulkUpdateEventModel));
      LOG.info(LoggerStandard.getLogInfoTemplate(loggerModel));
      setMandatoryParameters(bulkUpdateEventModel.getStoreId());
      eanProductLevel4BulkUpdateService.processBulkEANUpdateItem(bulkUpdateEventModel);
    } catch (Exception e) {
      LOG.error(LoggerStandard.convertErrorTemplate(this,
          "onDomainEventConsumed " + kafkaTopicProperties.getBulkUploadUpdateEANItem(), loggerModel, e,
          bulkUpdateEventModel), e);
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
          "Error while listening and processing " + kafkaTopicProperties.getBulkUploadUpdateEANItem()
              + "bulkUpdateEventModel : " + bulkUpdateEventModel.toString() + ", error is : " + e.getMessage(), e);
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
