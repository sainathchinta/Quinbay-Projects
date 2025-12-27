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
import com.gdn.mta.bulk.entity.BulkCreateProductEventModel;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerChannel;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.service.BulkCategoryProcessorService;
import com.gdn.partners.bulk.util.Constant;
import com.newrelic.api.agent.Trace;

@Service
public class BulkCategoryCreateProductListener {

  @Autowired
  private BulkCategoryProcessorService bulkCategoryProcessorService;

  private static final Logger LOG = LoggerFactory.getLogger(BulkCategoryCreateProductListener.class);

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkCnCreateProductEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    LOG.info("Consume event {} with message {} ", kafkaTopicProperties.getBulkCnCreateProductEvent(), message);
    BulkCreateProductEventModel bulkCreateProductEventModel =
        objectMapper.readValue(message, BulkCreateProductEventModel.class);
    LoggerAttributeModel loggerModel = null;
    try {
      loggerModel = new LoggerAttributeModel(this, "onDomainEventConsumed", null, null, null, null, null,
          LoggerChannel.KAFKA.getValue(), kafkaTopicProperties.getBulkCnCreateProductEvent(),
          String.valueOf(bulkCreateProductEventModel));
      LOG.info(LoggerStandard.getLogInfoTemplate(loggerModel));
      setMandatoryParameters(bulkCreateProductEventModel.getStoreId());
      bulkCategoryProcessorService.processEvent(bulkCreateProductEventModel);
    } catch (Exception e) {
      LOG.error(LoggerStandard.convertErrorTemplate(this, "onDomainEventConsumed", loggerModel, e,
          bulkCreateProductEventModel), e);
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
          "Error while listening and processing " + "BULK_CN_CREATE_PRODUCT_EVENT bulkCreateProductEventModel : "
              + bulkCreateProductEventModel.toString() + ", error is : " + e.getMessage(), e);
    }
  }

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkCnCreateProductForPriority1Event()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedForPriority1(String message) throws Exception {
    LOG.info("Consume event {} with message {} ", kafkaTopicProperties.getBulkCnCreateProductForPriority1Event(), message);
    BulkCreateProductEventModel bulkCreateProductEventModel =
      objectMapper.readValue(message, BulkCreateProductEventModel.class);
    LoggerAttributeModel loggerModel = null;
    try {
      loggerModel = new LoggerAttributeModel(this, "onDomainEventConsumed", null, null, null, null, null,
        LoggerChannel.KAFKA.getValue(), kafkaTopicProperties.getBulkCnCreateProductForPriority1Event(),
        String.valueOf(bulkCreateProductEventModel));
      LOG.info(LoggerStandard.getLogInfoTemplate(loggerModel));
      setMandatoryParameters(bulkCreateProductEventModel.getStoreId());
      bulkCategoryProcessorService.processEvent(bulkCreateProductEventModel);
    } catch (Exception e) {
      LOG.error(LoggerStandard.convertErrorTemplate(this, "onDomainEventConsumed", loggerModel, e,
        bulkCreateProductEventModel), e);
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
        "Error while listening and processing " + "BULK_CN_CREATE_PRODUCT_FOR_PRIORITY_1_EVENT bulkCreateProductEventModel : "
          + bulkCreateProductEventModel.toString() + ", error is : " + e.getMessage(), e);
    }
  }

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkCnCreateProductForPriority2Event()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedForPriority2(String message) throws Exception {
    LOG.info("Consume event {} with message {} ",
        kafkaTopicProperties.getBulkCnCreateProductForPriority2Event(), message);
    BulkCreateProductEventModel bulkCreateProductEventModel =
      objectMapper.readValue(message, BulkCreateProductEventModel.class);
    LoggerAttributeModel loggerModel = null;
    try {
      loggerModel = new LoggerAttributeModel(this, "onDomainEventConsumed", null, null, null, null, null,
        LoggerChannel.KAFKA.getValue(), kafkaTopicProperties.getBulkCnCreateProductForPriority2Event(),
        String.valueOf(bulkCreateProductEventModel));
      LOG.info(LoggerStandard.getLogInfoTemplate(loggerModel));
      setMandatoryParameters(bulkCreateProductEventModel.getStoreId());
      bulkCategoryProcessorService.processEvent(bulkCreateProductEventModel);
    } catch (Exception e) {
      LOG.error(LoggerStandard.convertErrorTemplate(this, "onDomainEventConsumed", loggerModel, e,
        bulkCreateProductEventModel), e);
      throw new ApplicationException(ErrorCategory.COMMUNICATION_FAILURE,
        "Error while listening and processing " +
          "BULK_CN_CREATE_PRODUCT_FOR_PRIORITY_2_EVENT bulkCreateProductEventModel : "
          + bulkCreateProductEventModel.toString() + ", error is : " + e.getMessage(), e);
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
