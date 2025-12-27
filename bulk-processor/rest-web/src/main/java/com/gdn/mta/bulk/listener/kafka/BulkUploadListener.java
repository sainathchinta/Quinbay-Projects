package com.gdn.mta.bulk.listener.kafka;

import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.FAILED;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.HYPHEN;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.PRODUCT_UPDATE_EVENT;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerChannel;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.service.BulkUpdateService;
import com.gdn.mta.bulk.service.TrackerService;
import com.newrelic.api.agent.Trace;

/**
 * Created by hardikbohra on 28/05/18.
 */
@Service
public class BulkUploadListener {

  private static final Logger LOG = LoggerFactory.getLogger(BulkUploadListener.class);
  private static final String SERVICE_BEAN_NAME = "BulkUpdateService";

  @Autowired
  private AutowireCapableBeanFactory autowireCapableBeanFactory;

  @Autowired
  private TrackerService trackerService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkUploadEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    LOG.info("Consume event {} with message {} ", kafkaTopicProperties.getBulkUploadEvent(), message);
    BulkUpdateQueue bulkUpdateQueue = objectMapper.readValue(message, BulkUpdateQueue.class);
    LoggerAttributeModel loggerModel = null;
    String userName = StringUtils.EMPTY;
    try {
      userName = bulkUpdateQueue.getUpdatedBy();
      BulkUpdateService bulkUpdateService = (BulkUpdateService) autowireCapableBeanFactory.getBean(bulkUpdateQueue
          .getBulkProcessType() + SERVICE_BEAN_NAME);

      loggerModel = new LoggerAttributeModel(this, "onDomainEventConsumed", bulkUpdateQueue.getBusinessPartnerCode(),
          null, bulkUpdateQueue.getRequestId(), null, null, LoggerChannel.KAFKA.getValue(), bulkUpdateQueue
          .getBulkProcessType() + ":" + bulkUpdateQueue.getBulkProcessCode(), String.valueOf(bulkUpdateQueue));

      bulkUpdateService.processBulkUpdateEvent(bulkUpdateQueue);
      LOG.info(LoggerStandard.getLogInfoTemplate(loggerModel));
    } catch (Exception e) {
      LOG.error(LoggerStandard.convertErrorTemplate(this, "onDomainEventConsumed", loggerModel, e, bulkUpdateQueue), e);
      this.trackerService.sendTracker(PRODUCT_UPDATE_EVENT, PRODUCT_UPDATE_ATTRI_TYPE, HYPHEN, FAILED, userName);
    }
  }

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkUpdatePriority1Event()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumedForPriority1(String message) throws Exception {
    LOG.info("Consume event {} with message {} ", kafkaTopicProperties.getBulkUpdatePriority1Event(), message);
    BulkUpdateQueue bulkUpdateQueue = objectMapper.readValue(message, BulkUpdateQueue.class);
    LoggerAttributeModel loggerModel = null;
    String userName = StringUtils.EMPTY;
    try {
      userName = bulkUpdateQueue.getUpdatedBy();
      BulkUpdateService bulkUpdateService = (BulkUpdateService) autowireCapableBeanFactory.getBean(bulkUpdateQueue
          .getBulkProcessType() + SERVICE_BEAN_NAME);

      loggerModel = new LoggerAttributeModel(this, "onDomainEventConsumedForPriority1", bulkUpdateQueue.getBusinessPartnerCode(),
          null, bulkUpdateQueue.getRequestId(), null, null, LoggerChannel.KAFKA.getValue(), bulkUpdateQueue
          .getBulkProcessType() + ":" + bulkUpdateQueue.getBulkProcessCode(), String.valueOf(bulkUpdateQueue));

      bulkUpdateService.processBulkUpdateEvent(bulkUpdateQueue);
      LOG.info(LoggerStandard.getLogInfoTemplate(loggerModel));
    } catch (Exception e) {
      LOG.error(LoggerStandard.convertErrorTemplate(this, "onDomainEventConsumedForPriority1", loggerModel, e, bulkUpdateQueue), e);
      this.trackerService.sendTracker(PRODUCT_UPDATE_EVENT, PRODUCT_UPDATE_ATTRI_TYPE, HYPHEN, FAILED, userName);
    }
  }
}
