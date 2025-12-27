package com.gdn.mta.bulk.listener.kafka;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.AutowireCapableBeanFactory;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkAddCampaignProductQueue;
import com.gdn.mta.bulk.entity.constants.BulkProcessConstant;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerChannel;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.service.BulkUpdateService;
import com.gdn.mta.bulk.service.TrackerService;
import com.newrelic.api.agent.Trace;

/**
 * Created by diko.raditya on 09/09/18.
 */
@Service
public class BulkUploadCampaignListener {

  private static final Logger LOGGER = LoggerFactory.getLogger(BulkUploadCampaignListener.class);
  private static final String PRODUCT_LEVEL_3 = "ProductLevel3";

  @Autowired
  private AutowireCapableBeanFactory autowireCapableBeanFactory;

  @Autowired
  private TrackerService trackerService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkUploadCampaignEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    LOGGER.info("Consume event {} with message {} ", kafkaTopicProperties.getBulkUploadCampaignEvent(), message);
    BulkAddCampaignProductQueue bulkAddCampaignProductQueue =
        objectMapper.readValue(message, BulkAddCampaignProductQueue.class);
    LoggerAttributeModel loggerModel = null;
    try {
      loggerModel = new LoggerAttributeModel(this, "onDomainEventConsumed",
          bulkAddCampaignProductQueue.getBusinessPartnerCode(), null,
          bulkAddCampaignProductQueue.getRequestId(), null, null, LoggerChannel.KAFKA.getValue(),
          bulkAddCampaignProductQueue.getBulkProcessType() + ":"
              + bulkAddCampaignProductQueue.getBulkProcessCode(),
          String.valueOf(bulkAddCampaignProductQueue));
      LOGGER.info(LoggerStandard.getLogInfoTemplate(loggerModel));
      BulkUpdateService bulkUpdateService = (BulkUpdateService) autowireCapableBeanFactory
          .getBean(PRODUCT_LEVEL_3 + BulkProcessConstant.SERVICE_BEAN_NAME);
      bulkUpdateService.processCampaignProductBulkUpdate(bulkAddCampaignProductQueue);
    } catch (Exception e) {
      LOGGER.error(LoggerStandard.convertErrorTemplate(this, "onDomainEventConsumed", loggerModel,
          e, bulkAddCampaignProductQueue));
    }
  }
}
