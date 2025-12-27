package com.gdn.mta.bulk.listener.kafka;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerChannel;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.service.EANProductLevel4BulkUpdateService;
import com.gdn.mta.bulk.service.TrackerService;
import com.newrelic.api.agent.Trace;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.FAILED;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.HYPHEN;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.PRODUCT_UPDATE_ATTRI_TYPE;
import static com.gdn.mta.bulk.dto.product.constant.TrackerConstants.PRODUCT_UPDATE_EVENT;

@Service
@ConditionalOnProperty(value = "bulk.update.ean.listener.switch", havingValue = "true")
public class BulkUploadEANListener {
  private static final Logger LOG = LoggerFactory.getLogger(BulkUploadEANListener.class);
  @Autowired
  private TrackerService trackerService;
  @Autowired
  private ObjectMapper objectMapper;
  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;
  @Autowired
  private EANProductLevel4BulkUpdateService eanProductLevel4BulkUpdateService;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkUploadEANEvent()}",
      autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    LOG.info("Consume event {} with message {} ", kafkaTopicProperties.getBulkUploadEANEvent(), message);
    BulkUpdateQueue bulkUpdateQueue = objectMapper.readValue(message, BulkUpdateQueue.class);
    LoggerAttributeModel loggerModel = null;
    String userName = StringUtils.EMPTY;
    try {
      userName = bulkUpdateQueue.getUpdatedBy();
      loggerModel =
          new LoggerAttributeModel(this, "onDomainEventConsumed", bulkUpdateQueue.getBusinessPartnerCode(), null,
              bulkUpdateQueue.getRequestId(), null, null, LoggerChannel.KAFKA.getValue(),
              bulkUpdateQueue.getBulkProcessType() + ":" + bulkUpdateQueue.getBulkProcessCode(),
              String.valueOf(bulkUpdateQueue));

      eanProductLevel4BulkUpdateService.processBulkEANUpdate(bulkUpdateQueue);
      LOG.info(LoggerStandard.getLogInfoTemplate(loggerModel));
    } catch (Exception e) {
      LOG.error(LoggerStandard.convertErrorTemplate(this, "onDomainEventConsumed", loggerModel, e, bulkUpdateQueue), e);
      this.trackerService.sendTracker(PRODUCT_UPDATE_EVENT, PRODUCT_UPDATE_ATTRI_TYPE, HYPHEN, FAILED, userName);
    }
  }

}
