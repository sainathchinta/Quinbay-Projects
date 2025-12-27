package com.gdn.mta.bulk.listener.kafka;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.BulkDownloadQueue;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerChannel;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.service.BulkDownloadService;
import com.newrelic.api.agent.Trace;

/**
 * Created by hardikbohra on 25/05/18.
 */
@Service
public class BulkDownloadProductListener {

  private static final Logger LOG = LoggerFactory.getLogger(BulkDownloadProductListener.class);

  @Autowired
  private BulkDownloadService bulkDownloadService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkProductDownloadEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    LOG.info("Consume event {} with message {} ", kafkaTopicProperties.getBulkProductDownloadEvent(), message);
    BulkDownloadQueue bulkDownloadQueue = objectMapper.readValue(message, BulkDownloadQueue.class);
    LoggerAttributeModel loggerModel = null;
    try {
      loggerModel =
          new LoggerAttributeModel(this, "onDomainEventConsumed", bulkDownloadQueue.getBusinessPartnerCode(), null,
              bulkDownloadQueue.getRequestId(), null, null, LoggerChannel.KAFKA.getValue(),
              bulkDownloadQueue.getEmailTo(), String.valueOf(bulkDownloadQueue));

      bulkDownloadService.postProcess(bulkDownloadQueue);
      LOG.info(LoggerStandard.getLogInfoTemplate(loggerModel));
    } catch (Exception e) {
      LOG.error(LoggerStandard.convertErrorTemplate(this, "onDomainEventConsumed", loggerModel, e, bulkDownloadQueue),
          e);
      throw e;
    }
  }
}
