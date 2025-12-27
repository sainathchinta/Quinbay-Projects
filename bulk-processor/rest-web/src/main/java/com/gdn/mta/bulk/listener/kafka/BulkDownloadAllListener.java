package com.gdn.mta.bulk.listener.kafka;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerChannel;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.models.download.BulkDownloadRequest;
import com.gdn.mta.bulk.service.download.BulkProcessDownloadService;
import com.newrelic.api.agent.Trace;

/**
 * Created by hardikbohra on 27/05/18.
 */
@Service
public class BulkDownloadAllListener {

  private static final Logger LOG = LoggerFactory.getLogger(BulkDownloadAllListener.class);

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private BulkProcessDownloadService bulkProcessDownloadService;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkDownloadAllEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    LOG.info("Consume event {} with message {}", kafkaTopicProperties.getBulkDownloadAllEvent(), message);
    BulkDownloadRequest bulkDownloadRequest = objectMapper.readValue(message, BulkDownloadRequest.class);
    LoggerAttributeModel loggerModel = null;
    if (bulkDownloadRequest == null) {
      loggerModel = new LoggerAttributeModel(this, "onDomainEventConsumed", null);
      LOG.error(LoggerStandard.getLogErrorTemplate(loggerModel, new Exception("Bulk Download : No message " +
          "received from queue, ignoring message..")));
      return;
    }

    try {
      loggerModel = new LoggerAttributeModel(this, "onDomainEventConsumed", null, bulkDownloadRequest.getUsername(),
          bulkDownloadRequest.getRequestId(), null, null, LoggerChannel.KAFKA.getValue(), null, String.valueOf
          (bulkDownloadRequest));
      bulkProcessDownloadService.downloadAll(bulkDownloadRequest);
      LOG.info(LoggerStandard.getLogInfoTemplate(loggerModel));
    } catch (Exception e) {
      LOG.error(LoggerStandard.convertErrorTemplate(this, "onDomainEventConsumed", loggerModel, e,
          bulkDownloadRequest), e);
      throw e;
    }
  }
}
