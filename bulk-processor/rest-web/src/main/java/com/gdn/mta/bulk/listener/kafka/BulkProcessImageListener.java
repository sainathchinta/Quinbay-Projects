package com.gdn.mta.bulk.listener.kafka;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerChannel;
import com.gdn.mta.bulk.logger.LoggerClient;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.service.GeneralProcessorService;
import com.gdn.x.mta.rest.web.request.BulkProcessPostImageRequest;
import com.newrelic.api.agent.Trace;

/**
 * Created by hardikbohra on 21/06/18.
 */
@Service
public class BulkProcessImageListener {

  private static final Logger LOG = LoggerFactory.getLogger(BulkProcessImageListener.class);

  @Autowired
  @Qualifier("PostImageApiProcessorServiceBean")
  private GeneralProcessorService<BulkProcessPostImageRequest, Void, Void> postImageApiProcessorService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkApiProcessImageV1Event()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    LOG.info("Consume event {} with message {}", kafkaTopicProperties.getBulkApiProcessImageV1Event(), message);
    BulkProcessPostImageRequest bulkProcessPostImageRequest =
        objectMapper.readValue(message, BulkProcessPostImageRequest.class);
    LoggerAttributeModel loggerModel = null;
    try {
      loggerModel = new LoggerAttributeModel(this, "onDomainEventConsumed", bulkProcessPostImageRequest
          .getMerchantCode(), null, bulkProcessPostImageRequest.getRequestId(), null, LoggerChannel.KAFKA.getValue
          (), LoggerClient.MTAAPI.getValue(), null, String.valueOf(bulkProcessPostImageRequest));
      postImageApiProcessorService.process(bulkProcessPostImageRequest);
      LOG.info(LoggerStandard.getLogInfoTemplate(loggerModel));
    } catch (Exception ex) {
      LOG.error(LoggerStandard.convertErrorTemplate(this, "onDomainEventConsumed", loggerModel, ex,
          bulkProcessPostImageRequest), ex);
    }
  }
}
