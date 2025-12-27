package com.gdn.mta.bulk.listener.kafka;

import java.io.IOException;

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
import com.gdn.mta.bulk.models.BulkProcessPostImageV2Request;
import com.gdn.mta.bulk.service.GeneralProcessorService;
import com.newrelic.api.agent.Trace;

/**
 * Created by hardikbohra on 28/05/18.
 */
@Service
public class BulkProcessImageV2Listener {

  public static final String CREATE_PRODUCT_ACTION = "createProduct";
  public static final String POST_IMAGE_ACTION = "postImage";
  public static final String POST_IMAGE_ACTION_V2 = "postImageV2";
  private static final Logger LOG = LoggerFactory.getLogger(BulkProcessImageV2Listener.class);

  @Autowired
  @Qualifier("PostImageApiProcessorV2ServiceBean")
  private GeneralProcessorService<BulkProcessPostImageV2Request, Void, Void> postImageApiProcessorV2Service;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkApiProcessImageV2Event()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws IOException {
    LOG.info("Consume event {} with message {} ", kafkaTopicProperties.getBulkApiProcessImageV2Event(), message);
    BulkProcessPostImageV2Request bulkProcessPostImageRequest =
        objectMapper.readValue(message, BulkProcessPostImageV2Request.class);
    LoggerAttributeModel loggerModel = null;
    try {
        loggerModel = new LoggerAttributeModel(this, "onDomainEventConsumedUploadImageV2",
            bulkProcessPostImageRequest.getMerchantCode(), null, bulkProcessPostImageRequest.getRequestId(), null,
            LoggerChannel.KAFKA.getValue(), LoggerClient.MTAAPI.getValue(), null, String.valueOf
            (bulkProcessPostImageRequest));
        postImageApiProcessorV2Service.process(bulkProcessPostImageRequest);
        LOG.info(LoggerStandard.getLogInfoTemplate(loggerModel));
    } catch (Exception e) {
      LOG.error(LoggerStandard.convertErrorTemplate(this, "onDomainEventConsumed", loggerModel, e,
          bulkProcessPostImageRequest), e);
    }
  }
}
