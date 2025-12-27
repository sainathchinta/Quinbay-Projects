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
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.service.GeneralProcessorService;
import com.gdn.x.mta.rest.web.request.ProductApiRequest;
import com.newrelic.api.agent.Trace;

/**
 * Created by hardikbohra on 28/05/18.
 */
@Service
public class CreateProductListener {

  private static final Logger LOG = LoggerFactory.getLogger(CreateProductListener.class);

  @Autowired
  @Qualifier("ProductLevel3ApiProcessorServiceBean")
  private GeneralProcessorService<ProductApiRequest, Void, Void> productLevel3ApiProcessorServiceBean;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getCreateProductEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    LOG.info("Consume event {} with message {}", kafkaTopicProperties.getCreateProductEvent(), message);
    ProductApiRequest productApiRequest = objectMapper.readValue(message, ProductApiRequest.class);
    LoggerAttributeModel loggerModel = null;
    try {
      loggerModel = new LoggerAttributeModel(this, "onDomainEventConsumed", productApiRequest.getMerchantCode(),
          productApiRequest.getUsername(), productApiRequest.getRequestId(), null, null, LoggerChannel.KAFKA.getValue
          (), null, String.valueOf(productApiRequest));

      productLevel3ApiProcessorServiceBean.process(productApiRequest);
      LOG.info(LoggerStandard.getLogInfoTemplate(loggerModel));
    } catch (Exception e) {
      LOG.error(LoggerStandard.convertErrorTemplate(this, "onDomainEventConsumed", loggerModel, e, productApiRequest)
          , e);
      throw e;
    }
  }
}
