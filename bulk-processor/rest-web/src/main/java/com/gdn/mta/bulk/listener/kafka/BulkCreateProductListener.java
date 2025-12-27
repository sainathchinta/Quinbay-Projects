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
import com.gdn.x.mta.rest.web.request.BulkListProductCreateRequest;
import com.gdn.x.mta.rest.web.request.ProductApiRequest;
import com.newrelic.api.agent.Trace;

/**
 * Created by hardikbohra on 21/06/18.
 */
@Service
public class BulkCreateProductListener {

  private static final Logger LOG = LoggerFactory.getLogger(BulkCreateProductListener.class);

  @Autowired
  @Qualifier("ProductLevel3ApiProcessorServiceBean")
  private GeneralProcessorService<ProductApiRequest, Void, Void> productLevel3ApiProcessorServiceBean;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkApiCreateProductEvent()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    LOG.info("Consume event {} with message {} ", kafkaTopicProperties.getBulkApiCreateProductEvent(), message);
    BulkListProductCreateRequest bulkListProductCreateRequest =
        objectMapper.readValue(message, BulkListProductCreateRequest.class);
    LoggerAttributeModel loggerModel = null;
    try {
      for (ProductApiRequest productApiRequest : bulkListProductCreateRequest.getProducts()) {
        productApiRequest.setRequestId(bulkListProductCreateRequest.getRequestId());
        loggerModel = new LoggerAttributeModel(this, "onDomainEventConsumed",
            bulkListProductCreateRequest.getMerchantCode(), null, bulkListProductCreateRequest.getRequestId(),
            null, LoggerChannel.KAFKA.getValue(), LoggerClient.MTAAPI.getValue(), productApiRequest.getProductName
            (), null);
        productLevel3ApiProcessorServiceBean.preProcess(productApiRequest, null);
        LOG.info(LoggerStandard.getLogInfoTemplate(loggerModel));
      }
    } catch (Exception ex) {
      LOG.error(LoggerStandard.convertErrorTemplate(this, "onDomainEventConsumed", loggerModel, ex,
          bulkListProductCreateRequest), ex);
    }
  }
}
