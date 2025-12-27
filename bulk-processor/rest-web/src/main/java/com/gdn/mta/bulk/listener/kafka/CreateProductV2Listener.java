package com.gdn.mta.bulk.listener.kafka;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.dto.product.ProductV2Request;
import com.gdn.mta.bulk.logger.LoggerAttributeModel;
import com.gdn.mta.bulk.logger.LoggerChannel;
import com.gdn.mta.bulk.logger.LoggerClient;
import com.gdn.mta.bulk.logger.LoggerStandard;
import com.gdn.mta.bulk.service.GeneralProcessorService;
import com.newrelic.api.agent.Trace;

/**
 * Listener to process create product flow 1 from MTA-API 
 * version 2
 * 
 * @author agie.falah
 *
 */
@Service
public class CreateProductV2Listener {
  
  private static final Logger LOG = LoggerFactory.getLogger(CreateProductV2Listener.class);
  
  @Autowired
  @Qualifier("CreateProductV2ServiceBean")
  private GeneralProcessorService<ProductV2Request, Void, Void> createProductV2ApiService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getBulkApiCreateProductV2Event()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    LOG.info("Consume event {} with message {} ", kafkaTopicProperties.getBulkApiCreateProductV2Event(), message);
    ProductV2Request productV2Request = objectMapper.readValue(message, ProductV2Request.class);
    LoggerAttributeModel loggerModel =
        new LoggerAttributeModel(this, "onDomainEventConsumed", productV2Request.getBpCode(),
            productV2Request.getUsername(), productV2Request.getRequestId(), productV2Request.getStoreId(),
            LoggerChannel.KAFKA.getValue(), LoggerClient.MTAAPI.getValue(), null, null);
    try{
      createProductV2ApiService.process(productV2Request);
    } catch(Exception e){
      LOG.error(LoggerStandard.convertErrorTemplate(this, "listen", loggerModel, e, null), e);
    }
  }

}
