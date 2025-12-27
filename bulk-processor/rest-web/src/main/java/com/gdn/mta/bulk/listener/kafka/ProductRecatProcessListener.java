package com.gdn.mta.bulk.listener.kafka;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.entity.ProductCodeAndCategoryCodeListEvent;
import com.gdn.mta.bulk.service.RecatProcessServiceWrapper;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductRecatProcessListener {

  @Autowired
  private RecatProcessServiceWrapper processServiceWrapper;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getProductRecatProcess()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message)
      throws Exception {
    log.info("Consume event : {} , with message : {} ", kafkaTopicProperties.getProductRecatProcess(), message);
    ProductCodeAndCategoryCodeListEvent productCodeAndCategoryCodeListEvent =
        objectMapper.readValue(message, ProductCodeAndCategoryCodeListEvent.class);
    try {
      processServiceWrapper.updateProductCategory(productCodeAndCategoryCodeListEvent);
    } catch (Exception e) {
      log.error("Error when consume event com.gdn.mta.bulk.product.recat.process ", e);
    }
  }
}
