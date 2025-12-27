package com.gdn.mta.bulk.listener.kafka;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.config.KafkaTopicProperties;
import com.gdn.mta.bulk.models.DormantSellerItemDetail;
import com.gdn.mta.bulk.service.DormantSellerService;
import com.newrelic.api.agent.Trace;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class DormantSellerItemViewConfigUpdateListener {

  @Autowired
  private DormantSellerService dormantSellerService;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Trace(dispatcher = true)
  @KafkaListener(topics = "#{kafkaTopicProperties.getDormantSellerItemSkuViewConfigUpdate()}", autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws Exception {
    log.info("onDomainEventConsumed : dormant seller item viewConfig event : {} with update request : {}",
        kafkaTopicProperties.getDormantSellerItemSkuViewConfigUpdate(), message);
    DormantSellerItemDetail dormantSellerItemDetail = objectMapper.readValue(message, DormantSellerItemDetail.class);
    try {
      dormantSellerService.updateProductItemViewConfig(dormantSellerItemDetail);
    } catch (Exception e) {
      log.error(
          "onDomainEventConsumed : Error occurred on Dormant seller Item viewConfig Event consumed : {} error message : {} ",
          dormantSellerItemDetail, e.getMessage(), e);
    }
  }
}
