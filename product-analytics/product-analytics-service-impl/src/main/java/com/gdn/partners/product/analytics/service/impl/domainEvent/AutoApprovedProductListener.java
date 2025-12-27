package com.gdn.partners.product.analytics.service.impl.domainEvent;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.product.analytics.service.AutoApprovedService;
import model.DeleteAutoApprovedProductsEventModel;
import com.gdn.partners.product.analytics.properties.KafkaTopicProperties;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

@Service
@Slf4j
public class AutoApprovedProductListener {

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private AutoApprovedService autoApprovedService;

  @KafkaListener(topics = "#{kafkaTopicProperties.getDeleteAutoApprovedProductEventName()}",
                 autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Listened message from topic : {} , message : {} ",
      kafkaTopicProperties.getDeleteAutoApprovedProductEventName(), message);
    try {
      DeleteAutoApprovedProductsEventModel anchorMappingEventModel =
        objectMapper.readValue(message, DeleteAutoApprovedProductsEventModel.class);
      if (StringUtils.isNotBlank(anchorMappingEventModel.getProductCode())) {
        autoApprovedService.deleteAutoApprovedProduct(anchorMappingEventModel.getProductCode(),
          anchorMappingEventModel.getAction());
      }
    } catch (Exception e) {
      log.error("Error occurred while listening to event , message = {} error - ", message, e);
    }
  }
}