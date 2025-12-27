package com.gdn.x.product.service.event.listener;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.gdn.x.product.service.api.AllowedAttributeValuesService;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.AttributeDomainEventModel;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
@ConditionalOnProperty(value = "com.gdn.x.productcategorybase.attribute.publish.listener.enabled"
    , havingValue = "true")
public class AttributeChangeListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private AllowedAttributeValuesService allowedAttributeValuesService;

  @KafkaListener(topics = DomainEventName.MASTER_ATTRIBUTE_INFO_EVENT, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Received message for attribute change, payload : {}", message);
    try {
      AttributeDomainEventModel attributeDomainEventModel =
          this.objectMapper.readValue(message, AttributeDomainEventModel.class);
      this.allowedAttributeValuesService.updateAttributeValueSequenceByAttributeCode(attributeDomainEventModel);
    } catch (Exception e) {
      log.error("Error on consuming attribute change event, error - ", e);
    }
  }
}
