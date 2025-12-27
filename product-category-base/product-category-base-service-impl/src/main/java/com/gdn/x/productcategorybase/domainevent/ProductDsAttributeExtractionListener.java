package com.gdn.x.productcategorybase.domainevent;

import org.slf4j.MDC;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.util.GdnMandatoryRequestParameterUtil;
import com.gdn.x.productcategorybase.Constants;
import com.gdn.x.productcategorybase.domain.event.model.ProductSuitabilityEventModel;
import com.gdn.x.productcategorybase.service.ProductServiceWrapper;
import com.gdn.x.productcategorybase.service.config.KafkaTopicProperties;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductDsAttributeExtractionListener {

  @Autowired
  private ObjectMapper objectMapper;

  @Autowired
  private KafkaTopicProperties kafkaTopicProperties;

  @Autowired
  private ProductServiceWrapper productServiceWrapper;

  @KafkaListener(topics = "#{kafkaTopicProperties.getProductDsAttributeExtractionEvent()}",
      autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) throws JsonProcessingException {
    log.info("Received ProductSuitabilityEventModel, from topic {}, message : {} ",
        kafkaTopicProperties.getProductDsAttributeExtractionEvent(), message);
    MDC.put(GdnMandatoryRequestParameterUtil.USERNAME_KEY_PARAMETER,  Constants.DS_EXTRACTED_ATTRIBUTE);
    try {
      ProductSuitabilityEventModel productSuitabilityEventModel =
          objectMapper.readValue(message, ProductSuitabilityEventModel.class);
      productServiceWrapper.validateAndProcessProductDsAttributeMapping(productSuitabilityEventModel);
    } catch (Exception exception) {
      log.error("Error while processing ds attribute mapping to product {} ", message, exception);
    }
  }
}
