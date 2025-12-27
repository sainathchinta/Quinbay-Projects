package com.gdn.x.productcategorybase.domainevent;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.kafka.annotation.KafkaListener;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.domain.event.config.DomainEventName;
import com.gdn.x.productcategorybase.domain.event.model.ProductAttributeExtractionModel;
import com.gdn.x.productcategorybase.service.ProductAttributeExtractionService;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductAttributeExtractionBackfillingListener {

  @Autowired
  private ProductAttributeExtractionService productAttributeExtractionService;

  @Autowired
  private ObjectMapper objectMapper;

  @KafkaListener(topics = DomainEventName.PRODUCT_ATTRIBUTE_EXTRACTION_BACKFILLING_PUBLISH, autoStartup = "#{kafkaTopicProperties.isAutoStartup()}")
  public void onDomainEventConsumed(String message) {
    log.info("Message consumed for the topic : {}, message : {} ",
        DomainEventName.PRODUCT_ATTRIBUTE_EXTRACTION_BACKFILLING_PUBLISH, message);
    try {
      ProductAttributeExtractionModel productAttributeExtractionModel =
          objectMapper.readValue(message, ProductAttributeExtractionModel.class);
      productAttributeExtractionService.updateExtractedAttributesForBackfilling(productAttributeExtractionModel);
    } catch (Exception ex) {
      log.error("error while listening '{}', error is : ",
          DomainEventName.PRODUCT_ATTRIBUTE_EXTRACTION_BACKFILLING_PUBLISH, ex);
    }
  }
}
