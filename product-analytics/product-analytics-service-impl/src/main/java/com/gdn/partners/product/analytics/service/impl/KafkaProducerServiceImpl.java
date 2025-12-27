package com.gdn.partners.product.analytics.service.impl;

import com.gdn.partners.product.analytics.service.impl.helper.KafkaPublisher;
import model.ProductAttributeExtractionsEventModel;
import model.TerminatedSellerDeletionEventModel;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;


import com.gdn.partners.product.analytics.service.KafkaProducerService;
import com.gdn.partners.product.analytics.web.model.SellerFieldsChangeResponse;
import com.gdn.partners.product.analytics.web.model.config.DomainEventName;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class KafkaProducerServiceImpl implements KafkaProducerService {

  @Autowired
  private KafkaPublisher kafkaPublisher;

  @Override
  public void publishMessage(SellerFieldsChangeResponse sellerFieldsChangeResponse) {
    try {
      kafkaPublisher.send(DomainEventName.SELLER_AUTO_QC_DATA_UPDATE,
          sellerFieldsChangeResponse.getSellerCode() + sellerFieldsChangeResponse.getCategoryCode(),
          sellerFieldsChangeResponse);
      log.info("Publish event {} with payload {} ", DomainEventName.SELLER_AUTO_QC_DATA_UPDATE,
          sellerFieldsChangeResponse);
    } catch (Exception e) {
      log.error("Error when publish event {} with payload {} ", DomainEventName.SELLER_AUTO_QC_DATA_UPDATE,
          sellerFieldsChangeResponse, e);
    }
  }

  @Override
  public void publishMessageForProductDeletion(
    TerminatedSellerDeletionEventModel terminatedSellerDeletionEventModel, String topicName) {
    kafkaPublisher.send(topicName, terminatedSellerDeletionEventModel.getProductCode(),
      terminatedSellerDeletionEventModel);
    log.info("Publishing event {} with payload {}", topicName, terminatedSellerDeletionEventModel);
  }

  @Override
  public void publishMessageForProductAttributeExtractions(
      ProductAttributeExtractionsEventModel productAttributeExtractionsEventModel, String topicName) {
    log.info("Publishing event {} with payload {}", topicName, productAttributeExtractionsEventModel);
    kafkaPublisher.send(topicName, productAttributeExtractionsEventModel.getProductId(),
        productAttributeExtractionsEventModel);
  }
}
