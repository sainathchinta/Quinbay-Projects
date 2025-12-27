package com.gdn.partners.product.analytics.service;

import com.gdn.partners.product.analytics.web.model.SellerFieldsChangeResponse;
import model.ProductAttributeExtractionsEventModel;
import model.TerminatedSellerDeletionEventModel;

public interface KafkaProducerService {

  /**
   * publish kafka message
   *
   * @return
   * @param sellerFieldsChangeResponse
   */
  void publishMessage(SellerFieldsChangeResponse sellerFieldsChangeResponse);

  /**
   * publish to the particular service for product deletion
   * @param terminatedSellerDeletionEventModel
   * @param topicName
   */
  void publishMessageForProductDeletion(
      TerminatedSellerDeletionEventModel terminatedSellerDeletionEventModel, String topicName);

  /**
   * publish events for product-attribute extractions
   *
   * @param productAttributeExtractionsEventModel
   * @param topicName
   */
  void publishMessageForProductAttributeExtractions(
      ProductAttributeExtractionsEventModel productAttributeExtractionsEventModel, String topicName);
}
