package com.gdn.partners.product.analytics.service;

import model.ProductAttributeFeedbackEventModel;
import model.ProductAttributeExtractionsEventModel;

import java.lang.reflect.InvocationTargetException;
import java.util.List;

public interface ProductAttributeExtractionsService {

  /**
   * Publish events for product-attribute extractions
   *
   * @param storeId
   * @param batchSize
   */
  void publishEventsForProductAttributeExtractions(String storeId, int batchSize);

  /**
   * Publish events for product-attribute extractions by product SKU
   * @param storeId
   * @param productSku
   */
  void publishEventsForProductAttributeExtractionsByProductSku(String storeId, List<String> productSku);

  /**
   * Validate and publish events to PCB for product-attribute extractions
   *
   * @param productAttributeExtractionsEventModel
   */
  void validateAndPublishPCBEventsForProductAttributeExtractions(
      ProductAttributeExtractionsEventModel productAttributeExtractionsEventModel)
      throws InvocationTargetException, IllegalAccessException;

  /**
   * Update ds extracted attribute feedback from user
   * @param eventModel Event model consisting feedback
   */
  void updateFeedbackForProductAttribute(ProductAttributeFeedbackEventModel eventModel)
    throws InvocationTargetException, IllegalAccessException;
}