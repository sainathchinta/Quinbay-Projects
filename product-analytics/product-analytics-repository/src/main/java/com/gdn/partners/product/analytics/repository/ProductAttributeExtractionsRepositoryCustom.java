package com.gdn.partners.product.analytics.repository;

import com.gdn.partners.product.analytics.entity.ProductAttributeExtractions;
import com.mongodb.bulk.BulkWriteResult;

import java.util.List;

public interface ProductAttributeExtractionsRepositoryCustom {

  /**
   * Upsert for given list of Product Attribute Extractions
   *
   * @param productAttributeExtractions List of product attribute extractions
   * @return Bulk write result
   */
  BulkWriteResult bulkWriteProductAttributeExtractions(
      List<ProductAttributeExtractions> productAttributeExtractions);

  /**
   * Fetch product-attribute extractions in batches to publish events
   *
   * @param storeId
   * @param batchSize
   * @return
   */
  List<ProductAttributeExtractions> fetchProductAttributeExtractions(String storeId,
      int batchSize);
}