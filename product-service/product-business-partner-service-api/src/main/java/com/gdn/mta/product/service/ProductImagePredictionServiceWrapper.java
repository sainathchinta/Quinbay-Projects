package com.gdn.mta.product.service;

import com.gda.mta.product.dto.ProductImagePredictionAndCategoryMappingRequest;
import com.gdn.mta.product.entity.ProductImagePrediction;

public interface ProductImagePredictionServiceWrapper {

  /**
   * update image prediction and category mapping and cache evict
   *
   * @param storeId
   * @param productImagePrediction
   * @throws Exception
   */
  void updateImagePredictionAndCategoryMappingAndCacheEvict(String storeId,
      ProductImagePredictionAndCategoryMappingRequest productImagePrediction) throws Exception;

  /**
   * update image prediction and cache evict
   *
   * @param productImagePrediction
   */
  void update(ProductImagePrediction productImagePrediction);
}
