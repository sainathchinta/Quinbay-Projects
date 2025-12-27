package com.gdn.x.product.service.api;

import com.gdn.x.product.model.entity.ProductScore;

public interface ProductScoreHistoryL3Service {

  /**
   * Save history for l3 product score
   * @param storeId
   * @param productSku
   * @param existingProductScore
   * @param updatedProductScore
   * @throws Exception
   */
  void saveProductScoreHistoryL3(String storeId, String productSku, ProductScore existingProductScore,
      ProductScore updatedProductScore) throws Exception;
}
