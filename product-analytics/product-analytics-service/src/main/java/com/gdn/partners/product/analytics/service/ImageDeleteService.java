package com.gdn.partners.product.analytics.service;

public interface ImageDeleteService {

  /**
   * to update image collection for permanent delete
   *
   * @param productCode
   */
  void updateImageCollectionForProductDelete(String productCode);

  /**
   * Delete images from GCS
   * @param storeId non null storeId
   * @param productCode can be null
   */
  void deleteImagesOfProduct(String storeId, String productCode);
}