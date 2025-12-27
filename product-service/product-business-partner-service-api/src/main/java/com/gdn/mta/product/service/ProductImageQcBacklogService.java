package com.gdn.mta.product.service;

import java.util.List;

import com.gdn.mta.product.entity.ProductImageQcBacklog;

public interface ProductImageQcBacklogService {

  /**
   * Find list of productImageQcBackLog by storeId and status
   *
   * @param storeId
   * @param status
   * @param orderProductsBy
   * @param orderProductsIn
   * @param batchSize
   * @return
   */
  List<ProductImageQcBacklog> findProductImageQcBacklogByStoreIdAndStatus(String storeId, String status, String orderProductsBy,
      String orderProductsIn, int batchSize);

  /**
   * Find by productCode and status
   *
   * @param storeId
   * @param productCode
   * @param status
   * @return
   */
  ProductImageQcBacklog findByStoreIdAndProductCodeAndStatus(String storeId, String productCode, String status);

  /**
   * Save ProductImageQcBacklog
   *
   * @param productImageQcBacklog
   * @return
   */
  ProductImageQcBacklog saveProductImageQcBacklog(ProductImageQcBacklog productImageQcBacklog);
}