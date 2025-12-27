package com.gdn.mta.product.service;

import java.util.List;

import com.gdn.mta.product.entity.ProductImageQcProcessingResponse;

public interface ProductImageQcProcessingResponseService {

  ProductImageQcProcessingResponse findByStoreIdAndProductCode(String storeId, String productCode);

  List<ProductImageQcProcessingResponse> findByStoreIdAndProductCodeIn(String storeId, List<String> productCodes);

  /**
   * Fetch image qc response from DB
   *
   * @param storeId
   * @param productCode
   * @return
   */
  ProductImageQcProcessingResponse findByStoreIdAndProductCodeDb(String storeId, String productCode);

  void save(ProductImageQcProcessingResponse productImageQcProcessingResponse);

  /**
   * delete entries from product image qc processing response repo by storeId and productCode
   * @param storeId
   * @param productCode
   */
  void deleteProductImageQcProcessingResponseByStoreIdAndProductCode(String storeId, String productCode);
}
