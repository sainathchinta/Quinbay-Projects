package com.gdn.partners.pcu.internal.service;

import com.gdn.x.productcategorybase.dto.request.ProductRequest;

/**
 * Created by govind on 15/01/2019 AD.
 */
public interface ProductServiceWrapper {

  /**
   * Update product
   *
   * @param requestId
   * @param productRequest
   * @param internalFlow3AddProduct
   * @param isActive
   * @return
   */
  String updateProduct(String requestId, String userType, ProductRequest productRequest,
      boolean internalFlow3AddProduct, boolean isActive) throws Exception;
}
