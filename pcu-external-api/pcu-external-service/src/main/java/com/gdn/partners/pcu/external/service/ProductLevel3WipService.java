package com.gdn.partners.pcu.external.service;

import com.gdn.partners.pcu.external.web.model.response.ProductLevel3WipDetailWebResponse;

public interface ProductLevel3WipService {

  /**
   * API to get the product detail by product sku
   *
   * @param productSku
   * @param isActive
   * @return
   */
  ProductLevel3WipDetailWebResponse findProductDetailByProductSku(String productSku, boolean isActive);
}
