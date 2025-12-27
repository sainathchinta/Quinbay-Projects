package com.gdn.x.product.service.api;

import org.springframework.data.domain.Page;

import com.gdn.x.product.enums.ProductCenterActivity;
import com.gdn.x.product.rest.web.model.dto.ProductCenterHistoryResponse;

public interface ProductCenterHistoryService {

  /**
   * Store history according to old category and new category
   * @param productSku
   * @param activity
   * @param requestId
   * @param oldCategoryName
   * @param newCategoryName
   */
  void saveProductCenterHistory(String productSku, ProductCenterActivity activity, String requestId,
      String oldCategoryName, String newCategoryName);

  /**
   *
   *
   * @param storeId
   * @param productSku
   * @return
   */
  Page<ProductCenterHistoryResponse> getProductCenterHistoryByStoreIdAndProductSku(String storeId, String productSku, int page,
      int size);
}
