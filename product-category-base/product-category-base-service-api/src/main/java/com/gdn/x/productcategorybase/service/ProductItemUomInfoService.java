package com.gdn.x.productcategorybase.service;

import java.util.List;

import com.gdn.x.productcategorybase.entity.ProductItemUomInfo;

public interface ProductItemUomInfoService {

  /**
   * Fetch ProductItemUomInfo by storeId, sellerCode and productCode
   *
   * @param storeId
   * @param productCode
   * @return
   */
  List<ProductItemUomInfo> findByStoreIdAndProductCodeAndMarkForDeleteFalse(String storeId, String productCode);

  List<ProductItemUomInfo> findByStoreIdAndSkuCodeInAndMarkForDeleteFalse(String storeId, List<String> skuCodes);
}
