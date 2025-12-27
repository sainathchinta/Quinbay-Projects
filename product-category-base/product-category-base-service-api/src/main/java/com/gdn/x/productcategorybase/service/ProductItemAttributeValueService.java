package com.gdn.x.productcategorybase.service;

import java.util.List;

import com.gdn.x.productcategorybase.entity.ProductItem;
import com.gdn.x.productcategorybase.entity.ProductItemAttributeValue;

public interface ProductItemAttributeValueService {

  /**
   *
   * @param storeId
   * @param productItemIds
   * @return
   */
  List<ProductItemAttributeValue> getDetachedProductItemAttributeValuesByStoreIdAndProductItemIds(
      String storeId, List<String> productItemIds);

  /**
   * delete by product item attributes id's
   * @param productItemAttributeIds
   */
  void deleteByProductItemAttributeIds(List<String> productItemAttributeIds);

  /**
   * Update only the attribute value for the attribute Id
   *
   * @param storeId
   * @param attributeId
   * @param value
   * @param productItem
   */
  void updateOnlyValueForProductItemAttributeValuesByAttributeId(String storeId, String attributeId,
      String value, ProductItem productItem);
}
