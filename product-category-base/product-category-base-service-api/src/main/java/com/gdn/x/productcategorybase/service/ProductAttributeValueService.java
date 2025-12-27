package com.gdn.x.productcategorybase.service;

import java.util.List;
import java.util.Set;

import org.springframework.transaction.annotation.Transactional;

import com.gdn.common.base.service.GdnBaseService;
import com.gdn.x.productcategorybase.entity.AllowedAttributeValue;
import com.gdn.x.productcategorybase.entity.ProductAttributeValue;

public interface ProductAttributeValueService extends GdnBaseService<ProductAttributeValue> {

  List<ProductAttributeValue> findByStoreIdAndAllowedAttributeValue(String storeId,
      AllowedAttributeValue allowedAttributeValue);

  ProductAttributeValue findByStoreIdAndId(String storeId, String id) throws Exception;

  /**
   *
   * @param storeId
   * @param productAttributeIds
   * @return
   */
  List<ProductAttributeValue> getProductAttributeValuesByStoreIdAndProductAttributeIdsAndMarkForDeleteFalseCached(
      String storeId, Set<String> productAttributeIds);

  /**
   * get product attribute value by ids
   * @param storeId
   * @param productAttributeIds
   * @return
   */
  List<ProductAttributeValue> getProductAttributeValuesByStoreIdAndProductAttributeIds(String storeId,
      Set<String> productAttributeIds);

  List<ProductAttributeValue> saveProductAttributeValues(
      List<ProductAttributeValue> productAttributeValues);
}
