package com.gdn.x.productcategorybase.service;

import java.util.List;
import java.util.Map;

import com.gdn.common.base.service.GdnBaseService;
import com.gdn.x.productcategorybase.entity.ProductAttribute;

public interface ProductAttributeService extends GdnBaseService<ProductAttribute> {

  ProductAttribute findByStoreIdAndId(String storeId, String id);

  /**
   *
   * @param storeId
   * @param productId
   * @return
   */
  List<ProductAttribute> getProductAttributesByStoreIdAndProductIdCached(String storeId, String productId);


  /**
   * get product attributes by product id
   * @param storeId
   * @param productId
   * @return
   */
  List<ProductAttribute> getProductAttributesByStoreIdAndProductId(String storeId, String productId);

  /**
   * Update Extracted Value flag on Content Edit
   * @param storeId storeId
   * @param id productId
   */
  void updateExtractedValueInProductAttribute(String storeId, String id);

  /**
   * get product attribute values
   *
   * @param storeId
   * @param productAttributeMap
   * @return
   */
  Map<String, ProductAttribute> getProductAttributeValues(String storeId,
      Map<String, ProductAttribute> productAttributeMap);

  List<ProductAttribute> saveMissedProductAttributes(List<ProductAttribute> productAttributes);

  /**
   * delete by product attributes id's
   *
   * @param productAttributeIds
   */
  void deleteByProductAttributeIds(List<String> productAttributeIds);
}
