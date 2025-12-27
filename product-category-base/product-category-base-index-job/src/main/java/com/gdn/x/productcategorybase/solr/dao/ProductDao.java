package com.gdn.x.productcategorybase.solr.dao;

import com.gdn.x.productcategorybase.solr.model.AttributeModel;
import com.gdn.x.productcategorybase.solr.model.ProductModel;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Created by Kesha on 24/04/16.
 */
public interface ProductDao {


  /**
   * Get ALl Categories with its parent category mapping
   *
   * @return
   */
  Map<String, String> getAllCategoryMapping();

  /**
   * Get Product Category for each parent product
   *
   * @param productIdSet
   * @return
   */
  Map<String, String> getProductCategories(Set<String> productIdSet);

  /**
   * Get product Attributes from product item ids
   *
   * @param productIdSet
   * @return
   */
  Map<String, List<AttributeModel>> getProductAttributes(Set<String> productIdSet);

  /**
   * Get All Active Products
   *
   * @return
   */
  List<ProductModel> getAllProducts();

  /** API to get upc codes for each parent product
   *
   * @param productIdSet
   * @return
   */
  Map<String, Set<String>> getProductToUPCCodesMap(Set<String> productIdSet);
}
