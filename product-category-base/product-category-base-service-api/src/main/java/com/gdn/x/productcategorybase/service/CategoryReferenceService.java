package com.gdn.x.productcategorybase.service;

import java.util.List;

import com.gdn.x.productcategorybase.domain.event.model.ProductSalesCategoryMapping;

public interface CategoryReferenceService {

  /**
   * Get sales category mapping reference by master category id
   *
   * @param oldCategoryId
   * @param newCategoryId
   * @param ignoreHalalCategories
   * @return
   */
  ProductSalesCategoryMapping getSalesCategoryReferenceByMasterCategory(String oldCategoryId, String newCategoryId,
      boolean ignoreHalalCategories);

  /**
   *
   * Get sales category id's by master category id
   *
   * @param storeId
   * @param categoryId
   * @return
   */
  List<String> getSalesCategoryIdByMasterCategoryId(String storeId, String categoryId);

  /**
   *
   * Get master category id's by sales category id
   *
   * @param storeId
   * @param categoryId
   * @return
   */
  List<String> getMasterCategoryIdBySalesCategoryId(String storeId, String categoryId);
}
