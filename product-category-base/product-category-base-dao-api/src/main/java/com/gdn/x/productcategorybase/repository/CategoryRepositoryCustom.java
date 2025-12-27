package com.gdn.x.productcategorybase.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.entity.Category;

/**
 * Created by Kesha on 09/06/16.
 */
public interface CategoryRepositoryCustom {
  /**
   * Dao to get final parent category from given categoryID
   *
   * @param categoryId
   * @return
   */
  String getFinalParentCategoryId(String categoryId);

  /**
   * Dao to get final parent category hierarchy from given categoryID
   *
   * @param categoryId
   * @return
   */
  List<String> getParentCategoryHierarchyByCategoryId(String categoryId);
  
  /**
   * Dao to get category summary from given store_id, catalog_id and display
   * @param storeId store id (mandatory)
   * @param catalogId catalog id (mandatory)
   * @param display displayable (optional)
   * @param pageable page
   * @return page of category
   */
  Page<Category> findCategorySummaryByStoreIdAndCatalogIdAndDisplay(String storeId, String catalogId, Boolean display, Pageable pageable);

  /**
   * Dao to get category summary from given store_id, name state and documentFilterType
   *
   * @param storeId
   * @param name
   * @param state
   * @param documentFilterType
   * @param pageable
   * @return page of category
   */
  Page<Category> findCategoryByStoreIdAndNameAndStateAndDocumentFilterType(String storeId, String name, String state,
      String documentFilterType, Pageable pageable);

  /**
   * Dao to get category summary from given store_id, parentCategoryId, catalogId, documentFilterType,
   * ignoreB2bExclusive and filterHalalCategory
   *
   * @param storeId
   * @param parentCategoryId
   * @param catalogId
   * @param documentFilterType
   * @param ignoreB2bExclusive
   * @param filterHalalCategory
   * @param pageable
   * @return page of category
   */
  Page<Category> findByStoreIdAndParentCategoryIdAndCatalogIdAndDocumentFilterTypeAndB2bExclusiveAndHalalCategoryAndMarkForDeleteFalse(
      String storeId, String parentCategoryId, String catalogId, String documentFilterType, boolean ignoreB2bExclusive,
      boolean filterHalalCategory, Pageable pageable);
}
