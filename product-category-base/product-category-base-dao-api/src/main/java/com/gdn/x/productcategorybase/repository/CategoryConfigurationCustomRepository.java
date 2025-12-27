package com.gdn.x.productcategorybase.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.entity.CategoryConfiguration;

public interface CategoryConfigurationCustomRepository {

  /**
   * Calls database to retrieve category configurations by request filters
   *
   * @param storeId
   * @param reviewConfig
   * @param categoryCode
   * @param searchKey
   * @param sortOrder
   * @param pageable
   * @return
   */
  Page<CategoryConfiguration> findByReviewConfigAndCategoryAndMarkForDeleteFalseOrderByCreatedDate(
      String storeId, String reviewConfig, String categoryCode, String searchKey, String sortOrder, Pageable pageable);
}
