package com.gdn.x.productcategorybase.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.entity.MerchantConfiguration;

public interface MerchantConfigurationCustomRepository {

  /**
   * Calls database to retrieve category configurations by request filters
   *
   * @param storeId
   * @param reviewConfig
   * @param categoryName
   * @param searchKey
   * @param sortOrder
   * @param pageable
   * @return
   */
  Page<MerchantConfiguration> findByReviewConfigAndCategoryNameAndKeywordMarkForDeleteFalseOrderByCreatedDate(
      String storeId, String reviewConfig, String categoryName, String searchKey, String sortOrder, Pageable pageable);
}
