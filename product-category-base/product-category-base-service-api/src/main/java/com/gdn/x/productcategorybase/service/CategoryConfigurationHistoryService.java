package com.gdn.x.productcategorybase.service;

import org.springframework.data.domain.Pageable;
import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;

import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationHistoryResponse;
import com.gdn.x.productcategorybase.entity.CategoryConfigurationHistory;
import org.springframework.data.domain.Page;

public interface CategoryConfigurationHistoryService {

  /**
   * save the configuration history for categories
   *
   * @param categoryConfigurationHistories
   * @return
   */
  void saveCategoryHistoryConfigurations(List<CategoryConfigurationHistory> categoryConfigurationHistories);

  /**
   * save the configuration history for category
   *
   * @param categoryConfigurationHistory
   * @return
   */
  void saveCategoryHistoryConfiguration(CategoryConfigurationHistory categoryConfigurationHistory);

  /**
   * Fetch category history by createdDate
   *
   * @param storeId
   * @param createdDate
   * @return
   */
  Page<CategoryConfigurationHistory> getCategoryConfigurationByCreatedDate(String storeId, Date createdDate,
      Pageable pageable);

  /**
   * Fetch category configuration history
   *
   * @param storeId
   * @param categoryCode
   * @param page
   * @param size
   * @return
   */
  Page<CategoryConfigurationHistory> getCategoryConfigurationHistory(String storeId, String categoryCode,
      int page, int size);
}
