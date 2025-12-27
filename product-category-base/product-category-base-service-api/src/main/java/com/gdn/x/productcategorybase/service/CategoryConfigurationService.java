package com.gdn.x.productcategorybase.service;

import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryConfiguration;

public interface CategoryConfigurationService {

  /**
   * get the configuration for categories by categoryList
   *
   * @param storeId
   * @param categoryList
   * @return
   */
  List<CategoryConfiguration> getCategoryConfigurationsByCategoryList(String storeId, List<Category> categoryList);

  /**
   * get the configuration for category by category
   *
   * @param storeId
   * @param category
   * @return
   */
  CategoryConfiguration getCategoryConfigurationByCategory(String storeId, Category category);

  /**
   * get the configuration for category by category and MarkForDeleteFalse
   *
   * @param storeId
   * @param category
   * @return
   */
  CategoryConfiguration getCategoryConfigurationByCategoryAndMarkForDeleteFalse(String storeId, Category category);

  /**
   * save the configurations for category
   *
   * @param categoryConfigurations
   * @return
   */
  void saveCategoryConfigurations(List<CategoryConfiguration> categoryConfigurations);

  /**
   * save the configurations for category
   *
   * @param categoryConfiguration
   * @return
   */
  void saveCategoryConfiguration(CategoryConfiguration categoryConfiguration);

  /**
   * Fetch count of configured categories
   *
   * @param storeId
   * @return
   */
  Long getCategoryConfigurationCount(String storeId);

  /**
   *
   * @param storeId
   * @param date
   * @param pageable
   * @return
   */
  Page<CategoryConfiguration> getCategoryConfigurationByUpdatedDateGreaterThan(
      String storeId, Date date, Pageable pageable);

  /**
   *
   * @param storeId
   * @param configurationFilterRequest
   * @param pageable
   * @return
   */
  Page<CategoryConfiguration> getCategoryConfigurationList(String storeId,
      ConfigurationFilterRequest configurationFilterRequest, Pageable pageable);
}
