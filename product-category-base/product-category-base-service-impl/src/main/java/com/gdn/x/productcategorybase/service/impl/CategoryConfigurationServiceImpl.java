package com.gdn.x.productcategorybase.service.impl;

import java.util.Date;
import java.util.List;
import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.entity.Category;
import com.gdn.x.productcategorybase.entity.CategoryConfiguration;
import com.gdn.x.productcategorybase.repository.CategoryConfigurationRepository;
import com.gdn.x.productcategorybase.service.CategoryConfigurationService;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class CategoryConfigurationServiceImpl implements CategoryConfigurationService {

  @Autowired
  private CategoryConfigurationRepository categoryConfigurationRepository;

  @Override
  public List<CategoryConfiguration> getCategoryConfigurationsByCategoryList(String storeId,
      List<Category> categoryList) {
    GdnPreconditions
        .checkArgument(CollectionUtils.isNotEmpty(categoryList), ErrorMessage.CATEGORY_MUST_NOT_BE_BLANK.getMessage());
    return this.categoryConfigurationRepository.findByStoreIdAndCategoryIn(storeId, categoryList);
  }

  @Override
  public CategoryConfiguration getCategoryConfigurationByCategory(String storeId, Category category) {
    GdnPreconditions.checkArgument(Objects.nonNull(category), ErrorMessage.CATEGORY_MUST_NOT_BE_BLANK.getMessage());
    return this.categoryConfigurationRepository.findByStoreIdAndCategory(storeId, category);
  }

  @Override
  public CategoryConfiguration getCategoryConfigurationByCategoryAndMarkForDeleteFalse(String storeId,
      Category category) {
    GdnPreconditions.checkArgument(Objects.nonNull(category), ErrorMessage.CATEGORY_MUST_NOT_BE_BLANK.getMessage());
    return this.categoryConfigurationRepository.findByStoreIdAndCategoryAndMarkForDeleteFalse(storeId, category);
  }

  @Override
  public void saveCategoryConfigurations(List<CategoryConfiguration> categoryConfigurations) {
    this.categoryConfigurationRepository.saveAll(categoryConfigurations);
  }

  @Override
  public void saveCategoryConfiguration(CategoryConfiguration categoryConfiguration) {
    this.categoryConfigurationRepository.save(categoryConfiguration);
  }

  @Override
  public Long getCategoryConfigurationCount(String storeId) {
    return this.categoryConfigurationRepository.countByStoreIdAndMarkForDeleteFalse(storeId);
  }

  @Override
  public Page<CategoryConfiguration> getCategoryConfigurationByUpdatedDateGreaterThan(
      String storeId, Date date, Pageable pageable) {
    GdnPreconditions.checkArgument(Objects.nonNull(storeId), ErrorMessage.STORE_ID_MUST_NOT_BE_BLANK.getMessage());
    GdnPreconditions.checkArgument(Objects.nonNull(date), ErrorMessage.UPDATED_DATE_MUST_NOT_BE_BLANK.getMessage());
    return categoryConfigurationRepository.findByStoreIdAndUpdatedDateGreaterThan(storeId, date, pageable);
  }

  @Override
  public Page<CategoryConfiguration> getCategoryConfigurationList(String storeId,
      ConfigurationFilterRequest configurationFilterRequest, Pageable pageable) {
    return this.categoryConfigurationRepository
        .findByReviewConfigAndCategoryAndMarkForDeleteFalseOrderByCreatedDate(storeId,
            configurationFilterRequest.getReviewConfig(), configurationFilterRequest.getCategoryCode(),
            configurationFilterRequest.getSearchKey(), configurationFilterRequest.getSortOrder(), pageable);
  }
}
