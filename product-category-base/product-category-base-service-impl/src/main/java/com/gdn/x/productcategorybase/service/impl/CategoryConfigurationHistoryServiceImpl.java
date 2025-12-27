package com.gdn.x.productcategorybase.service.impl;

import org.springframework.data.domain.Pageable;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import org.apache.commons.collections.CollectionUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.stereotype.Service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.entity.CategoryConfigurationHistory;
import com.gdn.x.productcategorybase.repository.CategoryConfigurationHistoryRepository;
import com.gdn.x.productcategorybase.service.CategoryConfigurationHistoryService;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class CategoryConfigurationHistoryServiceImpl implements CategoryConfigurationHistoryService {

  @Autowired
  private CategoryConfigurationHistoryRepository categoryConfigurationHistoryRepository;

  @Override
  public void saveCategoryHistoryConfigurations(List<CategoryConfigurationHistory> categoryConfigurationHistories) {
    this.categoryConfigurationHistoryRepository.saveAll(categoryConfigurationHistories);
  }

  @Override
  public void saveCategoryHistoryConfiguration(CategoryConfigurationHistory categoryConfigurationHistory) {
    GdnPreconditions
        .checkArgument(Objects.nonNull(categoryConfigurationHistory), ErrorMessage.CATEGORY_HISTORY_ERROR.getMessage());
    this.categoryConfigurationHistoryRepository.save(categoryConfigurationHistory);
  }

  @Override
  public Page<CategoryConfigurationHistory> getCategoryConfigurationByCreatedDate(String storeId, Date createdDate,
      Pageable pageable) {
    GdnPreconditions
        .checkArgument(Objects.nonNull(createdDate), ErrorMessage.CREATED_DATE_MUST_NOT_BE_BLANK.getMessage());
    return this.categoryConfigurationHistoryRepository
        .findByStoreIdAndCreatedDateGreaterThanOrderByCreatedDateDesc(storeId, createdDate, pageable);
  }

  @Override
  public Page<CategoryConfigurationHistory> getCategoryConfigurationHistory(String storeId, String categoryCode,
      int page, int size) {
    Pageable pageable = PageRequest.of(page, size);
    return this.categoryConfigurationHistoryRepository
        .findByStoreIdAndCategoryCodeAndMarkForDeleteFalseOrderByCreatedDateDesc(storeId, categoryCode, pageable);
  }
}
