package com.gdn.x.productcategorybase.repository;

import java.util.Date;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.x.productcategorybase.entity.CategoryConfigurationHistory;

public interface CategoryConfigurationHistoryRepository extends JpaRepository<CategoryConfigurationHistory, String> {

  Page<CategoryConfigurationHistory> findByStoreIdAndCategoryCodeAndMarkForDeleteFalseOrderByCreatedDateDesc(
      String storeId, String categoryCode, Pageable pageable);

  Page<CategoryConfigurationHistory> findByStoreIdAndCreatedDateGreaterThanOrderByCreatedDateDesc(String storeId,
      Date createdDate, Pageable pageable);
}
