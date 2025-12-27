package com.gdn.x.productcategorybase.repository;

import com.gdn.x.productcategorybase.entity.CategoryHistory;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

public interface CategoryHistoryRepository extends JpaRepository<CategoryHistory, String> {

  Page<CategoryHistory> findByStoreIdAndCategoryCodeOrderByCreatedDateDesc(String storeId,
      String categoryCode, Pageable pageable);
}
