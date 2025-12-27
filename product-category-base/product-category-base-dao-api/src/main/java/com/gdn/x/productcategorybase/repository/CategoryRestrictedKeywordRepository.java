package com.gdn.x.productcategorybase.repository;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.x.productcategorybase.entity.CategoryRestrictedKeyword;

public interface CategoryRestrictedKeywordRepository extends JpaRepository<CategoryRestrictedKeyword, String> {

  List<CategoryRestrictedKeyword> findByStoreIdAndCategoryCode(String storeId, String categoryCode);

  Page<CategoryRestrictedKeyword> findByStoreIdAndCategoryCodeAndMarkForDeleteFalseOrderByUpdatedDateDescIdDesc(
      String storeId, String categoryCode, Pageable pageable);

  CategoryRestrictedKeyword findByStoreIdAndId(String storeId, String id);

  List<CategoryRestrictedKeyword> findByStoreIdAndCategoryCodeAndRestrictedKeywordIdIn(String storeId, String categoryCode,
      List<String> keywordIds);
}
