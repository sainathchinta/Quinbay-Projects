package com.gdn.x.productcategorybase.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.x.productcategorybase.entity.RestrictedKeywordHistory;

public interface RestrictedKeywordHistoryRepository extends JpaRepository<RestrictedKeywordHistory, String> {

  Page<RestrictedKeywordHistory> findByStoreIdAndKeywordIdAndMarkForDeleteFalseOrderByCreatedDateDesc(String storeId,
      String keywordId, Pageable pageable);
}
