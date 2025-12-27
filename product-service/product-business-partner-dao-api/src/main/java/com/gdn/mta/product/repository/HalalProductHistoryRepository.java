package com.gdn.mta.product.repository;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.mta.product.entity.HalalProductHistory;

public interface HalalProductHistoryRepository extends JpaRepository<HalalProductHistory, String> {

  Page<HalalProductHistory> findByStoreIdAndProductSkuOrderByCreatedDateDesc(String storeId, String productSku,
      Pageable pageable);
}
