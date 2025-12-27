package com.gdn.x.productcategorybase.repository;

import java.util.Date;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

import com.gdn.x.productcategorybase.entity.MerchantConfigurationHistory;

public interface MerchantConfigurationHistoryRepository extends JpaRepository<MerchantConfigurationHistory, String> {

  Page<MerchantConfigurationHistory> findByStoreIdAndCreatedDateGreaterThanOrderByCreatedDateDesc(String storeId, Date createdDate,
      Pageable pageable);

  Page<MerchantConfigurationHistory> findByStoreIdAndMerchantCodeAndMarkForDeleteFalseOrderByCreatedDateDesc(
      String storeId, String merchantCode, Pageable pageable);
}
