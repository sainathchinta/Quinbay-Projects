package com.gdn.x.mta.distributiontask.dao.api;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import com.gdn.x.mta.distributiontask.model.AutoQcConfigChange;

public interface AutoQcConfigChangeRepository extends JpaRepository<AutoQcConfigChange, String> {

  @Query(value = "SELECT * FROM pdt_auto_qc_config_change WHERE store_id = ?1  AND status = ?2 AND mark_for_delete=false ORDER BY created_date LIMIT ?3", nativeQuery = true)
  List<AutoQcConfigChange> findByStoreIdAndStatus(String storeId, String status, int limit);

  AutoQcConfigChange findBySellerCodeAndC1CategoryCodeAndStatus(String sellerCode, String c1CategoryCode,
      String status);

}
