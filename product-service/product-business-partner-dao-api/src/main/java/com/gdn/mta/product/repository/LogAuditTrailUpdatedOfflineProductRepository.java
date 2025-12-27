package com.gdn.mta.product.repository;

import com.gdn.mta.product.entity.LogAuditTrailUpdatedOfflineProduct;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

@Repository
public interface LogAuditTrailUpdatedOfflineProductRepository
    extends JpaRepository<LogAuditTrailUpdatedOfflineProduct, String> {

  Page<LogAuditTrailUpdatedOfflineProduct> findByItemSkuAndPickupPointCodeAndStoreIdOrderByAccessTimeDesc(
      String itemSku, String pickupPointCode, String storeId, Pageable pageable);

}
