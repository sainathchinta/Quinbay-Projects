package com.gdn.mta.product.service;

import com.gdn.mta.product.entity.LogAuditTrailUpdatedOfflineProduct;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import java.util.List;

public interface LogAuditTrailUpdatedOfflineProductService {

  Page<LogAuditTrailUpdatedOfflineProduct> getOfflineProductAuditLogsByItemSkuAndPickupPointCode(
      String storeId, String itemSku, String pickupPointCode, Pageable pageable);

  void save(LogAuditTrailUpdatedOfflineProduct logAuditTrailUpdatedOfflineProduct);

  void save(List<LogAuditTrailUpdatedOfflineProduct> logAuditTrailUpdatedOfflineProducts);
}
