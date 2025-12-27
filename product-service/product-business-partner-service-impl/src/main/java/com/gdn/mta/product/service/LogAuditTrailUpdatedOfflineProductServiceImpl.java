package com.gdn.mta.product.service;

import com.gdn.common.base.GdnPreconditions;
import com.gdn.mta.product.entity.LogAuditTrailUpdatedOfflineProduct;
import com.gdn.mta.product.repository.LogAuditTrailUpdatedOfflineProductRepository;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
public class LogAuditTrailUpdatedOfflineProductServiceImpl
    implements LogAuditTrailUpdatedOfflineProductService {

  public static final String PICKUP_POINT_CODE_MUST_NOT_BE_BLANK =
      "Pickup point code must not be blank";
  public static final String ITEM_SKU_MUST_NOT_BE_EMPTY = "Item SKU must not be empty";

  @Autowired
  private LogAuditTrailUpdatedOfflineProductRepository repository;

  @Override
  public Page<LogAuditTrailUpdatedOfflineProduct> getOfflineProductAuditLogsByItemSkuAndPickupPointCode(
      String storeId, String itemSku, String pickupPointCode, Pageable pageable) {
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(itemSku), ITEM_SKU_MUST_NOT_BE_EMPTY);
    GdnPreconditions.checkArgument(StringUtils.isNotBlank(pickupPointCode),
        PICKUP_POINT_CODE_MUST_NOT_BE_BLANK);
    return this.repository.findByItemSkuAndPickupPointCodeAndStoreIdOrderByAccessTimeDesc(itemSku,
        pickupPointCode, storeId, pageable);
  }

  @Override
  public void save(LogAuditTrailUpdatedOfflineProduct logAuditTrailUpdatedOfflineProduct) {
    this.repository.save(logAuditTrailUpdatedOfflineProduct);
  }

  @Override
  public void save(List<LogAuditTrailUpdatedOfflineProduct> logAuditTrailUpdatedOfflineProducts) {
    this.repository.saveAll(logAuditTrailUpdatedOfflineProducts);
  }
}
