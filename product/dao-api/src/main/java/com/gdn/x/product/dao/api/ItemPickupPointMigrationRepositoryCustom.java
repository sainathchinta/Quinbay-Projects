package com.gdn.x.product.dao.api;

import com.gdn.x.product.model.entity.ItemPickupPointMigration;

import java.util.List;

public interface ItemPickupPointMigrationRepositoryCustom {

  /**
   * Find list of items for migration by status and limit
   *
   * @param status
   * @param limit
   * @return
   */
  List<ItemPickupPointMigration> getItemsByStatusAndLimit(String status, int limit);

  /**
   * Update item skus by status
   *
   * @param itemSkuList
   * @param status
   */
  void updateStatusByItemSkus(List<String> itemSkuList, String status);
}
