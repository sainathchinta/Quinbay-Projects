package com.gdn.x.product.service.api;

import com.gdn.x.product.model.entity.ItemPickupPointMigration;

import java.util.List;
import java.util.Map;

public interface ItemPickupPointMigrationService {

  /**
   * Fetch items for migration by status and limit
   *
   * @param status
   * @param limit
   * @return
   */
  List<ItemPickupPointMigration> findByStatusAndLimit(String status, int limit);

  /**
   * Save list input of entity
   *
   * @param itemPickupPointMigrationList
   * @return
   */
  List<ItemPickupPointMigration> saveCollection(List<ItemPickupPointMigration> itemPickupPointMigrationList);

  /**
   * Find entry for migration by item sku
   *
   * @param itemSku
   * @return
   */
  ItemPickupPointMigration findByItemSku(String itemSku);

  /**
   * Update status by item sku list
   *
   * @param itemSkuList
   * @param status
   */
  void updateStatusByItemSku(List<String> itemSkuList, String status);

  /**
   * Update failed itemSkus with error message
   *  @param failedItemSku
   *
   */
  void updateFailedStatusByItemSku(Map<String, String> failedItemSku);

  /**
   * Upsert items by list to given state
   *
   * @param itemSkuList
   * @param state
   */
  void insertItemSkuByState(List<String> itemSkuList, String state);
}
