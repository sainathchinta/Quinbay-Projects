package com.gdn.mta.product.service;

public interface ProductFbbMigrationService {

  /**
   *
   * @param storeId store id
   * @param migrationType migration type
   */
  void migrateFbbProducts(String storeId, String migrationType);
}
