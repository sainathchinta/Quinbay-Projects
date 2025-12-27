package com.gdn.x.productcategorybase.service;

import java.util.List;

import com.gdn.x.productcategorybase.entity.ProductMigration;

public interface ProductMigrationService {

  ProductMigration findProductMigrationByProductCodeAndStatusAndMigrationType(String productCode, String status,
      String migrationType);

  ProductMigration saveProductMigration(ProductMigration commonImageMigration);

  /**
   *
   * @param storeId
   * @param migrationType
   * @param productCodes
   * @return
   */
  List<ProductMigration> findProductMigrationByProductCodes(String storeId, String migrationType,
      List<String> productCodes);

  /**
   *
   * @param storeId
   * @param migrationType
   * @param limit
   * @return
   */
  List<ProductMigration> findProductMigrationByStoreIdAndStatus(String storeId, String migrationType, int limit);

}
