package com.gdn.x.mta.distributiontask.service.api;

import java.util.List;

import com.gdn.x.mta.distributiontask.model.ProductMigration;

public interface ProductMigrationService {

  ProductMigration findProductMigrationByProductCodeAndStatus(String productCode, String status);

  ProductMigration saveProductMigration(ProductMigration commonImageMigration);

  /**
   * @param storeId
   * @param limit
   * @return
   */
  List<ProductMigration> findProductMigrationMigrationByStoreIdAndStatus(String storeId, String migrationType, int limit);

  /**
   * @param storeId
   * @param productCodes
   * @return
   */
  List<ProductMigration> findProductMigrationByProductCodes(String storeId, String migrationType, List<String> productCodes);
}
