package com.gdn.x.mta.distributiontask.dao.api;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import com.gdn.x.mta.distributiontask.model.ProductMigration;

public interface ProductMigrationRepository extends JpaRepository<ProductMigration, String> {

  ProductMigration findByProductCodeAndStatus(String productCode, String status);

  @Query(value = "SELECT * FROM pdt_product_migration WHERE store_id = ?1 AND status = 'PENDING' AND migration_type = ?2  limit ?3", nativeQuery = true)
  List<ProductMigration> findByStoreIdAndStatus(String storeId, String migrationType, int limit);

  List<ProductMigration> findProductMigrationByStoreIdAndMigrationTypeAndProductCodeIn(String storeId,
      String migrationType, List<String> productCodes);
}
