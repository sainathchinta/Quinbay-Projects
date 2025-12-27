package com.gdn.x.productcategorybase.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;

import com.gdn.x.productcategorybase.entity.ProductMigration;

public interface ProductMigrationRepository extends JpaRepository<ProductMigration, String> {

  ProductMigration findByProductCodeAndStatusAndMigrationType(String productCode, String status, String migrationType);

  List<ProductMigration> findProductMigrationByStoreIdAndMigrationTypeAndProductCodeIn(String storeId,
      String migrationType, List<String> productCodes);

  @Query(value = "SELECT * FROM pcc_product_migration WHERE store_id = ?1 AND status = 'PENDING' AND migration_type = ?2  limit ?3", nativeQuery = true)
  List<ProductMigration> findByStoreIdAndStatusPendingAndMigrationType(String storeId, String migrationType, int limit);
}
