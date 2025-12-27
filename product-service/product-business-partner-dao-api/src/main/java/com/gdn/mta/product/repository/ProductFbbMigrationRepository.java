package com.gdn.mta.product.repository;

import com.gdn.mta.product.entity.ProductFbbMigration;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;

public interface ProductFbbMigrationRepository extends JpaRepository<ProductFbbMigration, String> {
  Page<ProductFbbMigration> findByStoreIdAndStatusAndMigrationType(String storeId, String status, Pageable pageable,
      String migrationType);
}
