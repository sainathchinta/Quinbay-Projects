package com.gdn.mta.product.repository;

import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.gdn.mta.product.entity.ProductMigration;

public interface ProductMigrationRepository extends JpaRepository<ProductMigration, String> {

  List<ProductMigration> findByGdnProductSkuIn(List<String> gdnProductSkus);

  List<ProductMigration> findByGdnProductSkuInAndMigrationStatus(List<String> gdnProductSkus, String migrationStatus);

  List<ProductMigration> findByProductCodeIn(List<String> gdnProductSkus);

  List<ProductMigration> findByProductCodeInAndMigrationStatus(List<String> productCodes, String migrationStatus);

  Page<ProductMigration> findByMigrationStatusAndRetryCountLessThanOrderByUpdatedDateDesc(String migrationStatus, int retryCount,
      Pageable pageable);

  @Modifying(clearAutomatically = true)
  @Query("UPDATE ProductMigration pm set pm.merchantType = :merchantType, pm.merchantStatus = :merchantStatus, pm.businessPartnerName = :businessPartnerName "
          + "where pm.businessPartnerId = :businessPartnerId and pm.gdnProductSku in :gdnProductSkuList")
  void updateBusinessPartnerDetails(@Param("businessPartnerId") String businessPartnerId,
      @Param("merchantType") String merchantType, @Param("merchantStatus") String merchantStatus,
      @Param("businessPartnerName") String businessPartnerName, @Param("gdnProductSkuList") List<String> gdnProductSkuList);

  @Query(value = "select distinct p.product_code from PRD_PRODUCT_MIGRATION p where p.product_code is not null and p.MIGRATION_STATUS is null LIMIT :limit ", nativeQuery = true)
  List<String> findDistinctProductCodesForMigration(@Param("limit") int limit);

  @Modifying(clearAutomatically = true)
  @Query("UPDATE ProductMigration pm set pm.migrationStatus = :migrationStatus where pm.productCode in :productCodes")
  void updateProductMigrationStatusByProductCodes(@Param("productCodes") List<String> productCodes,
      @Param("migrationStatus") String migrationStatus);

  @Modifying(clearAutomatically = true)
  @Query("UPDATE ProductMigration pm set pm.migrationStatus = :migrationStatus where pm.gdnProductSku in :gdnProductSkuList")
  void updateProductMigrationStatusByProductSkus(@Param("gdnProductSkuList") List<String> gdnProductSkuList,
      @Param("migrationStatus") String migrationStatus);

  @Query(value = "select p.GDN_PRODUCT_SKU from PRD_PRODUCT_MIGRATION p where p.product_code is null and p.MIGRATION_STATUS is null LIMIT :limit ", nativeQuery = true)
  List<String> findProductSkusForMigration(@Param("limit") int limit);

  @Modifying(clearAutomatically = true)
  @Query("UPDATE ProductMigration pm set pm.migrationStatus = :migrationStatus, pm.updatedDate = CURRENT_TIMESTAMP, pm.updatedBy = 'ABORTED-BY-RUNDECK' where pm.migrationStatus = :oldMigrationStatus and pm.updatedDate < :updatedDate")
  void updateProductMigrationStatus(@Param("oldMigrationStatus") String oldMigrationStatus,
      @Param("migrationStatus") String migrationStatus, @Param("updatedDate") Date updatedDate);
}
