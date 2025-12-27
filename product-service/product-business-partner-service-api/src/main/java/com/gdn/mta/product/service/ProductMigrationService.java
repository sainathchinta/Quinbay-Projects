package com.gdn.mta.product.service;

import java.util.Date;
import java.util.List;

import org.springframework.data.domain.Page;

import com.gdn.mta.product.entity.ProductMigration;

public interface ProductMigrationService {

  /**
   * API to fetch the product to migrate by product skus
   *
   * @param productSkus
   * @throws Exception
   */
  List<ProductMigration> getProductMigrationByProductSkus(List<String> productSkus) throws Exception;

  /**
   * API to fetch the product to migrate by product skus
   *
   * @param productSkus
   * @param migrationStatus
   * @throws Exception
   */
  List<ProductMigration> getProductMigrationByProductSkusWithStatus(List<String> productSkus, String migrationStatus)
      throws Exception;

  /**
   * API to fetch the product to migrate by product codes
   *
   * @param productCodes
   * @throws Exception
   */
  List<ProductMigration> findByProductCodes(List<String> productCodes) throws Exception;

  /**
   * API to fetch the product to migrate by product codes and migrationStatus
   *
   * @param productCodes
   * @param migrationStatus
   * @throws Exception
   */
  List<ProductMigration> findByProductCodesWithStatus(List<String> productCodes, String migrationStatus)
      throws Exception;

    /**
     * API to update the business partner details
     *
     * @param businessPartnerId
     * @param merchantType
     * @param merchantStatus
     * @param businessPartnerName
     * @param gdnProductSkuList
     * @throws Exception
     */
  void updateBusinessPartnerDetails(String businessPartnerId, String merchantType, String merchantStatus,
      String businessPartnerName, List<String> gdnProductSkuList);

  /**
   * API to save the product migration entity
   *
   * @param productMigration
   * @throws Exception
   */
  ProductMigration save(ProductMigration productMigration);

  /**
   * API to find distinct product codes for migration with limit
   *
   * @param limit
   * @throws Exception
   */
  List<String> findDistinctProductCodesForMigration(int limit);

  /**
   * API to find distinct products for migration where product code us null
   *
   * @param limit
   * @throws Exception
   */
  List<String> findProductForMigrationWithNullProductCode(int limit);

  /**
   * API to find products for migration where migration failed
   *
   * @param migrationStatus
   * @param batchSize
   * @param retryCount
   * @throws Exception
   */
  Page<ProductMigration> findFailedProductMigration(String migrationStatus, int batchSize, int retryCount);

  /**
   * API to update the status to given status for product codes
   *
   * @param productCodes
   * @throws Exception
   */
  void updateProductMigrationStatusByProductCodes(List<String> productCodes, String migrationStatus);


  /**
   * API to update the status to migrationStatus for product skus
   *
   * @param productSkus
   * @param migrationStatus
   * @throws Exception
   */
  void updateProductMigrationStatusByProductSkus(List<String> productSkus, String migrationStatus);

  /**
   * API to update the status from oldMigrationStatus to migrationStatus
   *
   * @param oldMigrationStatus
   * @param migrationStatus
   * @throws Exception
   */
  void updateProductMigrationStatus(String oldMigrationStatus, String migrationStatus, Date updatedDate);

}
