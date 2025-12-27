package com.gdn.mta.product.service;

import java.util.List;

import com.gda.mta.product.dto.ProductAndL5MigrationRequest;
import com.gdn.mta.product.entity.ProductMigration;

public interface ProductMigrationWrapperService {

  /**
   * API to update the new product code in x-product
   *
   * @param productSkus
   * @throws Exception
   */
  void updateMigratedProductCode(List<String> productSkus) throws Exception;

  /**
   * API to migrate the product by product code
   *
   * @param productCode
   * @param checkStatus
   * @throws Exception
   */
  Integer migrateProductByProductCode(String productCode, boolean checkStatus) throws Exception;

  /**
   * API to migrate the product where product code is null
   *
   * @param productSkus
   * @throws Exception
   */
  Integer migrateProductByProductSkus(List<String> productSkus) throws Exception;

  /**
   * API to retry the product migration for failed product by product code
   *
   * @param productCodes
   * @throws Exception
   */
  void retryFailedMigratedProductsByProductCodes(List<String> productCodes) throws Exception;

  /**
   * API to retry the product migration for failed products
   *
   * @param batchSize
   * @param retryCount
   * @throws Exception
   */
  void retryFailedMigratedProducts(int batchSize, int retryCount) throws Exception;

  /**
   * API to migrate Product And L5 Details By ProductSku
   *
   * @param storeId not null
   * @param productAndL5MigrationRequest  migration request not null
   * @param username updated by (nullable)
   */
  void migrateProductAndL5DetailsByProductSku(String storeId,
    ProductAndL5MigrationRequest productAndL5MigrationRequest, String username) throws Exception;
}
