package com.gdn.mta.product.service;

import com.gdn.mta.product.entity.ProductLevel3FailedEntity;

import java.util.List;

public interface ProductLevel3RetryService {

  /**
   * Add entry by productSku if no entry is present
   *
   * @param storeId
   * @param productSku
   */
  void upsertProductLevel3FailureLog(String storeId, String productSku);

  /**
   * Find products by retry limit and product max count
   *
   * @param storeId
   * @param maxRetryCount
   * @param limit
   * @return
   */
  List<ProductLevel3FailedEntity> findProductsForRetryJob(String storeId, int maxRetryCount, int limit);

  /**
   * Update state to omitted by gdn skus
   *  @param storeId
   * @param omittedGdnSku
   */
  void updateCompletedOrOmittedState(String storeId, String omittedGdnSku, String status);

  /**
   * @param storeId
   * @param maxRetryCount
   * @return
   */
  List<ProductLevel3FailedEntity> findProductsForSendingMail(String storeId, int maxRetryCount);

  /**
   * @param productLevel3FailedEntityList
   */
  void updateFailedRetryProductsAfterMail(List<ProductLevel3FailedEntity> productLevel3FailedEntityList);

  /**
   * Update L3 retry entry
   *
   * @param storeId
   * @param productSku
   * @param retryCount
   * @param state
   */
  void updateRetryProduct(String storeId, String productSku, int retryCount, String state);
}
