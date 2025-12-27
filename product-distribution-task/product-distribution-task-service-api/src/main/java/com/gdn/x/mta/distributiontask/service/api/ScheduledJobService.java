package com.gdn.x.mta.distributiontask.service.api;

import com.gdn.x.mta.distributiontask.rest.model.request.ProductCodeListRequest;

public interface ScheduledJobService {

  /**
   *
   * @param storeId
   * @param requestId
   * @param username
   * @param dateInMillis
   * @throws Exception
   */
  void runPostLiveConfigChanges(String storeId, String requestId, String username, long dateInMillis) throws Exception;

  /**
   * @param storeId
   * @param days
   * @param batchSize
   * @param maxBatchSize
   * @param eventDeleteFlow
   * @throws Exception
   */
  void deleteProducts(String storeId, int days, int batchSize, int maxBatchSize, boolean eventDeleteFlow) throws Exception;

  /**
   *
   * @param storeId
   * @param orderProductsBy
   * @param orderProductsIn
   * @param status
   * @throws Exception
   */
  void autoApprovePendingProducts(String storeId, String orderProductsBy, String orderProductsIn, String status) throws Exception;

  /**
   *
   * @param storeId
   * @param orderProductsBy
   * @param orderProductsIn
   * @param action
   * @throws Exception
   */
  void retryProductsByAction(String storeId, String orderProductsBy, String orderProductsIn, String action) throws Exception;

  /**
   * scheduler api to add product to auto approval table on config change
   * @param storeId
   */
  void addProductsForAutoApprovalOnConfigChange(String storeId);

  /**
   * @param storeId
   * @param productCodeListRequest
   */
  void publishPendingProductMigrationRecords(String storeId, String migrationType, ProductCodeListRequest productCodeListRequest);

  /**
   * @param storeId
   * @param dataWindowInHours
   * @throws Exception
   */
  void syncNeedCorrectionProducts(String storeId, int dataWindowInHours) throws Exception;
}
