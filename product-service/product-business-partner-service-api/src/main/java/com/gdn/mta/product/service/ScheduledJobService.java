package com.gdn.mta.product.service;

import java.util.List;

import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;

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
   *
   * @param productCodes
   * @throws Exception
   */
  void migrateProducts(String storeId, List<String> productCodes) throws Exception;

  /**
   *
   * @param storeId
   * @param productCodes
   * @throws Exception
   */
  void retryFailedMigratedProducts(String storeId, List<String> productCodes);

  /**
   *
   * @param storeId
   * @throws Exception
   */
  void updateMigrationStatus(String storeId);

  /**
   * Publish Image qc Backlog products
   *  @param storeId
   * @param orderProductsBy
   * @param orderProductsIn
   */
  void publishImageQcBacklogProducts(String storeId, String orderProductsBy, String orderProductsIn) throws Exception;

  /**
   *
   * @param storeId
   * @throws Exception
   */
  void syncInReviewProducts(String storeId) throws Exception;

  /**
   * sync active products
   *
   * @param storeId
   */
  void syncActiveProducts(String storeId);

  /**
   * Fetch seller detail response
   *
   * @param businessPartnerCode
   * @return
   */
  SellerDetailResponse fetchSellerDetailResponse(String businessPartnerCode);

  /**
   * Sync pre-live products
   *
   * @param storeId
   */
  void syncPreLiveProducts(String storeId) throws Exception;

  /**
   * add delete variant retry publish event
   *
   * @param storeId
   * @param productCode
   * @param requestId
   * @param userName
   */
  void addDeleteVariantRetryPublishEvents(String storeId, String productCode, String requestId, String userName)
      throws Exception;
}