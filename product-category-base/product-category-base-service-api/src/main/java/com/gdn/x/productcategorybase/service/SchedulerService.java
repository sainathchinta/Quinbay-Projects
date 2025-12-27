package com.gdn.x.productcategorybase.service;

import com.gdn.x.productcategorybase.dto.request.ProductCodesRequest;
import com.gdn.x.productcategorybase.dto.request.ProductMigrationRequest;

public interface SchedulerService {

  /**
   * publish product attribute extraction backfilling event in batches
   * @param storeId
   * @param username
   * @param status
   */
  void processPublishingProductAttributeExtractionBackFilling(String storeId, String username, String status);

  /**
   * publish products to backfil common image flag
   *
   * @param storeId
   * @param migrationType
   * @param productCodesRequest
   */
  void publishPendingProductMigrationRecords(String storeId, String migrationType,
      ProductCodesRequest productCodesRequest);

  /**
   * scheudler to delete archived product data
   * @param storeId
   */
  void deleteArchiveProductData(String storeId) throws Exception;

 /**
   * scheduler to published rejected products for deletion
   * @param storeId
   */
  void publishRejectedProductForDeletion(String storeId);

  /**
   * Scheduler to publish activation of brand authorisation
   * @param storeId Store id
   * @param daysThreshold Days threshold
   */
  void activateUpcomingBrandAuthorisation(String storeId, int daysThreshold);

  /**
   * Update Product Migration Status
   * @param storeId Store id
   * @param requestId non null
   * @param productMigrationRequest migration update request
   */
  void updateProductMigrationStatus(String storeId, String requestId, ProductMigrationRequest productMigrationRequest);

  /**
   * Scheduler to send near expiry mails
   *
   * @param storeId storeId
   */
  void sendNearExpiryMails(String storeId);
}
