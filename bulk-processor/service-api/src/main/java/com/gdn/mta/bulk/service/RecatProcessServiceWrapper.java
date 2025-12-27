package com.gdn.mta.bulk.service;


import com.gdn.mta.bulk.dto.RecatProcessSummaryRequest;
import com.gdn.mta.bulk.dto.RecatProcessSummaryResponse;
import com.gdn.mta.bulk.dto.RecatProductCountResponse;
import com.gdn.mta.bulk.entity.ProductCodeAndCategoryCodeListEvent;
import org.springframework.data.domain.Page;

public interface RecatProcessServiceWrapper {

  /**
   * Process new recat process
   *
   * @param storeId
   */
  void processNewRecatProcess(String storeId) ;

  /**
   * Get Failed Products Mail
   *
   * @param storeId
   * @param recatRequestCode
   * @param username
   * @param requestId
   */

  void getFailedProductsMail(String storeId, String recatRequestCode, String username,String requestId) throws Exception ;

  /**
   * Upload new request for recat
   * @param storeId
   * @param recatRequestCode
   * @param fileName
   * @param scheduledTime
   */
  void uploadNewRecatRequest(String storeId, String recatRequestCode, String fileName,
      String scheduledTime);

  /**
   * get the count of success, failed, in-progress, total products for a recat request
   * @param storeId
   * @param recatRequestCode
   * @return
   */
  RecatProductCountResponse getProductCountsByRecatRequestCode(String storeId, String recatRequestCode);

  /**
   * Publish pending products for recat
   *
   * @param storeId
   */
  void publishPendingProducts(String storeId);

  /**
   * Perform recat of products
   *
   * @param productCodeAndCategoryCodeListEvent
   */
  void updateProductCategory(ProductCodeAndCategoryCodeListEvent productCodeAndCategoryCodeListEvent);

  /**
   * Fetch recat request summary
   *
   * @param storeId
   * @param recatProcessSummaryRequest
   * @param page
   * @param size
   * @return
   */
  Page<RecatProcessSummaryResponse> getRecatProcessSummary(String storeId,
      RecatProcessSummaryRequest recatProcessSummaryRequest, int page, int size);
  /**
   * Update product status final status
   *
   * @param storeId
   */
  void updateRecatProcessFinalStatus(String storeId);

  /**
   * Cancel recat request code
   *  @param storeId
   * @param recatRequestCode
   * @param forceUpdate
   * @param userName
   */
  void cancelRecatRequest(String storeId, String recatRequestCode, boolean forceUpdate, String userName);

}
