package com.gdn.mta.product.service;

import java.util.List;

import com.gda.mta.product.dto.ProductL3RetryListRequest;
import com.gda.mta.product.dto.SuspensionProductRequest;
import com.gda.mta.product.dto.response.SuspensionProductResponse;

public interface ProductLevel3Wrapper {


  /**
   * Archive of items which are OOS for certain number of months
   *
   * @param storeId
   * @param bulkArchiveSize
   * @param monthsToArchiveFor
   * @param archiveWithoutSendingMail
   * @throws Exception
   */
  void bulkArchiveOldOosProducts(String storeId, long bulkArchiveSize, int monthsToArchiveFor,
      Boolean archiveWithoutSendingMail) throws Exception;


  /**
   * Do the suspension for products uploaded by bulk excel
   *
   * @param storeId
   * @param username
   * @param suspensionProductRequestList
   */
  List<SuspensionProductResponse> doBulkProductSuspension(String storeId, String username,
      List<SuspensionProductRequest> suspensionProductRequestList);

  /**
   * Retry L3 and L4 creation with products from logger
   *
   * @param storeId
   * @param requestId
   * @param username
   * @param clientId
   * @param channelId
   */
  void retryL3CreationJob(String storeId, String requestId, String username, String clientId, String channelId);

  /**
   * Update L3 retry requests
   *
   * @param storeId
   * @param requestId
   * @param username
   * @param productL3RetryListRequest
   */
  void overrideL3Retry(String storeId, String requestId, String username, ProductL3RetryListRequest productL3RetryListRequest);
}
