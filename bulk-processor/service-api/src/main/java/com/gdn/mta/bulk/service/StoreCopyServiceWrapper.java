package com.gdn.mta.bulk.service;

public interface StoreCopyServiceWrapper {

  /**
   * Download target seller template
   *
   * @param storeId
   * @param sellerCode
   * @param username
   * @param requestId
   * @return
   */
  String downloadTargetSellerTemplate(String storeId, String sellerCode, String username, String requestId) throws Exception;
}
