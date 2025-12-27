package com.gdn.mta.bulk.service;

public interface StoreCopyProcessServiceWrapper {

  /**
   * @param storeId
   * @param userName
   */
  void processNewStoreCopyRequest(String storeId, String userName);

  /**
   * @param storeId
   * @param requestId
   * @param username
   */

  void processStoreCopyDataProductCreation(String storeId, String requestId, String username);

  /**
   *
   * @param storeId
   */
  void abortPendingBulkInternalProcessBefore(String storeId);

  /**
   *  @param storeId
   * @param requestId
   * @param username
   * @param processType
   */
  void processStatusUpdate(String storeId, String requestId, String username, String processType);
}
