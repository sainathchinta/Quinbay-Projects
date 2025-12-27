package com.gdn.mta.bulk.service;

public interface DormantSellerServiceWrapper {

  /**
   * Process pending dormant seller events
   *
   * @param storeId
   * @param requestId
   * @param username
   * @param processType
   */
  void processPendingDormantSellerEvent(String storeId, String requestId, String username,
    String processType);
}
