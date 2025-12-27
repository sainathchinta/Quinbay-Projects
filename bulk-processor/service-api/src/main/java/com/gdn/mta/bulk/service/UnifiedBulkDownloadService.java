package com.gdn.mta.bulk.service;

import java.util.Set;

import com.gdn.mta.bulk.entity.UnifiedBulkDownloadEvent;

public interface UnifiedBulkDownloadService {

  /**
   * Update pickup point flag
   * @param businessPartnerCode
   */
  void updatePickUpPointFlag(String businessPartnerCode);

  /**
   * Abort all pending download processes updated before date
   * @param storeId
   */
  void abortPendingDownloadProcesses(String storeId);

  /**
   * Update brand flag on consuming the brand update/create event
   * @param businessPartnerCode
   */
  void updateBrandFlag(String businessPartnerCode);

  /**
   * get Flag details and last download details for business partner
   * @param storeId
   * @param businessPartnerCode
   */
  UnifiedBulkDownloadEvent getByStoreIdAndBusinessPartnerCode(String storeId, String businessPartnerCode);

  /**
   * get Flag details and last download details for business partner
   * @param status
   * @param businessPartnerCode
   * @param pickupPointCodes
   */
  void  updateStatusAndLastDownloadTime(String status, String businessPartnerCode, Set<String> pickupPointCodes);
}