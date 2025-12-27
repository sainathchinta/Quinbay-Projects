package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.entity.BulkNeedRevisionDeletion;
import com.gdn.mta.bulk.entity.BulkNeedRevisionDeletionData;

import java.util.List;

public interface NeedRevisionDeletionService {
  /**
   * populate need revision business partner codes in blp_need_revision_deletion
   *
   * @param storeId
   * @param requestId
   */
  void populateNRBusinessPartnerCodes(String storeId, String requestId);

  /**
   * process need revision deletion
   *
   * @param storeId
   */
  void processNeedRevisionDeletion(String storeId, Integer fetchProcessCountForDeletion)
      throws ApplicationException, JsonProcessingException;

  /**
   * send notification for need revision deletion
   *
   * @param storeId
   * @param requestId
   */
  void sendNotificationForNeedRevisionDeletion(String storeId, String requestId) throws Exception;

  /**
   * Fetch NR products and save in Data table
   * @param storeId
   * @param requestId
   */
  void fetchProductsOfABusinessPartner(String storeId, String requestId) throws Exception;

  BulkNeedRevisionDeletion fetchNeedRevisionDeletionByDeletionProcessCode(String storeId, String deletionBulkProcessCode);

  List<BulkNeedRevisionDeletionData> fetchNeedRevisionDeletionDataByDeletionProcessCodeAndIds(
      String storeId, String needRevisionDeletionProcessCode, List<String> needRevisionDeletionDataIds);

  void performEligibilityCheckAndProcessDataDeletion(
      String storeId, List<BulkNeedRevisionDeletionData> bulkNeedRevisionDeletionDataList,
      String businessPartnerCode) throws ApplicationException;

  void saveBulkNeedRevisionDeletion(BulkNeedRevisionDeletion bulkNeedRevisionDeletion);

  List<BulkNeedRevisionDeletionData> saveBulkNeedRevisionDeletionData(
      List<BulkNeedRevisionDeletionData> bulkNeedRevisionDeletionData);
}
