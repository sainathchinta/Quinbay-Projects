package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.BulkInternalProcess;

public interface BulkRebateUpdateService {

  /**
   * Update Final Status and Generate Error File
   *
   * @param storeId
   * @param bulkInternalProcess
   */
  void setFinalStatusAndGenerateErrorFileForBulkRebateUpdate(String storeId, BulkInternalProcess bulkInternalProcess)
      throws Exception;

  /**
   * Send EMAIL
   * @param templateId
   * @param subject
   * @param fileName
   * @param username
   * @param errorFilePath
   * @param rebateListingURL
   * @param maxLimit
   * @throws Exception
   */
  void sendEmailNotification(String templateId, String subject, String fileName, String username, String errorFilePath, String rebateListingURL, int maxLimit) throws Exception;


}
