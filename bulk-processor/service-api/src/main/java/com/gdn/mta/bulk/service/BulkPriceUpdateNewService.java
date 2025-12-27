package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.BulkInternalProcess;

public interface BulkPriceUpdateNewService {

  /**
   * Send mail
   *
   * @param templateId
   * @param subject
   * @param fileName
   * @param username
   * @param totalCount
   * @param failureCount
   * @param successCount
   * @param maxLimit
   * @throws Exception
   */
  void sendEmailNotification(String templateId, String subject, String fileName, String username, int totalCount,
      int failureCount, int successCount, int maxLimit) throws Exception;

  /**
   * To set final status and generate notification
   *
   * @param storeId
   * @param bulkInternalProcess
   * @throws Exception
   */
  void setFinalStatusAndGenerateErrorFileForBulkNewPriceUpdate(String storeId,
      BulkInternalProcess bulkInternalProcess) throws Exception;
}
