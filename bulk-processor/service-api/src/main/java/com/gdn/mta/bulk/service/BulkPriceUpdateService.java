package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.BulkInternalProcess;

public interface BulkPriceUpdateService {
  /**
   * To set final status and generate notification
   *
   * @param storeId
   * @param bulkInternalProcess
   * @throws Exception
   */
  void setFinalStatusAndGenerateFailedExcelForBulkPriceUpdate(String storeId, BulkInternalProcess bulkInternalProcess)
      throws Exception;

  /**
   * Send Email to user
   *
   * @param templateId
   * @param subject
   * @param requestCode
   * @param username
   * @param totalCount
   * @param successCount
   * @throws Exception
   */
  void sendEmailNotification(String templateId, String subject, String requestCode, String username, String totalCount,
      String successCount) throws Exception;
}
