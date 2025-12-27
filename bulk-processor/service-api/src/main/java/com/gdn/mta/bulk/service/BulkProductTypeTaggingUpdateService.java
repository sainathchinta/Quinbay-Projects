package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.BulkInternalProcess;

public interface BulkProductTypeTaggingUpdateService {
  /**
   * To set final status and generate notification
   *
   * @param storeId
   * @param bulkInternalProcess
   * @throws Exception
   */
  void setFinalStatusAndGenerateFailedExcelForBulkProductTypeUpdate(String storeId,
    BulkInternalProcess bulkInternalProcess)
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
   * @param errorFilePath
   * @param fileName
   * @throws Exception
   */
  void sendEmailNotificationForBulkProductTypeUpdate(String templateId, String subject, String requestCode,
    String username, String totalCount, String successCount, String errorFilePath, String fileName) throws Exception;
}
