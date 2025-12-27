package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.BulkInternalProcess;

public interface BulkSkuLevelRebateService {

  /**
   * Send EMAIL
   *
   * @param templateId
   * @param subject
   * @param fileName
   * @param username
   * @param maxLimit
   * @throws Exception
   */
  void sendEmailNotification(String templateId, String subject, String fileName, String username, int maxLimit)
      throws Exception;

  /**
   * To set final status and generate notification
   *
   * @param storeId
   * @param bulkInternalProcess
   * @throws Exception
   */
  void setFinalStatusAndGenerateErrorFileForBulkSkuLevelRebateUpdate(String storeId,
      BulkInternalProcess bulkInternalProcess) throws Exception;
}
