package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.BulkProcess;

public interface QrCodeFinalizeService {
  /**
   * @param storeId
   * @param bulkProcess
   */
  void setFinalStatusAndSendNotificationOnQRGeneration(String storeId, BulkProcess bulkProcess)
      throws Exception;
}
