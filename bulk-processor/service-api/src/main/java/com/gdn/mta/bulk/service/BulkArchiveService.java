package com.gdn.mta.bulk.service;

import org.apache.poi.ss.usermodel.Sheet;

import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;

public interface BulkArchiveService {

  /**
   * @param storeId
   * @param bulkArchiveSheet
   * @param bulkProcess
   */
  void addProcessDataAndUpdateProcess(String storeId, Sheet bulkArchiveSheet, BulkProcess bulkProcess);

  /**
   * Process event for bulk archive by batch of rows
   *
   * @param bulkUpdateEventModel
   * @throws Exception
   */
  void processArchiveEvent(BulkUpdateEventModel bulkUpdateEventModel) throws Exception;

  /**
   * Update final status and send notification for bulk Archive
   *
   * @param bulkProcess
   * @param storeId
   */
  void setFinalStatusAndSendNotificationOnBulkArchive(BulkProcess bulkProcess, String storeId);
}
