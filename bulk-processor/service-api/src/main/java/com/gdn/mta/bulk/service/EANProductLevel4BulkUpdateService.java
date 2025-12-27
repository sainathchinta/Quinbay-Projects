package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.dto.BulkUpdateProcessDTO;
import com.gdn.mta.bulk.entity.BulkProcess;
import com.gdn.mta.bulk.entity.BulkUpdateEventModel;
import com.gdn.mta.bulk.entity.BulkUpdateQueue;

import java.util.List;
import java.util.Map;

public interface EANProductLevel4BulkUpdateService {

  /**
   * Pre processing of Bulk Update EAN Request before sending data to Queue
   *
   * @param storeId
   * @param bulkUpdateProcessDTO
   * @throws Exception
   */
  void preProcessBulkUpdateEAN(String storeId, String requestId, BulkUpdateProcessDTO bulkUpdateProcessDTO)
      throws Exception;

  /**
   * Post Processing of Bulk EAN Update Request after retrieving data from Queue
   *
   * @param bulkUpdateQueue
   * @throws Exception
   */
  void processBulkEANUpdate(BulkUpdateQueue bulkUpdateQueue) throws Exception;

  /**
   * Api to save the data for update EAN
   *
   * @param bulkProcess
   * @param productDataFromExcel
   * @return
   */
  void saveBulkProcessDataForEANUpdate(BulkProcess bulkProcess, List<Map<String, String>> productDataFromExcel)
      throws Exception;

  void processBulkEANUpdateItem(BulkUpdateEventModel bulkUpdateEventModel) throws Exception;

  void setFinalStatusAndNotificationOnEANUpdate(String storeId, BulkProcess bulkProcess) throws Exception;
}
