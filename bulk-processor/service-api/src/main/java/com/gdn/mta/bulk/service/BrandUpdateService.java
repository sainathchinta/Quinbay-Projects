package com.gdn.mta.bulk.service;

import java.util.List;

import com.gdn.mta.bulk.entity.BulkInternalProcess;

public interface BrandUpdateService {

  /**
   * Publish brand update for each product
   * @param storeId
   * @param bulkInternalProcesses
   * @param fetchBatchSize
   */
  void publishBrandUpdateEvent(String storeId, List<BulkInternalProcess> bulkInternalProcesses, int fetchBatchSize);

  /**
   * process brand update event
   * @param storeId
   * @param processType
   * @param internalProcessDataRequestId
   */
  void processBrandUpdateEvent(String storeId, String processType, String internalProcessDataRequestId);

}
