package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.WorkOrderEventModel;

public interface BulkWorkOrderServiceWrapper {


  /**
   * Process bulk work order upload
   *
   * @param workOrderEventModel must not be null
   */
  void processBulkWorkOrderUpload(WorkOrderEventModel workOrderEventModel);
}
