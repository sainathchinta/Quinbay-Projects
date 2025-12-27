package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.BulkInternalProcess;

public interface BulkVendorActionService {

  /**
   * setting the final status and generate failed excel
   *
   * @param bulkInternalProcess
   * @param storeId
   * @throws Exception
   */
  void setFinalStatusAndGenerateFailedExcel(String storeId, BulkInternalProcess bulkInternalProcess)
    throws Exception;
}
