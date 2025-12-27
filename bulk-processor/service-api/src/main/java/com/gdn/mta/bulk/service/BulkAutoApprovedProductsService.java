package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.BulkInternalProcess;

import java.io.IOException;

public interface BulkAutoApprovedProductsService {

  /**
   * to set final status and generate notification
   *
   * @param storeId
   * @param bulkInternalProcess
   * @throws IOException
   */
  void setFinalStatusAndGenerateNotificationForAutoApprovedBulkAssignee(String storeId,
    BulkInternalProcess bulkInternalProcess) throws IOException;
}
