package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.dto.MasterDataBulkUpdateRequest;
import com.gdn.mta.bulk.entity.BulkInternalProcess;

import java.io.IOException;

public interface MasterDataBulkUpdateService {

  /**
   * Bulk update for master product data
   * @param masterDataBulkUpdateRequest
   */
  void processBulkUpdate(MasterDataBulkUpdateRequest masterDataBulkUpdateRequest)
    throws IOException;

  /**
   *
   * @param bulkInternalProcess
   * @param storeId
   * @throws Exception
   */
  void setFinalStatusAndGenerateFailedExcel(BulkInternalProcess bulkInternalProcess, String storeId) throws Exception;

  /**
   *
   * @param storeId
   * @param updatedBy
   * @param processType
   * @param internalProcessDataRequestId
   */
  void processInternalBulkUploadEvent(String storeId, String updatedBy, String processType, String internalProcessDataRequestId);
}
