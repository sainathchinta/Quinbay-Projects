package com.gdn.mta.bulk.service;

import java.io.IOException;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.mta.bulk.dto.BulkVendorProductAssignRequest;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.models.download.VendorAutoAssignmentRequest;

public interface VendorProductBulkAssignService {

  /**
   * process bulk vendor product assignment
   *
   * @param masterDataBulkUpdateRequest
   * @throws Exception
   */
  void processBulkUpdate(BulkVendorProductAssignRequest masterDataBulkUpdateRequest) throws Exception;

  /**
   * Process vendor bulk assignment
   *
   * @param storeId
   * @param updatedBy
   * @param processType
   * @param internalProcessDataRequestId
   */
  void processVendorBulkAssignment(String storeId, String updatedBy, String processType,
      String internalProcessDataRequestId);

  /**
   * save vendor auto assignment request in internalprocess table
   *
   * @param vendorAutoAssignmentRequest
   */
  void processVendorAutoAssignment(VendorAutoAssignmentRequest vendorAutoAssignmentRequest)
      throws JsonProcessingException;

  /**
   * Send mail for vendor failed bulk assignment
   *
   * @param storeId
   * @param bulkInternalProcess
   */
  void setFinalStatusAndGenerateFailedExcel(String storeId, BulkInternalProcess bulkInternalProcess) throws IOException;
}
