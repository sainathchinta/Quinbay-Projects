package com.gdn.partners.pcu.internal.service;

import com.gdn.mta.bulk.dto.BulkInternalPendingRequestResponse;
import com.gdn.mta.bulk.dto.InternalProcessPendingFilesResponse;
import com.gdn.mta.bulk.dto.RecatProcessSummaryRequest;
import com.gdn.mta.bulk.dto.RecatProcessSummaryResponse;
import com.gdn.mta.bulk.dto.RecatProductCountResponse;
import com.gdn.mta.bulk.dto.RecatProductSummaryRequest;
import com.gdn.mta.bulk.dto.RecatProductSummaryResponse;
import com.gdn.mta.bulk.dto.product.UploadProcessCount;

import org.springframework.data.domain.Page;

public interface XBulkOutboundService {

  UploadProcessCount getPendingProcessCount(String bulkProcessType, String status);

  /**
   * get failed products mail
   *
   * @param recatRequestCode
   */
  void getFailedProductsMail(String recatRequestCode);

  /**
   * Cancel request for recat
   *
   * @param recatRequestCode
   */
  void cancelRecatRequest(String recatRequestCode);

  /**
   * Fetch recat process by input summary filter
   *
   * @param recatProcessSummaryRequest
   * @param page
   * @param size
   * @return
   */
  Page<RecatProcessSummaryResponse> recatProcessFilterSummary(
      RecatProcessSummaryRequest recatProcessSummaryRequest, int page, int size);
  /**
   * Create new request in bulk for processing
   *
   * @param recatRequestCode
   * @param fileName
   * @param scheduledTime
   * @return
   */
  void uploadNewRecatRequest(String recatRequestCode, String fileName, String scheduledTime);

  /**
   * get recat product summary form x-bulk
   * @param recatRequestCode
   * @param page
   * @param size
   * @param recatProductSummaryRequest
   * @return
   */
  Page<RecatProductSummaryResponse> getRecatProductSummaryByRecatRequestCode(String recatRequestCode, int page, int size,
      RecatProductSummaryRequest recatProductSummaryRequest);

  /**
   * get product counts for recat request code
   * @param recatRequestCode
   * @return
   */
  RecatProductCountResponse getRecatProductStatusCount(String recatRequestCode);

  /**
   * Download upload template for store copy
   *
   * @param sellerCode
   * @return
   */
  String downloadStoreCopyUploadTemplate(String sellerCode);

  /**
   * Fetching pending download process
   *
   * @param sellerCode
   * @param userName
   * @param processType
   * @return
   */
  BulkInternalPendingRequestResponse getPendingProcesses(String sellerCode, String userName,
      String processType);

  /**
   * check pending files status
   *
   * @param storeId
   * @param userName
   * @param processType
   * @return
   */
  InternalProcessPendingFilesResponse checkPendingFilesForAutoAssignment(String storeId, String userName, String processType);
}
