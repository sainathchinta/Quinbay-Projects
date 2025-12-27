package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.BulkInternalProcess;

public interface BulkMasterSkuReviewService {

  /**
   * to set status and send email for bulk assignee
   *
   * @param storeId
   * @param bulkInternalProcess
   * @throws Exception
   */
  void setFinalStatusAndGenerateFailedExcelForBulkAssignee(String storeId,
    BulkInternalProcess bulkInternalProcess) throws Exception;

  /**
   * set final status and generate excel file for bulk master sku review
   *
   * @param storeId             storeId
   * @param bulkInternalProcess bulkInternalProcess
   * @throws Exception Exception
   */
  void setFinalStatusAndGenerateFailedExcelForBulkMasterSkuReview(String storeId,
      BulkInternalProcess bulkInternalProcess) throws Exception;
}
