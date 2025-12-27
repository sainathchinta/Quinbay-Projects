package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.BulkInternalProcess;

public interface BulkIPRProductService {

  /**
   *
   * @param storeId
   * @param bulkInternalProcess
   * @throws Exception
   */
  void setFinalStatusAndGenerateFailedExcelForBulkAddReviewIPRProducts(String storeId,
      BulkInternalProcess bulkInternalProcess) throws Exception;
}
