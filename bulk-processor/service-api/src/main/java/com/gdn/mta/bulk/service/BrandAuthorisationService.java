package com.gdn.mta.bulk.service;

import com.gdn.mta.bulk.entity.BulkInternalProcess;

public interface BrandAuthorisationService {

  /**
   * setting the final status and generate failed excel
   * @param bulkInternalProcess
   * @param storeId
   * @throws Exception
   */
  void setFinalStatusAndGenerateFailedExcel(String storeId, BulkInternalProcess bulkInternalProcess) throws Exception;

  /**
   * setting the final status and generate failed excel for bulk brand Authorization
   * @param storeId
   * @param bulkInternalProcess
   */
  void setFinalStatusAndGenerateFailedExcelForBulkBrandAuth(String storeId, BulkInternalProcess bulkInternalProcess)
      throws Exception;
}
