package com.gdn.mta.bulk.service;

import java.util.List;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.models.RestrictedKeywordProcessModel;

public interface RestrictedKeywordService {

  /**
   * process restricted keyword bulk operation
   * @param storeId
   * @param restrictedKeywordProcessModel
   */
  void processRestrictedKeywordBulkOperation(String storeId, RestrictedKeywordProcessModel restrictedKeywordProcessModel)
      throws JsonProcessingException;

  /**
   *
   * @param storeId
   * @param bulkInternalProcess
   */
  void setFinalStatusAndGenerateFailedExcel(String storeId, BulkInternalProcess bulkInternalProcess) throws Exception;

  /**
   * publish event to process restricted keyword
   * @param storeId
   * @param bulkInternalProcessList
   */
  void publishRestrictedKeywordBulkUpload(String storeId, List<BulkInternalProcess> bulkInternalProcessList, int fetchBatchSize);
}
