package com.gdn.mta.bulk.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.gdn.mta.bulk.dto.product.FbbL5CreateDTO;
import com.gdn.mta.bulk.entity.BulkInternalProcess;
import com.gdn.mta.bulk.entity.FbbConsignmentEventModel;
import org.springframework.data.domain.Page;

public interface FbbConsignmentService {

  /**
   *
   * @param fbbConsignmentEventModel
   */
  void preProcessFbbConsignmentCreation(FbbConsignmentEventModel fbbConsignmentEventModel);

  /**
   *
   * @param internalProcessCode
   * @param fbbL5CreateDTO
   */
  void processL5CreationEvent(String internalProcessCode, FbbL5CreateDTO fbbL5CreateDTO) throws JsonProcessingException;

  /**
   *
   * @param storeId
   * @param requestId
   * @param bulkProcess
   */
  void publishL5CreateRows(String storeId, String requestId,
    Page<BulkInternalProcess> bulkProcess);

  /**
   * process delete brand authorisation event
   * @param storeId
   * @param internalProcessDataRequestId
   */
  void processFbbL4RowEvent(String storeId, String internalProcessDataRequestId);

  /**
   *
   * @param bulkInternalProcess
   * @param storeId
   * @throws Exception
   */
  void setFinalStatusForDefaultFbbL5Creation(BulkInternalProcess bulkInternalProcess,
    String storeId)
    throws Exception;

}
