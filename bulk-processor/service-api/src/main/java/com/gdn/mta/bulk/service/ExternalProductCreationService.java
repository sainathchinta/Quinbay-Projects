package com.gdn.mta.bulk.service;

import com.gdn.common.exception.ApplicationException;
import com.gdn.mta.bulk.dto.BulkProcessExternalUploadRequest;

public interface ExternalProductCreationService {

  /**
   * Validates and prepares a bulk upload request before processing.
   *
   * @param storeId must not be blank
   * @param requestId must not be blank
   * @param request must not be null
   */
  void preProcess(String storeId, String requestId, String username,
    BulkProcessExternalUploadRequest request) throws Exception;

  /**
   * Executes the external product creation bulk upload process.
   *
   * @param bulkProcessExternalUploadRequest must not be blank
   */
  void process(BulkProcessExternalUploadRequest bulkProcessExternalUploadRequest)
    throws ApplicationException;
}
