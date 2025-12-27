package com.gdn.partners.pcu.internal.service;

import java.io.IOException;

import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import com.gdn.partners.pcu.internal.web.model.request.BulkInternalProcessSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.BulkInternalProcessSummaryWebResponse;

public interface BulkInternalProcessService {
  /**
   * Fetching store copy summary
   *
   * @param storeCopySummaryWebRequest
   * @param page
   * @param size
   * @return
   * @throws Exception
   */
  Page<BulkInternalProcessSummaryWebResponse> bulkInternalProcessSummary(BulkInternalProcessSummaryWebRequest storeCopySummaryWebRequest, int page,
      int size) throws Exception;


  /**
   * Upload bulk internal process
   *
   * @param internalProcessExcel
   * @param sellerCode
   * @param sellerName
   * @throws IOException
   */
  void uploadInternalProcess(MultipartFile internalProcessExcel, String sellerCode, String sellerName,
      String processType) throws Exception;

  /**
   * cancel new internal process request
   * @param internalProcessRequestCode
   */
  void cancelInternalBulkProcessRequest(String internalProcessRequestCode);
}


