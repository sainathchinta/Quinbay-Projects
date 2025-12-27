package com.gdn.partners.pcu.internal.service;

import com.gdn.partners.pcu.internal.web.model.request.RecatProcessSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.RecatProductSummaryWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.RecatProcessSummaryWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.RecatProductCountWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.RecatProductSummaryWebResponse;
import org.springframework.data.domain.Page;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;

public interface RecatService {

  /**
   *get failed products mail in excel
   *
   * @param recatRequestCode
   *
   */
  void getFailedProductsMail(String recatRequestCode);

  /**
   * Get recat process summary by request filter
   *
   * @param recatProcessSummaryRequest
   * @param page
   * @param size
   * @return
   */
  Page<RecatProcessSummaryWebResponse> getRecatSummaryByFilter(
      RecatProcessSummaryWebRequest recatProcessSummaryRequest, int page, int size)
      throws Exception;

  /**
   * Upload recategorization excel and schedule time for processing
   *
   * @param recatExcel
   * @param scheduledTime
   */
  void uploadRecatRequest(MultipartFile recatExcel, String scheduledTime) throws Exception;

  /**
   * Cancel recat request
   *
   * @param recatRequestCode
   */
  void cancelRecatRequest(String recatRequestCode);

  /**
   * get recat product summary by recat request code and filters
   * @param recatRequestCode
   * @param page
   * @param size
   * @param recatProductSummaryWebRequest
   * @return
   */
  Page<RecatProductSummaryWebResponse> getRecatProductSummary(String recatRequestCode, int page, int size,
      RecatProductSummaryWebRequest recatProductSummaryWebRequest);

   /**
   * get product counts for recat request code
   * @param recatRequestCode
   * @return
   */
  RecatProductCountWebResponse getRecatProductStatusCounts(String recatRequestCode);

}
