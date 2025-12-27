package com.gdn.partners.pcu.internal.service;

import org.springframework.data.domain.Page;

import com.gdn.partners.pcu.internal.web.model.request.RejectProductWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SummaryFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.BusinessPartnerWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.DistributionProductWebResponse;

public interface QCTaskService {

  /**
   * Filter Products for QC review
   *
   * @param summaryFilterWebRequest
   * @param page
   * @param size
   * @return
   */
  Page<DistributionProductWebResponse> filterQCProductList(SummaryFilterWebRequest summaryFilterWebRequest, int page, int size)
      throws Exception;

  /**
   * Retry product activation
   *
   * @param productCode
   * @return
   * @throws Exception
   */
  void retryApproveQc(String productCode);

  /**
   * Reject product by QC
   *
   * @param rejectProductWebRequest
   * @throws Exception
   */
  void rejectProduct(RejectProductWebRequest rejectProductWebRequest) throws Exception;

  /**
   * Approve product by productId
   *
   * @param productCode
   * @param productId
   * @throws Exception
   */
  void approveProduct(String productCode, String productId) throws Exception;

  /**
   * API to fetch business partner list from PDT
   *
   * @param state
   * @param keyword
   * @param page
   * @param size
   * @return
   */
  Page<BusinessPartnerWebResponse> getBusinessPartnerList(String state, String keyword, int page, int size);
}
