package com.gdn.partners.pcu.internal.service;

import org.springframework.web.multipart.MultipartFile;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.web.model.request.ApproveBrandWipWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.BrandApprovalResponse;
import com.gdn.partners.pcu.internal.web.model.response.BrandRejectionWebResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistoryResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistorySummaryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipSummaryRequest;

public interface BrandWipService {

  /**
   * Fetches brand wip detail using brand request code
   *
   * @param brandRequestCode
   * @return
   */
  GdnRestSingleResponse<BrandWipResponse> getBrandWipDetail(String brandRequestCode);

  /**
   *
   * @param brandWipHistorySummaryRequest
   * @param page
   * @param size
   * @return
   */
  GdnRestListResponse<BrandWipHistoryResponse> getBrandWipHistory(
      BrandWipHistorySummaryRequest brandWipHistorySummaryRequest, int page, int size);

  /**
   * Fetches list of brands accoording to brand name and state
   * @param brandWipSummaryRequest
   * @return
   */
  GdnRestListResponse<BrandWipResponse> getBrandWipList(BrandWipSummaryRequest brandWipSummaryRequest, int page,
      int size);

  /**
   * To approve a brandWip
   *
   * @param approveBrandWipWebRequest
   * @param brandLogo
   * @param profileBanner
   * @return
   */
  BrandApprovalResponse approveBrand(ApproveBrandWipWebRequest approveBrandWipWebRequest, MultipartFile brandLogo,
      MultipartFile profileBanner) throws Exception;


  /**
   * Get brand rejection reason by brand request code
   *
   * @param brandRequestCode
   * @return
   */
  BrandRejectionWebResponse getBrandRejectionReasonByBrandRequestCode(String brandRequestCode);

  /**
   * Update Brand Wip
   *
   * @param approveBrandWipWebRequest
   * @param brandLogo
   * @param multipartFile
   * @return
   * @throws Exception
   */
  GdnBaseRestResponse update(ApproveBrandWipWebRequest approveBrandWipWebRequest, MultipartFile brandLogo,
      MultipartFile multipartFile) throws Exception;

  /**
   * Reject a brand
   *
   * @param brandRejectRequest
   * @return
   */
  String rejectBrand(BrandRejectRequest brandRejectRequest);
}
