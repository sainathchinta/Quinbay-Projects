package com.gdn.x.productcategorybase.service.brand;

import com.gdn.x.productcategorybase.dto.BrandCreationDTO;
import com.gdn.x.productcategorybase.dto.BrandInReviewResponse;
import org.springframework.data.domain.Page;

import com.gdn.x.productcategorybase.dto.brand.BrandRejectionInfoResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandApproveRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandServiceWrapperResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistoryResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistorySummaryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipSummaryRequest;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;

import java.util.List;

public interface BrandWipService {


  /**
   * Create a brand with given reqeust
   *
   * @param storeId
   * @param brandWip
   * @return
   * @throws Exception
   */
  BrandCreationDTO create(String storeId, BrandWip brandWip) throws Exception;

  /**
   * Fetches the brand wip details for corresponding parameters
   *
   * @param storeId
   * @param brandRequestCode
   * @return
   */
  BrandWipResponse getBrandWipDetail(String storeId, String brandRequestCode);


  /**
   * Get brand rejection reason by brand request code
   *
   * @param storeId
   * @param brandRequestCode
   * @return
   */
  BrandRejectionInfoResponse getBrandRejectionInfoResponse(String storeId, String brandRequestCode);

  /**
   * Delete brandWip
   * @param brandWip
   */
  void deleteBrandWip(BrandWip brandWip);

  /**
   * Get brandWip by storeId and brandCode
   * @param storeId
   * @param brandCode
   * @return
   */
  BrandWip getBrandWipByStoreIdAndBrandCode(String storeId, String brandCode);


  /**
   * Fetches brand wip history
   * @param brandWipHistorySummaryRequest
   * @param page
   * @param size
   * @return
   */
  Page<BrandWipHistoryResponse> getBrandWipHistory(BrandWipHistorySummaryRequest brandWipHistorySummaryRequest,
      int page, int size);

  /**
   * Fetches brand wips  according to state and brand name
   *
   * @param brandWipSummaryRequest
   * @param page
   * @param size
   * @return
   */
  Page<BrandWipResponse> getBrandWipList(BrandWipSummaryRequest brandWipSummaryRequest, int page,
      int size);

  /**
   * Updates brand wip of given brand request code
   *
   * @param storeId
   * @param brandApproveRequest
   */
  BrandWip update(String storeId, BrandApproveRequest brandApproveRequest)  throws Exception;

  /**
   *  Approve brand
   *
   * @param brandApproveRequest
   * @return
   */
  BrandServiceWrapperResponse approveBrand(BrandApproveRequest brandApproveRequest) throws Exception;

  /**
   * Reject brand
   *
   * @param brandRejectRequest
   * @return
   */
  BrandWipResponse rejectBrand(BrandRejectRequest brandRejectRequest) throws Exception;

  /**
   * @param storeId
   * @param brandCode
   * @return
   */
  BrandWipResponse getBrandWipDetailByBrandCode(String storeId, String brandCode);

  /**
   * Fetches brand wip by brand name and business partner code
   *
   * @param brandName
   * @param businessPartnerCode
   * @return
   * @throws Exception
   */
  BrandWipResponse findByBrandNameAndBusinessPartnerCode(String brandName, String businessPartnerCode) throws Exception;

  /**
   * Find brand wip by brandRequestCode
   * @param brandRequestCode
   * @return
   * @throws Exception
   */
  BrandWipResponse filterByBrandRequestCode(String brandRequestCode) throws Exception;


  /**
   * Find brand wip by brandRequestCode irrespective of state
   * @param brandRequestCode
   * @return
   * @throws Exception
   */
  BrandWipResponse filterByBrandRequestCodeIrrespectiveOfState(String storeId, String brandRequestCode) throws Exception;

  /**
   * find brand by brandName irrespective of state
   * @param storeId
   * @param brandName
   * @return
   */
  BrandResponse getBrandByNameFromBrandWip(String storeId, String brandName);

  /**
   * update valid brand flag in pcc_brand_wip
   * @param updatedBy
   * @param brandCode
   * @param validBrand
   */
  void updateValidBrandFlag(String updatedBy, String brandCode, boolean validBrand);

  /**
   * Get list of all in review brands
   * @param storeId
   * @return
   */
  List<BrandInReviewResponse> getAllInReviewBrands(String storeId);

  /**
   * Update brand name
   * @param storeId
   * @param brandCode
   * @param brandName
   *
   */
  BrandWip updateBrandName(String storeId, String brandCode, String brandName);
}
