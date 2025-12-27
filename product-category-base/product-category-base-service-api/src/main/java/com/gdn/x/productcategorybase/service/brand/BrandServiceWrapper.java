package com.gdn.x.productcategorybase.service.brand;

import com.gdn.x.productcategorybase.dto.brand.BrandApproveRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandResponse;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;

public interface BrandServiceWrapper {

  /**
   * Service wrapper method for approving brand
   *
   * @param brandApproveRequest
   * @return
   * @throws Exception
   */
  CreateBrandResponse approveBrand(BrandApproveRequest brandApproveRequest) throws Exception;


  /**
   * Reject brand service wrapper method
   *
   * @param brandRejectRequest
   * @return
   * @throws Exception
   */
  BrandWipResponse rejectBrand(BrandRejectRequest brandRejectRequest) throws Exception;

  /**
   * Update brand service wrapper method
   *
   * @param storeId
   * @param brandApproveRequest
   */
  void updateBrand(String storeId, BrandApproveRequest brandApproveRequest) throws Exception;

  /**
   * Create a brand with given brand request
   *
   * @param storeId
   * @param brandWip
   * @return Brand Request Code
   * @throws Exception
   */
  String createBrand(String storeId, BrandWip brandWip) throws Exception;
}
