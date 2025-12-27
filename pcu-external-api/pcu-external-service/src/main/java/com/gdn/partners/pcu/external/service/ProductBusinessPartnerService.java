package com.gdn.partners.pcu.external.service;

import com.gda.mta.product.dto.ProductCopyRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryRequest;
import com.gda.mta.product.dto.ProductLevel3SummaryResponse;
import com.gda.mta.product.dto.response.AvailableToCopyProductDetailsResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.external.model.request.ProductBusinessPartnerServiceRequest;
import com.gdn.partners.pcu.external.web.model.request.DefaultConfigurationAndPickupPointRequest;
import com.gdn.partners.pcu.external.web.model.response.BusinessPartnerProfileWebResponse;
import com.gdn.partners.pcu.external.web.model.response.CreateProductBusinessPartnerResponse;

/**
 * Created by govind on 09/12/2018 AD.
 */
public interface ProductBusinessPartnerService {

  /**
   * Create Product using flow2
   *
   * @param productBusinessPartnerServiceRequest
   * @return
   * @throws Exception
   */
  SingleBaseResponse<CreateProductBusinessPartnerResponse> create(
      ProductBusinessPartnerServiceRequest productBusinessPartnerServiceRequest) throws Exception;

  /**
   * To get pick up points by bpCode
   *
   * @param businessPartnerCode
   * @return
   */
  BusinessPartnerProfileWebResponse getBusinessPartnerProfile(String businessPartnerCode);

  /**
   * Copy product items using flow2
   *
   * @param ProductCopyRequest product copy request
   * @param isRetryAttempt     is copy retry attempt
   *
   * @return
   */
  GdnBaseRestResponse copy(ProductCopyRequest ProductCopyRequest, boolean isRetryAttempt);

  /**
   * Copy All product items using flow2 from linked to partner account)
   *
   * @param productCopyRequest product copy request
   *
   * @return
   */
  GdnBaseRestResponse copyAll(ProductCopyRequest productCopyRequest);

  /**
   * Products available to copy from source (linked partner code) to target (business partner code)
   *
   * @param businessPartnerCode business partner code to which product is being copied
   * @param linkedPartnerCode   source (linked) partner account from which products being copied
   * @param page                page number
   * @param size                page size
   * @param summaryRequest      products filter request
   *
   * @return available products which are eligible to be copied
   */
  GdnRestListResponse<AvailableToCopyProductDetailsResponse> productsAvailableToDirectCopy(String businessPartnerCode,
    String linkedPartnerCode, int page, int size, ProductLevel3SummaryRequest summaryRequest);

  /**
   * Check if any product is mapped to merchant by merchant code
   *
   *
   * @param merchantCode
   * @return
   */
  boolean isProductMappedToMerchant(String merchantCode);

  /**
   * update default configuration and pickup point code
   *
   * @param storeId
   * @param merchantCode
   * @param request
   */
  void updateDefaultConfigurationsAndPickupPoints(String storeId, String merchantCode, DefaultConfigurationAndPickupPointRequest request);

}
