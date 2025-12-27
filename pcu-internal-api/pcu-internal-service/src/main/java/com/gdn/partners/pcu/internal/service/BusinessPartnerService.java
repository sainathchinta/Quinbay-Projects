package com.gdn.partners.pcu.internal.service;


import com.gdn.partners.pcu.internal.web.model.response.BusinessPartnerProfileWebResponse;
import org.springframework.data.domain.Page;

import com.gdn.partners.pcu.internal.web.model.request.ProductSuspensionFilterRequest;
import com.gdn.partners.pcu.internal.web.model.request.ReviewProductsFilterRequest;
import com.gdn.partners.pcu.internal.web.model.response.BusinessPartnerWebResponse;


public interface BusinessPartnerService {

  /**
   * Get business partners by filter request , activated and viewable flag
   *
   * @param request
   * @param activated
   * @param viewable
   * @param page
   * @param size
   * @return
   */
  Page<BusinessPartnerWebResponse> getBusinessPartnersByTimeAndStatusFilter(ReviewProductsFilterRequest request,
      boolean activated, boolean viewable, int page, int size);

  /**
   * Get all the active merchant list by request
   *
   * @param request
   * @param page
   * @param size
   * @return
   */
  Page<BusinessPartnerWebResponse> getAllActiveMerchantList(ProductSuspensionFilterRequest request, int page,
      int size);

  /**
   * Get Business Partner name by BP Code
   *
   * @param businessPartnerCode
   * @throws
   * @return
   *
   */
  String getBusinessPartnerNameByCode(String businessPartnerCode) throws Exception;

  /**
   *
   * @param businessPartnerCode Business Partner Code
   * @return BusinessPartnerProfileWebResponse
   */
  BusinessPartnerProfileWebResponse fetchBusinessPartnerFlags(String businessPartnerCode);
}
