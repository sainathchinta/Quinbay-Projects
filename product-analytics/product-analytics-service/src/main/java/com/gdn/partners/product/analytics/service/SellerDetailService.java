package com.gdn.partners.product.analytics.service;

import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;

public interface SellerDetailService {

  /**
   * find All AutoQC Details based on merchantCode and categoryCode
   *
   * @param merchantCode
   * @return
   */
  SellerDetailResponse findSellerDetailByMerchantCode(String merchantCode) throws Exception;

  /**
   * Update official store flag
   *
   * @param sellerCode
   * @param officialStore
   */
  void updateOfficialStoreFlagBySellerCode(String sellerCode, boolean officialStore);
}
