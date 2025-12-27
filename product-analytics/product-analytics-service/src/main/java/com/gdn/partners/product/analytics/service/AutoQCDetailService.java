package com.gdn.partners.product.analytics.service;

import java.util.List;

import com.gdn.partners.product.analytics.web.model.AutoQCDetailResponse;
import com.gdn.partners.product.analytics.web.model.SellerFieldsChangeResponse;

public interface AutoQCDetailService {

  /**
   * find All AutoQC Details based on merchantCode and categoryCode
   *
   * @param merchantCode
   * @param categoryCode
   * @return
   */
  AutoQCDetailResponse findByMerchantCodeAndCategoryCode(String merchantCode, String categoryCode) throws Exception;

  /**
   * Update official seller flag by businessPartner code
   *
   * @param merchantCode
   * @param officialStore
   */
  void updateOfficialStoreFlagBySellerCode(String merchantCode, boolean officialStore);

  /**
   * Clear cache and publish event
   *
   * @param sellerFieldsChangeResponseList
   */
  void clearCacheAndPublishEvent(List<SellerFieldsChangeResponse> sellerFieldsChangeResponseList);
}
