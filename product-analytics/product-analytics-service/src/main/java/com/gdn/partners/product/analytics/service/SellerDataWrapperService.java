package com.gdn.partners.product.analytics.service;

public interface SellerDataWrapperService {

  /**
   * Update official store flag
   *
   * @param sellerCode
   * @param officialStore
   */
  void updateOfficialStoreFlagForASeller(String sellerCode, boolean officialStore);
}
