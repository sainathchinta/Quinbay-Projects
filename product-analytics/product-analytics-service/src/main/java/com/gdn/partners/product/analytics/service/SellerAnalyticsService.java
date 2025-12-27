package com.gdn.partners.product.analytics.service;

import com.gdn.partners.product.analytics.web.model.SellerAnalyticsResponse;

public interface SellerAnalyticsService {

  /**
   * @param storeId
   * @param sellerCode
   * @return
   */
  SellerAnalyticsResponse findSellerAnalyticsDetailByStoreIdAndSellerCode(String storeId,
      String sellerCode);
}
