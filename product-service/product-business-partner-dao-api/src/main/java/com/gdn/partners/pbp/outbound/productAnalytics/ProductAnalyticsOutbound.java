package com.gdn.partners.pbp.outbound.productAnalytics;

import com.gdn.partners.product.analytics.web.model.AutoQCDetailResponse;
import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;

public interface ProductAnalyticsOutbound {

  /**
   * Get Auto Qc Details
   *
   * @param merchantCode
   * @param categoryCode
   * @return
   * @throws Exception
   */
  AutoQCDetailResponse getAutoQCDetails(String merchantCode, String categoryCode) throws Exception;

  /**
   * Get Seller detail
   *
   * @param merchantCode
   * @return
   */
  SellerDetailResponse getSellerDetail(String merchantCode);
}
