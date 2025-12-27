package com.gdn.x.product.outbound.api;

import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;

public interface ProductAnalyticsOutbound {

  /**
   * fetch seller detail by seller code
   * @param storeId
   * @param requestId
   * @param clientId
   * @param channelId
   * @param username
   * @param sellerCode
   * @return
   */
  SellerDetailResponse checkGoodSeller(String storeId, String requestId, String clientId, String channelId,
      String username, String sellerCode);

}
