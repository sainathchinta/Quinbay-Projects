package com.gdn.partners.product.analytics.service.cache;

import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;

public interface SellerDetailCacheableService {

  /**
   * Get All cached Seller Details based on merchantCode
   *
   * @param merchantCode
   * @return
   */
  SellerDetailResponse findCacheablesByMerchantCode(String merchantCode) throws Exception;

  /**
   * Evict cache by merchantCode
   *
   * @param merchantCode
   */
  void evictCacheByMerchantCode(String merchantCode);
}
