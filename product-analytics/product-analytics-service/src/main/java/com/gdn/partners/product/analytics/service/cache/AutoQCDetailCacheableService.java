package com.gdn.partners.product.analytics.service.cache;

import com.gdn.partners.product.analytics.entity.AutoQCDetail;

public interface AutoQCDetailCacheableService {

  /**
   * Get All cached AutoQC Details based on merchantCode and categoryCode
   *
   * @param merchantCode
   * @param categoryCode
   * @return
   */
  AutoQCDetail findCacheablesByMerchantCodeAndCategoryCode(String merchantCode, String categoryCode) throws Exception;

  /**
   * Evict cache by merchant and seller code
   *
   * @param merchantCode
   * @param categoryCode
   */
  void evictCacheByMerchantCodeAndCategoryCode(String merchantCode, String categoryCode);
}
