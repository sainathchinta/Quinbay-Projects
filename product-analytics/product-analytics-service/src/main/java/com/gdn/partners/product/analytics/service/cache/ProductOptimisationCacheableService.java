package com.gdn.partners.product.analytics.service.cache;


public interface ProductOptimisationCacheableService {

  /**
   * @param sellerCode sellerCode
   * @return product count
   */
  long findProductCountCacheablesBySellerCode(String sellerCode);

  /**
   *
   * @param sellerCode sellerCode
   */
  void evictCacheBySellerCode(String sellerCode);
}
