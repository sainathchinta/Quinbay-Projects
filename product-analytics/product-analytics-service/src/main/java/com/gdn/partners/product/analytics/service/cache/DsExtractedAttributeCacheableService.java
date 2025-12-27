package com.gdn.partners.product.analytics.service.cache;

import com.gdn.partners.product.analytics.entity.DSExtractionEntity;

public interface DsExtractedAttributeCacheableService {
  /**
   * to cache ds attribute data
   * @param dsAttributeName
   */
  void evictCacheByDsAttributeName(String dsAttributeName);

  /**
   * check if it DS-Extracted attribute
   *
   * @param dsAttributeName
   * @return
   */
  DSExtractionEntity fetchDSExtractionsByName(String dsAttributeName);
}
