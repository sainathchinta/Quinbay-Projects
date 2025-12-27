package com.gdn.mta.bulk.service;

public interface CacheEvictService {

  /**
   * Evict business partner cache
   *
   * @param businessPartnerCode
   */
  void evictBusinessPartnerCache(String businessPartnerCode);

}