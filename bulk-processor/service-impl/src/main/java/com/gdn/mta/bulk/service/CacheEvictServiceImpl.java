package com.gdn.mta.bulk.service;

import org.springframework.cache.annotation.CacheEvict;
import org.springframework.stereotype.Service;

import com.gdn.partners.bulk.util.CacheKeys;
import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class CacheEvictServiceImpl implements CacheEvictService {

  @Override
  @CacheEvict(cacheManager = CacheKeys.CAFFEINE_CACHE_MANAGER, value = CacheKeys.PROFILE_RESPONSE, key = "#businessPartnerCode")
  public void evictBusinessPartnerCache(String businessPartnerCode) {
  }
}
