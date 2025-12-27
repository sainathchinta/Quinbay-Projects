package com.gdn.mta.bulk.config;

import org.springframework.cache.Cache;
import org.springframework.cache.interceptor.CacheErrorHandler;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class CacheErrorHandlerAsMiss implements CacheErrorHandler {
  @Override
  public void handleCacheGetError(RuntimeException exception, Cache cache, Object key) {
    try {
    } catch (Exception e) {
      log.error("Exception caught while getting cache key : {} ", key, e);
    }
  }

  @Override
  public void handleCachePutError(RuntimeException exception, Cache cache, Object key, Object value) {
    try {
    } catch (Exception e) {
      log.error("Error while putting key {} ", key, e);
    }
  }

  @Override
  public void handleCacheEvictError(RuntimeException exception, Cache cache, Object key) {
    try {
    } catch (Exception e) {
      log.error("Error while evicting cache key {} ", key, e);
    }

  }

  @Override
  public void handleCacheClearError(RuntimeException exception, Cache cache) {
    try {
    } catch (Exception e) {
      log.error("Error while clearing cache ", e);
    }
  }
}
