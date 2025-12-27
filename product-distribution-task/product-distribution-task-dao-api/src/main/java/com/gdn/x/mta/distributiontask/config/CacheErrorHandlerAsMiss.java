package com.gdn.x.mta.distributiontask.config;

import org.springframework.cache.Cache;
import org.springframework.cache.interceptor.CacheErrorHandler;

import lombok.extern.slf4j.Slf4j;

@Slf4j
public class CacheErrorHandlerAsMiss implements CacheErrorHandler {

  public CacheErrorHandlerAsMiss() {
  }

  public void handleCacheClearError(RuntimeException exception, Cache cache) {
    try {
    } catch (Exception e) {
      log.error("Exception caught while clearing cache ", exception);
    }
  }

  public void handleCacheEvictError(RuntimeException exception, Cache cache, Object key) {
    try {
    } catch (Exception e) {
      log.error("Exception caught while evicting cache key : {} ", key, exception);
    }
  }

  public void handleCacheGetError(RuntimeException exception, Cache cache, Object key) {
    try {
    } catch (Exception e) {
      log.error("Exception caught while getting cache key : {} ", key, exception);
    }
  }

  public void handleCachePutError(RuntimeException exception, Cache cache, Object key, Object value) {
    try {
    } catch (Exception e) {
      log.error("Exception caught while putting cache key : {} ", key, exception);
    }
  }
}
