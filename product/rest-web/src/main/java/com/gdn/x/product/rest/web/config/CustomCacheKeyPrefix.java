package com.gdn.x.product.rest.web.config;

import org.springframework.data.redis.cache.CacheKeyPrefix;

@FunctionalInterface
public interface CustomCacheKeyPrefix extends CacheKeyPrefix {
  static CacheKeyPrefix simple() {
    return name -> name + ":";
  }
}
