package com.gdn.x.productcategorybase.config;

import com.gdn.x.config.CacheErrorHandlerAsMiss;
import com.github.benmanes.caffeine.cache.Caffeine;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.cache.CacheManager;
import org.springframework.cache.caffeine.CaffeineCacheManager;
import org.springframework.cache.interceptor.CacheErrorHandler;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.time.Duration;

@Configuration
@ConditionalOnProperty(value = "caffeine.cache.enabled", havingValue = "true")
public class CaffeineCacheConfiguration {

  @Value("${caffeine.cache.maximum-size}")
  private int maximumSize;

  @Value("${caffeine.cache.expire-after-write-seconds}")
  private int expireAfterWriteSeconds;

  @Value("${caffeine.cache.record-stats}")
  private boolean recordStats;

  @Bean(name = "caffeineCacheManager")
  public CacheManager cacheManager() {
    Caffeine<Object, Object> caffeine = Caffeine.newBuilder().maximumSize(maximumSize)
        .expireAfterWrite(Duration.ofSeconds(expireAfterWriteSeconds));
    if (recordStats) {
      caffeine.recordStats();
    }
    CaffeineCacheManager caffeineCacheManager = new CaffeineCacheManager();
    caffeineCacheManager.setCaffeine(caffeine);
    return caffeineCacheManager;
  }

  @Bean
  public CacheErrorHandler errorHandler() {
    return new CacheErrorHandlerAsMiss();
  }

}