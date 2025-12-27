package com.gdn.x.mta.distributiontask.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.cache.CacheManager;
import org.springframework.cache.caffeine.CaffeineCacheManager;
import org.springframework.cache.interceptor.CacheErrorHandler;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;

@Configuration
@ConditionalOnProperty(value = "redis.cache.enabled", havingValue = "false")
public class CaffeineCacheConfig {

  @Value("${caffeine.cache.spec}")
  private String caffeineSpecification;

  @Value("${caffeine.cache.higherTTL.spec}")
  private String caffeineHigherTTLSpecification;

  @Value("${caffeine.cache.moreThanOneDayTTL.spec}")
  private String caffeineMoreThanOneDayTTLSpec;

  @Primary
  @Bean(name = "cacheManager")
  public CacheManager cacheManager() {
    CaffeineCacheManager caffeineCacheManager = new CaffeineCacheManager();
    caffeineCacheManager.setCacheSpecification(caffeineSpecification);
    return caffeineCacheManager;
  }

  @Bean(name = "cacheManagerHigherTTL")
  public CacheManager higherTTLCacheManager() {
    CaffeineCacheManager cacheManager = new CaffeineCacheManager();
    cacheManager.setCacheSpecification(caffeineHigherTTLSpecification);
    return cacheManager;
  }

  @Bean(name = "cacheManagerMoreThanOneDayTTL")
  public CacheManager moreThanOneDayTTLCacheManager() {
    CaffeineCacheManager cacheManager = new CaffeineCacheManager();
    cacheManager.setCacheSpecification(caffeineMoreThanOneDayTTLSpec);
    return cacheManager;
  }

  @Bean
  public CacheErrorHandler errorHandler() {
    return new CacheErrorHandlerAsMiss();
  }

}
