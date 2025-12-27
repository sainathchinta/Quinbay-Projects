package com.gdn.mta.bulk.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.cache.CacheManager;
import org.springframework.cache.caffeine.CaffeineCacheManager;
import org.springframework.cache.interceptor.CacheErrorHandler;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

@Configuration
@ConditionalOnProperty(value = "caffeine.cache.enabled", havingValue = "true")
public class CaffeineCacheConfiguration {

  @Value("${caffeine.cache.spec}")
  private String caffeineSpecification;

  @Bean(name = "caffeineCacheManager")
  public CacheManager cacheManager() {
    CaffeineCacheManager caffeineCacheManager = new CaffeineCacheManager();
    caffeineCacheManager.setCacheSpecification(caffeineSpecification);
    return caffeineCacheManager;
  }

  @Bean
  public CacheErrorHandler errorHandler() {
    return new CacheErrorHandlerAsMiss();
  }

}