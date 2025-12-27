package com.gdn.x.productcategorybase.config;

import java.time.Duration;
import java.util.concurrent.Executor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CachingConfigurer;
import org.springframework.cache.interceptor.CacheErrorHandler;
import org.springframework.cache.interceptor.CacheResolver;
import org.springframework.cache.interceptor.SimpleCacheResolver;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.data.redis.cache.CacheKeyPrefix;
import org.springframework.data.redis.cache.RedisCacheConfiguration;
import org.springframework.data.redis.cache.RedisCacheManager;
import org.springframework.data.redis.connection.RedisStandaloneConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceClientConfiguration;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.connection.lettuce.LettucePoolingClientConfiguration;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.serializer.RedisSerializationContext;
import org.springframework.data.redis.serializer.RedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;

import com.gdn.common.base.cache.HashBasedKeyGenerator;
import com.gdn.x.base.serializer.KryoSerializer;
import com.gdn.x.config.CacheErrorHandlerAsMiss;
import com.gdn.x.productcategorybase.properties.RedisProperties;
import redis.clients.jedis.JedisPoolConfig;

@Configuration
public class RedisConfiguration implements CachingConfigurer {

  @Autowired
  RedisProperties redisProperties;

  @Override
  public HashBasedKeyGenerator keyGenerator() {
    return new HashBasedKeyGenerator();
  }

  @Bean(name = "stringRedisSerializer")
  public RedisSerializer stringRedisSerializer() {
    return new StringRedisSerializer();
  }

  @Bean(name = "kryoSerializer")
  public KryoSerializer kryoSerializer() {
    return new KryoSerializer();
  }

  @Primary
  @Bean(name = "lettuceConnectionFactory")
  public LettuceConnectionFactory lettuceConnectionFactory() {
    RedisStandaloneConfiguration redisStandaloneConfiguration = new RedisStandaloneConfiguration();
    redisStandaloneConfiguration.setHostName(redisProperties.getRedisHostName());
    redisStandaloneConfiguration.setPort(redisProperties.getRedisPort());
    redisStandaloneConfiguration.setDatabase(redisProperties.getRedisDataBase());
    LettuceClientConfiguration lettuceClientConfiguration =
        LettucePoolingClientConfiguration.builder().poolConfig(redisPoolConfig())
            .commandTimeout(Duration.ofMillis(redisProperties.getTimeout())).build();
    return new LettuceConnectionFactory(redisStandaloneConfiguration, lettuceClientConfiguration);
  }

  @Bean(name = "redisPoolConfig")
  public JedisPoolConfig redisPoolConfig() {
    JedisPoolConfig jedisPoolConfig = new JedisPoolConfig();
    jedisPoolConfig.setMinIdle(redisProperties.getMinIdle());
    jedisPoolConfig.setMaxIdle(redisProperties.getMaxIdle());
    jedisPoolConfig.setMaxTotal(redisProperties.getMaxTotal());
    jedisPoolConfig.setMaxWaitMillis(redisProperties.getMaxWaitMillis());
    jedisPoolConfig.setEvictionPolicyClassName(redisProperties.getEvictionPolicyClassName());
    jedisPoolConfig.setBlockWhenExhausted(redisProperties.isBlockWhenExhausted());
    jedisPoolConfig.setTestOnBorrow(redisProperties.isTestOnBorrow());
    jedisPoolConfig.setLifo(redisProperties.isLifo());
    jedisPoolConfig.setMinEvictableIdleTimeMillis(redisProperties.getMinEvictableIdleTimeMillis());
    jedisPoolConfig.setSoftMinEvictableIdleTimeMillis(
        redisProperties.getSoftMinEvictableIdleTimeMillis());
    jedisPoolConfig.setNumTestsPerEvictionRun(redisProperties.getNumTestsPerEvictionRun());
    jedisPoolConfig.setTestOnReturn(redisProperties.isTestOnReturn());
    jedisPoolConfig.setTestWhileIdle(redisProperties.isTestWhileIdle());
    jedisPoolConfig.setTimeBetweenEvictionRunsMillis(
        redisProperties.getTimeBetweenEvictionRunsMillis());
    jedisPoolConfig.setJmxEnabled(redisProperties.isJmxEnabled());
    jedisPoolConfig.setJmxNamePrefix(redisProperties.getJmxNamePrefix());
    return jedisPoolConfig;
  }

  @Bean(name = "redisTemplate")
  public RedisTemplate redisTemplate() {
    RedisTemplate redisTemplate = new RedisTemplate();
    redisTemplate.setConnectionFactory(lettuceConnectionFactory());
    redisTemplate.setKeySerializer(stringRedisSerializer());
    return redisTemplate;
  }

  @Override
  @Bean(name = "cacheManager")
  @Primary
  public RedisCacheManager cacheManager() {
    return RedisCacheManager.builder(lettuceConnectionFactory()).cacheDefaults(
        RedisCacheConfiguration.defaultCacheConfig()
            .serializeKeysWith(RedisSerializationContext.SerializationPair.fromSerializer(stringRedisSerializer()))
            .entryTtl(Duration.ofSeconds(redisProperties.getDefaultExpiration()))
            .computePrefixWith(CacheKeyPrefix.simple())).build();

  }

  @Bean(name = "restrictedKeywordCacheManager")
  public RedisCacheManager restrictedKeywordCacheManager() {
    return RedisCacheManager.builder(lettuceConnectionFactory()).cacheDefaults(
        RedisCacheConfiguration.defaultCacheConfig()
            .serializeKeysWith(RedisSerializationContext.SerializationPair.fromSerializer(stringRedisSerializer()))
            .entryTtl(Duration.ofSeconds(redisProperties.getRestrictedKeywordTTL()))
            .computePrefixWith(CacheKeyPrefix.simple())).build();
  }

  @Override
  public CacheErrorHandler errorHandler() {
    return new CacheErrorHandlerAsMiss();
  }

  @Override
  public CacheResolver cacheResolver() {
    return new SimpleCacheResolver(cacheManager());
  }

}
