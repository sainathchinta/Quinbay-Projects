package com.gdn.x.mta.distributiontask.config;

import java.time.Duration;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.cache.annotation.CachingConfigurer;
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
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.serializer.JdkSerializationRedisSerializer;
import org.springframework.data.redis.serializer.RedisSerializationContext;
import org.springframework.data.redis.serializer.StringRedisSerializer;

import com.gdn.common.base.cache.HashBasedKeyGenerator;
import com.gdn.x.base.serializer.KryoSerializer;
import redis.clients.jedis.JedisPoolConfig;

@Configuration
@ConditionalOnProperty(value = "redis.cache.enabled", havingValue = "true")
public class RedisConfiguration implements CachingConfigurer {

  @Autowired
  private RedisProperties redisProperties;

  @Bean(name = "keyGenerator")
  public HashBasedKeyGenerator keyGenerator() {
    return new HashBasedKeyGenerator();
  }

  @Bean(name = "stringRedisSerializer")
  public StringRedisSerializer stringRedisSerializer() {
    return new StringRedisSerializer();
  }

  @Bean(name = "defaultRedisSerializer")
  public JdkSerializationRedisSerializer defaultRedisSerializer() {
    return new JdkSerializationRedisSerializer();
  }

  @Bean(name = "kryoSerializer")
  public KryoSerializer kryoSerializer() {
    return new KryoSerializer();
  }

  @Primary
  @Bean(name = "lettuceConnectionFactory")
  public LettuceConnectionFactory lettuceConnectionFactory() {
    RedisStandaloneConfiguration redisStandaloneConfiguration = new RedisStandaloneConfiguration();
    redisStandaloneConfiguration.setHostName(redisProperties.getHostName());
    redisStandaloneConfiguration.setPort(redisProperties.getPort());
    redisStandaloneConfiguration.setDatabase(redisProperties.getDataBase());
    LettuceClientConfiguration lettuceClientConfiguration =
      LettucePoolingClientConfiguration.builder().poolConfig(redisPoolConfig())
        .commandTimeout(Duration.ofMillis(redisProperties.getTimeout())).build();
    return new LettuceConnectionFactory(redisStandaloneConfiguration, lettuceClientConfiguration);
  }

  @Bean(name = "redisPoolConfig")
  public JedisPoolConfig redisPoolConfig() {
    JedisPoolConfig jedisPoolConfig = new JedisPoolConfig();
    jedisPoolConfig.setMinIdle(redisProperties.getPoolMinIdle());
    jedisPoolConfig.setMaxIdle(redisProperties.getPoolMaxIdle());
    jedisPoolConfig.setMaxTotal(redisProperties.getPoolMaxTotal());
    jedisPoolConfig.setMaxWaitMillis(redisProperties.getPoolMaxWaitMillis());
    jedisPoolConfig.setEvictionPolicyClassName(redisProperties.getPoolEvictionPolicyClassName());
    jedisPoolConfig.setBlockWhenExhausted(redisProperties.isPoolBlockWhenExhausted());
    jedisPoolConfig.setTestOnBorrow(redisProperties.isPoolTestOnBorrow());
    jedisPoolConfig.setLifo(redisProperties.isPoolLifo());
    jedisPoolConfig.setMinEvictableIdleTimeMillis(redisProperties.getPoolMinEvictableIdleTimeMillis());
    jedisPoolConfig.setSoftMinEvictableIdleTimeMillis(redisProperties.getPoolSoftMinEvictableIdleTimeMillis());
    jedisPoolConfig.setNumTestsPerEvictionRun(redisProperties.getPoolNumTestsPerEvictionRun());
    jedisPoolConfig.setTestOnReturn(redisProperties.isPoolTestOnReturn());
    jedisPoolConfig.setTestWhileIdle(redisProperties.isPoolTestWhileIdle());
    jedisPoolConfig.setTimeBetweenEvictionRunsMillis(redisProperties.getPoolTimeBetweenEvictionRunsMillis());
    jedisPoolConfig.setJmxEnabled(redisProperties.isPoolJmxEnabled());
    jedisPoolConfig.setJmxNamePrefix(redisProperties.getPoolJmxNamePrefix());
    return jedisPoolConfig;
  }

  @Bean(name = "redisTemplate")
  public RedisTemplate redisTemplate() {
    RedisTemplate redisTemplate = new RedisTemplate();
    redisTemplate.setConnectionFactory(lettuceConnectionFactory());
    redisTemplate.setKeySerializer(stringRedisSerializer());
    redisTemplate.setValueSerializer(kryoSerializer());
    return redisTemplate;
  }

  @Bean(name = "stringRedisTemplate")
  public StringRedisTemplate stringRedisTemplate() {
    StringRedisTemplate stringRedisTemplate = new StringRedisTemplate();
    stringRedisTemplate.setConnectionFactory(lettuceConnectionFactory());
    stringRedisTemplate.setKeySerializer(stringRedisSerializer());
    return stringRedisTemplate;
  }

  @Primary
  @Bean(name = "cacheManager")
  public RedisCacheManager cacheManager() {
    return RedisCacheManager.builder(lettuceConnectionFactory()).cacheDefaults(
      RedisCacheConfiguration.defaultCacheConfig()
        .entryTtl(Duration.ofSeconds(redisProperties.getCacheDefaultExpiration()))
        .computePrefixWith(CacheKeyPrefix.simple())).build();
  }

  @Bean(name = "cacheManagerHigherTTL")
  public RedisCacheManager cacheManagerHigherTTL() {
    return RedisCacheManager.builder(lettuceConnectionFactory()).cacheDefaults(
      RedisCacheConfiguration.defaultCacheConfig()
        .serializeKeysWith(RedisSerializationContext.SerializationPair.fromSerializer(stringRedisSerializer()))
        .entryTtl(Duration.ofSeconds(redisProperties.getCacheExpirationHigherTtl()))
        .computePrefixWith(CacheKeyPrefix.simple())).build();
  }

  @Override
  public CacheErrorHandlerAsMiss errorHandler() {
    return new CacheErrorHandlerAsMiss();
  }

  @Bean(name = "simpleCacheResolver")
  public SimpleCacheResolver simpleCacheResolver() {
    return new SimpleCacheResolver(cacheManager());
  }

  @Bean(name = "cacheManagerMoreThanOneDayTTL")
  public RedisCacheManager cacheManagerMoreThanOneDayTTL() {
    return RedisCacheManager.builder(lettuceConnectionFactory()).cacheDefaults(
      RedisCacheConfiguration.defaultCacheConfig().serializeKeysWith(
          RedisSerializationContext.SerializationPair.fromSerializer(stringRedisSerializer()))
        .entryTtl(Duration.ofSeconds(redisProperties.getCacheExpirationMoreThanOneDayTtl()))
        .computePrefixWith(CacheKeyPrefix.simple())).build();
  }
}
