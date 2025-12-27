package com.gdn.mta.product.config;

import java.time.Duration;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CachingConfigurerSupport;
import org.springframework.cache.interceptor.CacheErrorHandler;
import org.springframework.cache.interceptor.SimpleCacheResolver;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.cache.RedisCacheConfiguration;
import org.springframework.data.redis.cache.RedisCacheManager;
import org.springframework.data.redis.cache.RedisCacheWriter;
import org.springframework.data.redis.connection.jedis.JedisConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.serializer.JdkSerializationRedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;

import com.gdn.common.base.cache.HashBasedKeyGenerator;
import redis.clients.jedis.JedisPoolConfig;

@Configuration
public class RedisConfiguration extends CachingConfigurerSupport {

  @Autowired
  RedisProperties redisProperties;

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
    return new  KryoSerializer();
  }

  @Bean(name = "jedisConnectionFactory")
  public JedisConnectionFactory jedisConnectionFactory() {
    JedisConnectionFactory jedisConnectionFactory = new JedisConnectionFactory();
    jedisConnectionFactory.setHostName(redisProperties.getRedisHostName());
    jedisConnectionFactory.setPort(redisProperties.getRedisPort());
    jedisConnectionFactory.setTimeout(redisProperties.getTimeout());
    jedisConnectionFactory.setUsePool(redisProperties.isUsePool());
    jedisConnectionFactory.setPassword(redisProperties.getRedisPassword());
    jedisConnectionFactory.setDatabase(redisProperties.getRedisDataBase());
    jedisConnectionFactory.setPoolConfig(redisPoolConfig());
    return jedisConnectionFactory;
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
    redisTemplate.setConnectionFactory(jedisConnectionFactory());
    redisTemplate.setKeySerializer(stringRedisSerializer());
    redisTemplate.setValueSerializer(kryoSerializer());
    return redisTemplate;
  }

  @Bean(name = "stringRedisTemplate")
  public StringRedisTemplate stringRedisTemplate() {
    StringRedisTemplate stringRedisTemplate = new StringRedisTemplate();
    stringRedisTemplate.setConnectionFactory(jedisConnectionFactory());
    stringRedisTemplate.setKeySerializer(stringRedisSerializer());
    return stringRedisTemplate;
  }

  @Bean(name = "cacheManager")
  public RedisCacheManager cacheManager() {
    RedisCacheWriter cacheWriter = RedisCacheWriter.nonLockingRedisCacheWriter(jedisConnectionFactory());
    RedisCacheConfiguration cacheConfiguration = RedisCacheConfiguration.defaultCacheConfig().entryTtl(Duration
        .ofSeconds(redisProperties.getDefaultExpiration()));
    if (!redisProperties.isUsePrefix()) {
      cacheConfiguration = cacheConfiguration.disableKeyPrefix();
    }
    return new RedisCacheManager(cacheWriter, cacheConfiguration);
  }

  @Bean(name = "productLevel3CountCache")
  public RedisCacheManager productLevel3CountCache() {
    RedisCacheWriter cacheWriter = RedisCacheWriter.nonLockingRedisCacheWriter(jedisConnectionFactory());
    RedisCacheConfiguration cacheConfiguration = RedisCacheConfiguration.defaultCacheConfig()
      .entryTtl(Duration.ofMinutes(redisProperties.getProductLevel3CountTimeToLiveInMinutes()));
    if (!redisProperties.isUsePrefix()) {
      cacheConfiguration = cacheConfiguration.disableKeyPrefix();
    }
    return new RedisCacheManager(cacheWriter, cacheConfiguration);
  }

  @Bean(name = "productLimitsCacheManager")
  public RedisCacheManager productLimitsCacheManager() {
    RedisCacheWriter cacheWriter = RedisCacheWriter.nonLockingRedisCacheWriter(jedisConnectionFactory());
    RedisCacheConfiguration cacheConfiguration = RedisCacheConfiguration.defaultCacheConfig()
        .entryTtl(Duration.ofSeconds(redisProperties.getProductLimitsTtl()));
    if (!redisProperties.isUsePrefix()) {
      cacheConfiguration = cacheConfiguration.disableKeyPrefix();
    }
    return new RedisCacheManager(cacheWriter, cacheConfiguration);
  }


  @Bean(name = "simpleCacheErrorHandler")
  public CacheErrorHandlerAsMiss simpleCacheErrorHandler() {
    return new CacheErrorHandlerAsMiss();
  }

  @Override
  public CacheErrorHandler errorHandler() {
    return new com.gdn.mta.product.config.CacheErrorHandlerAsMiss();
  }

  @Bean(name = "simpleCacheResolver")
  public SimpleCacheResolver simpleCacheResolver() {
    return new SimpleCacheResolver(cacheManager());
  }


}
