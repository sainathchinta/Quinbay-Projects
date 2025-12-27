package com.gdn.partners.product.analytics.config;


import java.time.Duration;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CachingConfigurerSupport;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.interceptor.SimpleCacheResolver;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.data.redis.cache.RedisCacheConfiguration;
import org.springframework.data.redis.cache.RedisCacheManager;
import org.springframework.data.redis.cache.RedisCacheWriter;
import org.springframework.data.redis.connection.RedisNode;
import org.springframework.data.redis.connection.RedisSentinelConfiguration;
import org.springframework.data.redis.connection.jedis.JedisConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.serializer.JdkSerializationRedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;

import com.gdn.common.base.cache.HashBasedKeyGenerator;
import com.gdn.partners.product.analytics.properties.RedisProperties;
import com.gdn.x.base.serializer.KryoSerializer;
import com.gdn.x.config.CacheErrorHandlerAsMiss;
import redis.clients.jedis.JedisPoolConfig;

@Configuration
@EnableCaching
public class RedisConfiguration extends CachingConfigurerSupport {
  private final RedisProperties redisProperties;

  @Autowired
  public RedisConfiguration(RedisProperties redisProperties) {
    this.redisProperties = redisProperties;
  }

  @Bean(name = "keyGenerator")
  public HashBasedKeyGenerator hashBasedKeyGenerator() {
    return new HashBasedKeyGenerator();
  }

  @Bean(name = "stringRedisSerializer")
  public StringRedisSerializer stringRedisSerializer() {
    return new StringRedisSerializer();
  }

  @Bean(name = "kryoSerializer")
  public KryoSerializer kryoSerializer() {
    return new KryoSerializer();
  }

  @Bean(name = "defaultRedisSerializer")
  public JdkSerializationRedisSerializer defaultRedisSerializer() {
    return new JdkSerializationRedisSerializer();
  }

  @Bean(name = "jedisConnectionFactory")
  public JedisConnectionFactory jedisConnectionFactory() {
    JedisConnectionFactory jedisConnectionFactory = new JedisConnectionFactory(sentinelConfiguration());
    jedisConnectionFactory.setPort(redisProperties.getRedisPort());
    jedisConnectionFactory.setTimeout(redisProperties.getTimeout());
    jedisConnectionFactory.setUsePool(redisProperties.isUsePool());
    jedisConnectionFactory.setDatabase(redisProperties.getRedisDatabase());
    jedisConnectionFactory.setPoolConfig(redisPoolConfig());
    return jedisConnectionFactory;
  }

  @Primary
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
    jedisPoolConfig.setSoftMinEvictableIdleTimeMillis(redisProperties.getSoftMinEvictableIdleTimeMillis());
    jedisPoolConfig.setNumTestsPerEvictionRun(redisProperties.getNumTestsPerEvictionRun());
    jedisPoolConfig.setTestOnReturn(redisProperties.isTestOnReturn());
    jedisPoolConfig.setTestWhileIdle(redisProperties.isTestWhileIdle());
    jedisPoolConfig.setTimeBetweenEvictionRunsMillis(redisProperties.getTimeBetweenEvictionRunsMillis());
    jedisPoolConfig.setJmxEnabled(redisProperties.isJmxEnabled());
    jedisPoolConfig.setJmxNamePrefix(redisProperties.getJmxNamePrefix());
    return jedisPoolConfig;
  }

  @Bean(name = "stringRedisTemplate")
  @Primary
  public StringRedisTemplate stringRedisTemplate() {
    StringRedisTemplate stringRedisTemplate = new StringRedisTemplate();
    stringRedisTemplate.setConnectionFactory(jedisConnectionFactory());
    stringRedisTemplate.setKeySerializer(stringRedisSerializer());
    return stringRedisTemplate;
  }

  @Bean(name = "redisTemplate")
  @Primary
  public RedisTemplate redisTemplate() {
    RedisTemplate redisTemplate = new RedisTemplate<>();
    redisTemplate.setConnectionFactory(jedisConnectionFactory());
    redisTemplate.setKeySerializer(stringRedisSerializer());
    return redisTemplate;
  }

  @Bean(name = "cacheManager")
  @Primary
  public RedisCacheManager cacheManager() {
    RedisCacheWriter cacheWriter = RedisCacheWriter.nonLockingRedisCacheWriter(jedisConnectionFactory());
    RedisCacheConfiguration cacheConfiguration = RedisCacheConfiguration.defaultCacheConfig()
        .entryTtl(Duration.ofSeconds(redisProperties.getRedisCacheDefaultExpiration()));
    if (!redisProperties.isUsePrefix()) {
      cacheConfiguration = cacheConfiguration.disableKeyPrefix();
    }
    return new RedisCacheManager(cacheWriter, cacheConfiguration);
  }

  @Primary
  @Bean(name = "simpleCacheErrorHandler")
  public CacheErrorHandlerAsMiss simpleCacheErrorHandler() {
    return new CacheErrorHandlerAsMiss();
  }

  @Primary
  @Bean(name = "simpleCacheResolver")
  public SimpleCacheResolver simpleCacheResolver() {
    return new SimpleCacheResolver(cacheManager());
  }

  @Bean(name = "sellerRedisTemplate")
  @Primary
  public <K, V> RedisTemplate<K, V> sellerRedisTemplate() {
    RedisTemplate<K, V> redisTemplate = new RedisTemplate<>();
    redisTemplate.setConnectionFactory(sellerJedisConnectionFactory());
    redisTemplate.setKeySerializer(stringRedisSerializer());
    return redisTemplate;
  }

  @Primary
  @Bean(name = "sellerJedisConnectionFactory")
  public JedisConnectionFactory sellerJedisConnectionFactory() {
    JedisConnectionFactory jedisConnectionFactory = new JedisConnectionFactory(sentinelConfiguration());
    jedisConnectionFactory.setTimeout(redisProperties.getSellerRedisTimeout());
    jedisConnectionFactory.setUsePool(redisProperties.isSellerRedisUsePool());
    jedisConnectionFactory.setDatabase(redisProperties.getSellerRedisDataBase());
    jedisConnectionFactory.setPoolConfig(sellerRedisPoolConfig());
    return jedisConnectionFactory;
  }

  @Bean(name = "sellerCacheManager")
  @Primary
  public RedisCacheManager sellerCacheManager() {
    RedisCacheWriter cacheWriter = RedisCacheWriter.nonLockingRedisCacheWriter(sellerJedisConnectionFactory());
    RedisCacheConfiguration cacheConfiguration = RedisCacheConfiguration.defaultCacheConfig()
        .entryTtl(Duration.ofSeconds(redisProperties.getSellerRedisCacheDefaultExpiration()));
    if (!redisProperties.isSellerRedisPrefix()) {
      cacheConfiguration = cacheConfiguration.disableKeyPrefix();
    }
    return new RedisCacheManager(cacheWriter, cacheConfiguration);
  }

  @Primary
  @Bean(name = "sellerCacheResolver")
  public SimpleCacheResolver sellerSimpleCacheResolver() {
    return new SimpleCacheResolver(sellerCacheManager());
  }

  @Primary
  @Bean(name = "sellerRedisPoolConfig")
  public JedisPoolConfig sellerRedisPoolConfig() {
    JedisPoolConfig jedisPoolConfig = new JedisPoolConfig();
    jedisPoolConfig.setMinIdle(redisProperties.getSellerMinIdle());
    jedisPoolConfig.setMaxIdle(redisProperties.getSellerMaxIdle());
    jedisPoolConfig.setMaxTotal(redisProperties.getSellerMaxTotal());
    jedisPoolConfig.setMaxWaitMillis(redisProperties.getSellerMaxWaitMillis());
    jedisPoolConfig.setEvictionPolicyClassName(redisProperties.getSellerEvictionPolicyClassName());
    jedisPoolConfig.setBlockWhenExhausted(redisProperties.isSellerBlockWhenExhausted());
    jedisPoolConfig.setTestOnBorrow(redisProperties.isSellerTestOnBorrow());
    jedisPoolConfig.setLifo(redisProperties.isSellerlifo());
    jedisPoolConfig.setMinEvictableIdleTimeMillis(redisProperties.getSellerMinEvictableIdleTimeMillis());
    jedisPoolConfig.setSoftMinEvictableIdleTimeMillis(redisProperties.getSellerSoftMinEvictableIdleTimeMillis());
    jedisPoolConfig.setNumTestsPerEvictionRun(redisProperties.getSellerNumTestsPerEvictionRun());
    jedisPoolConfig.setTestOnReturn(redisProperties.isSellerTestOnReturn());
    jedisPoolConfig.setTestWhileIdle(redisProperties.isSellerTestWhileIdle());
    jedisPoolConfig.setTimeBetweenEvictionRunsMillis(redisProperties.getSellerTimeBetweenEvictionRunsMillis());
    jedisPoolConfig.setJmxEnabled(redisProperties.isSellerJmxEnabled());
    jedisPoolConfig.setJmxNamePrefix(redisProperties.getSellerJmxNamePrefix());
    return jedisPoolConfig;
  }

  @Bean
  public RedisSentinelConfiguration sentinelConfiguration() {
      RedisSentinelConfiguration sentinelConfiguration = new RedisSentinelConfiguration();
      RedisNode sentinel = RedisNode.newRedisNode()
        .listeningAt(redisProperties.getRedisSentinelHost(), redisProperties.getRedisPort()).build();
      RedisNode master = RedisNode.newRedisNode()
        .withName(redisProperties.getRedisSentinelMaster()).build();
      master.setName(redisProperties.getRedisSentinelMaster());
      sentinelConfiguration.setMaster(master);
      sentinelConfiguration.addSentinel(sentinel);
    return sentinelConfiguration;
  }
}