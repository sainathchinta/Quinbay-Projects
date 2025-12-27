package com.gdn.x.product.rest.web.config;

import java.time.Duration;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.cache.annotation.CachingConfigurer;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.interceptor.CacheErrorHandler;
import org.springframework.cache.interceptor.SimpleCacheResolver;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
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
import org.springframework.data.redis.serializer.RedisSerializer;
import com.gdn.x.base.serializer.KryoSerializer;
import com.gdn.x.config.CacheErrorHandlerAsMiss;
import com.gdn.x.product.rest.web.properties.RedisProperties;
import redis.clients.jedis.JedisPoolConfig;

@Configuration
@EnableCaching
public class RedisConfiguration implements CachingConfigurer {

  private final RedisProperties redisProperties;

  @Autowired
  public RedisConfiguration(RedisProperties redisProperties) {
    this.redisProperties = redisProperties;
  }

  @Bean(name = "stringRedisSerializer")
  public RedisSerializer stringRedisSerializer() {
    return new JdkSerializationRedisSerializer();
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


  @Bean(name = "stringRedisTemplate")
  @Primary
  public StringRedisTemplate stringRedisTemplate() {
    StringRedisTemplate stringRedisTemplate = new StringRedisTemplate();
    stringRedisTemplate.setConnectionFactory(lettuceConnectionFactory());
    stringRedisTemplate.setKeySerializer(stringRedisSerializer());
    return stringRedisTemplate;
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

  //Redis template definition
  @Bean(name = "redisTemplate")
  @Primary
  public <K, V> RedisTemplate<K, V> redisTemplate() {
    RedisTemplate<K, V> redisTemplate = new RedisTemplate<>();
    redisTemplate.setConnectionFactory(lettuceConnectionFactory());
    redisTemplate.setValueSerializer(kryoSerializer());
    redisTemplate.setKeySerializer(stringRedisSerializer());
    redisTemplate.afterPropertiesSet();
    return redisTemplate;
  }

  // Declare Redis Cache Manager
  @Bean(name = "cacheManager")
  @Primary
  public RedisCacheManager cacheManager() {
    return RedisCacheManager.builder(lettuceConnectionFactory()).cacheDefaults(
        RedisCacheConfiguration.defaultCacheConfig()
            .entryTtl(Duration.ofSeconds(redisProperties.getRedisCacheDefaultExpiration()))
            .computePrefixWith(CustomCacheKeyPrefix.simple())
            .serializeKeysWith(RedisSerializationContext.SerializationPair.fromSerializer(stringRedisSerializer()))
            .serializeValuesWith(RedisSerializationContext.SerializationPair.fromSerializer(kryoSerializer()))).build();
  }

  @Override
  public CacheErrorHandler errorHandler() {
    return new CacheErrorHandlerAsMiss();
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

  @Bean(name = "redisPoolConfigCategory")
  public JedisPoolConfig redisPoolConfigCategory() {
    JedisPoolConfig jedisPoolConfig = new JedisPoolConfig();
    jedisPoolConfig.setMinIdle(redisProperties.getRedisPoolCategoryMinIdle());
    jedisPoolConfig.setMaxIdle(redisProperties.getRedisPoolCategoryMaxIdle());
    jedisPoolConfig.setMaxTotal(redisProperties.getRedisPoolCategoryMaxTotal());
    jedisPoolConfig.setMaxWaitMillis(redisProperties.getRedisPoolCategoryMaxWaitMillis());
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

  @Bean("lettuceCategoryCombination")
  public LettuceConnectionFactory lettuceCategoryCombination() {
    RedisStandaloneConfiguration redisStandaloneConfiguration = new RedisStandaloneConfiguration();
    redisStandaloneConfiguration.setHostName(redisProperties.getRedisCategoryHostName());
    redisStandaloneConfiguration.setPort(redisProperties.getRedisCategoryPort());
    redisStandaloneConfiguration.setDatabase(redisProperties.getCategoryCombinationDatabase());

    LettuceClientConfiguration lettuceClientConfiguration =
        LettucePoolingClientConfiguration.builder().poolConfig(redisPoolConfigCategory())
            .commandTimeout(Duration.ofMillis(redisProperties.getCategoryTimeout())).build();

    return new LettuceConnectionFactory(redisStandaloneConfiguration, lettuceClientConfiguration);
  }

  // Redis Template Bean Definition
  @Bean(name = "redisTemplateCategoryCombination")
  public RedisTemplate redisTemplateCategoryCombination() {
    RedisTemplate redisTemplate = new RedisTemplate();
    redisTemplate.setConnectionFactory(lettuceCategoryCombination());
    redisTemplate.setKeySerializer(stringRedisSerializer());
    // defined in properties (configurable)
    redisTemplate.setValueSerializer(kryoSerializer());
    redisTemplate.afterPropertiesSet();
    return redisTemplate;
  }

  //Declare Redis Cache Manager -->
  @Bean(value = "cacheManagerCategoryCombination", autowireCandidate = false)
  public RedisCacheManager cacheManagerCategoryCombination() {
    return RedisCacheManager.builder(lettuceCategoryCombination()).cacheDefaults(
        RedisCacheConfiguration.defaultCacheConfig()
            .entryTtl(Duration.ofSeconds(redisProperties.getCategoryCacheExpiration()))
            .computePrefixWith(CustomCacheKeyPrefix.simple()).serializeKeysWith(RedisSerializationContext.SerializationPair.fromSerializer(stringRedisSerializer()))
            .serializeValuesWith(RedisSerializationContext.SerializationPair.fromSerializer(kryoSerializer()))).build();
  }

  @Bean(name = "cacheResolverCategoryCombination", autowireCandidate = false)
  public SimpleCacheResolver cacheResolverCategoryCombination() {
    SimpleCacheResolver simpleCacheResolver =
      new SimpleCacheResolver(cacheManagerCategoryCombination());
    return simpleCacheResolver;
  }

  /////////////////////// System parameter related cache /////////////////////////
  @Bean(name = "redisPoolConfigSystemParameter")
  public JedisPoolConfig redisPoolConfigSystemParameter() {
    JedisPoolConfig jedisPoolConfigSystemParameter = new JedisPoolConfig();
    jedisPoolConfigSystemParameter.setMinIdle(redisProperties.getParameterMinIdle());
    jedisPoolConfigSystemParameter.setMaxIdle(redisProperties.getParameterMaxIdle());
    jedisPoolConfigSystemParameter.setMaxTotal(redisProperties.getParameterMaxTotal());
    jedisPoolConfigSystemParameter.setMaxWaitMillis(redisProperties.getParameterMaxWaitMillis());
    jedisPoolConfigSystemParameter.setEvictionPolicyClassName(
      redisProperties.getEvictionPolicyClassName());
    jedisPoolConfigSystemParameter.setBlockWhenExhausted(redisProperties.isBlockWhenExhausted());
    jedisPoolConfigSystemParameter.setTestOnBorrow(redisProperties.isTestOnBorrow());
    jedisPoolConfigSystemParameter.setLifo(redisProperties.isLifo());
    jedisPoolConfigSystemParameter.setMinEvictableIdleTimeMillis(
      redisProperties.getMinEvictableIdleTimeMillis());
    jedisPoolConfigSystemParameter.setSoftMinEvictableIdleTimeMillis(
      redisProperties.getSoftMinEvictableIdleTimeMillis());
    jedisPoolConfigSystemParameter.setNumTestsPerEvictionRun(
      redisProperties.getNumTestsPerEvictionRun());
    jedisPoolConfigSystemParameter.setTestOnReturn(redisProperties.isTestOnReturn());
    jedisPoolConfigSystemParameter.setTestWhileIdle(redisProperties.isTestWhileIdle());
    jedisPoolConfigSystemParameter.setTimeBetweenEvictionRunsMillis(
      redisProperties.getTimeBetweenEvictionRunsMillis());
    jedisPoolConfigSystemParameter.setJmxEnabled(redisProperties.isJmxEnabled());
    jedisPoolConfigSystemParameter.setJmxNamePrefix(redisProperties.getJmxNamePrefix());
    return jedisPoolConfigSystemParameter;
  }

  @Bean(name = "lettuceSystemParameterCombination")
  public LettuceConnectionFactory lettuceSystemParameterCombination() {
    RedisStandaloneConfiguration redisStandaloneConfiguration = new RedisStandaloneConfiguration();
    redisStandaloneConfiguration.setHostName(redisProperties.getRedisSystemParameterHostName());
    redisStandaloneConfiguration.setPort(redisProperties.getRedisSystemParameterPort());
    redisStandaloneConfiguration.setDatabase(redisProperties.getParameterDatabase());

    LettuceClientConfiguration lettuceClientConfiguration =
        LettucePoolingClientConfiguration.builder().poolConfig(redisPoolConfigSystemParameter())
            .commandTimeout(Duration.ofMillis(redisProperties.getRedisSystemParameterTimeout())).build();

    return new LettuceConnectionFactory(redisStandaloneConfiguration, lettuceClientConfiguration);
  }

  // Redis Template Bean Definition
  @Bean(name = "redisTemplateSystemParameterCombination")
  public RedisTemplate redisTemplateSystemParameterCombination() {
    RedisTemplate redisTemplate = new RedisTemplate<>();
    redisTemplate.setConnectionFactory(lettuceSystemParameterCombination());
    redisTemplate.setKeySerializer(stringRedisSerializer());
    // redis.system.parameter.serializer (configurable)
    redisTemplate.setValueSerializer(kryoSerializer());
    redisTemplate.afterPropertiesSet();
    return redisTemplate;
  }

  // Declare Redis Cache Manager //
  @Bean(name = "cacheManagerSystemParameterCombination", autowireCandidate = false)
  public RedisCacheManager cacheManagerSystemParameterCombination() {
    return RedisCacheManager.builder(lettuceSystemParameterCombination()).cacheDefaults(
        RedisCacheConfiguration.defaultCacheConfig()
            .entryTtl(Duration.ofSeconds(redisProperties.getParameterDefaultExpiration()))
            .computePrefixWith(CustomCacheKeyPrefix.simple()).serializeKeysWith(RedisSerializationContext.SerializationPair.fromSerializer(stringRedisSerializer()))
            .serializeValuesWith(RedisSerializationContext.SerializationPair.fromSerializer(kryoSerializer()))).build();
  }

  @Bean(name = "cacheResolverSystemParameterCombination")
  public SimpleCacheResolver cacheResolverSystemParameterCombination() {
    return new SimpleCacheResolver(cacheManagerSystemParameterCombination());
  }

  /////////////////// Unique ID related cache /////////////////////////////////////
  @Bean(name = "redisPoolConfigUniqueIdCheck")
  public JedisPoolConfig redisPoolConfigUniqueIdCheck() {
    JedisPoolConfig jedisPoolConfigForUniqueId = new JedisPoolConfig();
    jedisPoolConfigForUniqueId.setMinIdle(redisProperties.getUniqueMinIdle());
    jedisPoolConfigForUniqueId.setMaxIdle(redisProperties.getUniqueMaxIdle());
    jedisPoolConfigForUniqueId.setMaxTotal(redisProperties.getUniqueMaxTotal());
    jedisPoolConfigForUniqueId.setMaxWaitMillis(redisProperties.getUniqueMaxWaitMillis());
    jedisPoolConfigForUniqueId.setEvictionPolicyClassName(
      redisProperties.getEvictionPolicyClassName());
    jedisPoolConfigForUniqueId.setBlockWhenExhausted(redisProperties.isBlockWhenExhausted());
    jedisPoolConfigForUniqueId.setTestOnBorrow(redisProperties.isTestOnBorrow());
    jedisPoolConfigForUniqueId.setLifo(redisProperties.isLifo());
    jedisPoolConfigForUniqueId.setMinEvictableIdleTimeMillis(
      redisProperties.getMinEvictableIdleTimeMillis());
    jedisPoolConfigForUniqueId.setSoftMinEvictableIdleTimeMillis(
      redisProperties.getSoftMinEvictableIdleTimeMillis());
    jedisPoolConfigForUniqueId.setNumTestsPerEvictionRun(
      redisProperties.getNumTestsPerEvictionRun());
    jedisPoolConfigForUniqueId.setTestOnReturn(redisProperties.isTestOnReturn());
    jedisPoolConfigForUniqueId.setTestWhileIdle(redisProperties.isTestWhileIdle());
    jedisPoolConfigForUniqueId.setTimeBetweenEvictionRunsMillis(
      redisProperties.getTimeBetweenEvictionRunsMillis());
    jedisPoolConfigForUniqueId.setJmxEnabled(redisProperties.isJmxEnabled());
    jedisPoolConfigForUniqueId.setJmxNamePrefix(redisProperties.getJmxNamePrefix());
    return jedisPoolConfigForUniqueId;
  }

  @Bean(name = "lettuceUniqueIdCheckCombination")
  public LettuceConnectionFactory lettuceUniqueIdCheckCombination() {
    RedisStandaloneConfiguration redisStandaloneConfiguration = new RedisStandaloneConfiguration();
    redisStandaloneConfiguration.setHostName(redisProperties.getRedisUniqueIdHostName());
    redisStandaloneConfiguration.setPort(redisProperties.getRedisUniqueIdPort());
    redisStandaloneConfiguration.setDatabase(redisProperties.getUniqueIdCombination());

    LettuceClientConfiguration lettuceClientConfiguration =
        LettucePoolingClientConfiguration.builder().poolConfig(redisPoolConfigUniqueIdCheck())
            .commandTimeout(Duration.ofMillis(redisProperties.getUniqueIdTimeout())).build();

    return new LettuceConnectionFactory(redisStandaloneConfiguration, lettuceClientConfiguration);
  }

  //Redis template definition for Unique ID
  @Bean(name = "redisTemplateUniqueIdCheckCombination")
  public RedisTemplate redisTemplateUniqueIdCheckCombination() {
    RedisTemplate redisTemplate = new RedisTemplate();
    redisTemplate.setConnectionFactory(lettuceUniqueIdCheckCombination());
    redisTemplate.setKeySerializer(stringRedisSerializer());
    //ref= redis.unique.id.serializer
    redisTemplate.setValueSerializer(kryoSerializer());
    return redisTemplate;
  }

  //Declare Redis Cache Manager for Unique Id
  @Bean(name = "cacheManagerUniqueIdCheckCombination")
  public RedisCacheManager cacheManagerUniqueIdCheckCombination() {
    return RedisCacheManager.builder(lettuceUniqueIdCheckCombination()).cacheDefaults(
        RedisCacheConfiguration.defaultCacheConfig()
            .entryTtl(Duration.ofSeconds(redisProperties.getUniqueIdExpiration()))
            .computePrefixWith(CustomCacheKeyPrefix.simple()).serializeKeysWith(RedisSerializationContext.SerializationPair.fromSerializer(stringRedisSerializer()))
            .serializeValuesWith(RedisSerializationContext.SerializationPair.fromSerializer(kryoSerializer()))).build();
  }

  @Bean(name = "cacheResolverUniqueIdCheckCombination")
  public SimpleCacheResolver cacheResolverUniqueIdCheckCombination() {
    return new SimpleCacheResolver(cacheManagerUniqueIdCheckCombination());
  }

  ///////////////////    Product Related Cache   ///////////////////////////////////////
  @Bean(name = "redisPoolConfigProduct")
  public JedisPoolConfig redisPoolConfigProduct() {
    JedisPoolConfig jedisPoolConfig = redisPoolConfig();
    jedisPoolConfig.setMinIdle(redisProperties.getProductMinIdle());
    jedisPoolConfig.setMaxIdle(redisProperties.getProductMaxIdle());
    jedisPoolConfig.setMaxTotal(redisProperties.getProductMaxTotal());
    jedisPoolConfig.setMaxWaitMillis(redisProperties.getProductMaxWaitMillis());
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

  @Bean(name = "lettuceProductCombination")
  public LettuceConnectionFactory lettuceProductCombination() {
    RedisStandaloneConfiguration redisStandaloneConfiguration = new RedisStandaloneConfiguration();
    redisStandaloneConfiguration.setHostName(redisProperties.getRedisProductHostName());
    redisStandaloneConfiguration.setPort(redisProperties.getRedisProductPort());
    redisStandaloneConfiguration.setDatabase(redisProperties.getProductCombination());

    LettuceClientConfiguration lettuceClientConfiguration =
        LettucePoolingClientConfiguration.builder().poolConfig(redisPoolConfigProduct())
            .commandTimeout(Duration.ofMillis(redisProperties.getProductTimeout())).build();

    return new LettuceConnectionFactory(redisStandaloneConfiguration, lettuceClientConfiguration);
  }

  //redis template definition For product Combination
  @Bean("redisTemplateProductCombination")
  public RedisTemplate redisTemplateProductCombination() {
    RedisTemplate redisTemplate = new RedisTemplate();
    redisTemplate.setConnectionFactory(lettuceProductCombination());
    redisTemplate.setKeySerializer(stringRedisSerializer());
    redisTemplate.setValueSerializer(kryoSerializer());
    redisTemplate.afterPropertiesSet();
    return redisTemplate;
  }

  //declare Redis Cache Manager
  @Bean(name = "cacheManagerProductCombination", autowireCandidate = false)
  public RedisCacheManager cacheManagerProductCombination() {
    return RedisCacheManager.builder(lettuceProductCombination()).cacheDefaults(
        RedisCacheConfiguration.defaultCacheConfig().entryTtl(Duration.ofSeconds(redisProperties.getProductExpiration()))
            .computePrefixWith(CustomCacheKeyPrefix.simple()).serializeKeysWith(RedisSerializationContext.SerializationPair.fromSerializer(stringRedisSerializer()))
            .serializeValuesWith(RedisSerializationContext.SerializationPair.fromSerializer(kryoSerializer()))).build();
  }

  @Bean(name = "cacheResolverProductCombination")
  public SimpleCacheResolver cacheResolverProductCombination() {
    return new SimpleCacheResolver(cacheManagerProductCombination());
  }

  ////////////////// Product Sync Related Cache ////////////////////
  @Bean(name = "redisPoolConfigProductSync")
  public JedisPoolConfig redisPoolConfigProductSync() {
    JedisPoolConfig jedisPoolConfigForProductSync = redisPoolConfig();
    jedisPoolConfigForProductSync.setMinIdle(redisProperties.getSyncMinIdle());
    jedisPoolConfigForProductSync.setMaxIdle(redisProperties.getSyncMaxIdle());
    jedisPoolConfigForProductSync.setMaxTotal(redisProperties.getSyncMaxTotal());
    jedisPoolConfigForProductSync.setMaxWaitMillis(redisProperties.getSyncMaxWaitMillis());
    jedisPoolConfigForProductSync.setEvictionPolicyClassName(
      redisProperties.getEvictionPolicyClassName());
    jedisPoolConfigForProductSync.setBlockWhenExhausted(redisProperties.isBlockWhenExhausted());
    jedisPoolConfigForProductSync.setTestOnBorrow(redisProperties.isTestOnBorrow());
    jedisPoolConfigForProductSync.setLifo(redisProperties.isLifo());
    jedisPoolConfigForProductSync.setMinEvictableIdleTimeMillis(
      redisProperties.getMinEvictableIdleTimeMillis());
    jedisPoolConfigForProductSync.setSoftMinEvictableIdleTimeMillis(
      redisProperties.getSoftMinEvictableIdleTimeMillis());
    jedisPoolConfigForProductSync.setNumTestsPerEvictionRun(
      redisProperties.getNumTestsPerEvictionRun());
    jedisPoolConfigForProductSync.setTestOnReturn(redisProperties.isTestOnReturn());
    jedisPoolConfigForProductSync.setTestWhileIdle(redisProperties.isTestWhileIdle());
    jedisPoolConfigForProductSync.setTimeBetweenEvictionRunsMillis(
      redisProperties.getTimeBetweenEvictionRunsMillis());
    jedisPoolConfigForProductSync.setJmxEnabled(redisProperties.isJmxEnabled());
    jedisPoolConfigForProductSync.setJmxNamePrefix(redisProperties.getJmxNamePrefix());
    return jedisPoolConfigForProductSync;
  }

  @Bean(name = "lettuceProductSyncCombination")
  public LettuceConnectionFactory lettuceProductSyncCombination() {
    RedisStandaloneConfiguration redisStandaloneConfiguration = new RedisStandaloneConfiguration();
    redisStandaloneConfiguration.setHostName(redisProperties.getRedisProductHostName());
    redisStandaloneConfiguration.setPort(redisProperties.getRedisProductSyncPort());
    redisStandaloneConfiguration.setDatabase(redisProperties.getSyncCombination());

    LettuceClientConfiguration lettuceClientConfiguration =
        LettucePoolingClientConfiguration.builder().poolConfig(redisPoolConfigProductSync())
            .commandTimeout(Duration.ofMillis(redisProperties.getSyncTimeout())).build();

    return new LettuceConnectionFactory(redisStandaloneConfiguration, lettuceClientConfiguration);
  }

  // Redis Template
  @Bean(name = "redisTemplateProductSyncCombination")
  public RedisTemplate<String, Object> redisTemplateProductSyncCombination() {
    RedisTemplate<String, Object> redisTemplate = new RedisTemplate<>();
    redisTemplate.setConnectionFactory(lettuceProductSyncCombination());
    redisTemplate.setKeySerializer(stringRedisSerializer());
    redisTemplate.setValueSerializer(kryoSerializer());
    redisTemplate.afterPropertiesSet();
    return redisTemplate;
  }

  //Declare Redis Cache Manager

  @Bean(name = "cacheManagerProductSyncCombination")
  public RedisCacheManager cacheManagerProductSyncCombination() {
    return RedisCacheManager.builder(lettuceProductSyncCombination()).cacheDefaults(
        RedisCacheConfiguration.defaultCacheConfig().entryTtl(Duration.ofSeconds(redisProperties.getSyncExpiration()))
            .computePrefixWith(CustomCacheKeyPrefix.simple()).serializeKeysWith(RedisSerializationContext.SerializationPair.fromSerializer(stringRedisSerializer()))
            .serializeValuesWith(RedisSerializationContext.SerializationPair.fromSerializer(kryoSerializer()))).build();
  }

  @Bean(name = "cacheResolverProductSyncCombination")
  public SimpleCacheResolver cacheResolverProductSyncCombination() {
    return new SimpleCacheResolver(cacheManagerProductSyncCombination());
  }

  // Product SKU related cache
  @Bean(name = "redisPoolConfigProductSku")
  public JedisPoolConfig redisPoolConfigProductSku() {
    JedisPoolConfig jedisPoolConfigForProductSku = new JedisPoolConfig();
    jedisPoolConfigForProductSku.setMinIdle(redisProperties.getSkuMinIdle());
    jedisPoolConfigForProductSku.setMaxIdle(redisProperties.getSyncMaxIdle());
    jedisPoolConfigForProductSku.setMaxTotal(redisProperties.getMaxTotal());
    jedisPoolConfigForProductSku.setMaxWaitMillis(redisProperties.getSkuMaxWaitMillis());
    jedisPoolConfigForProductSku.setEvictionPolicyClassName(
      redisProperties.getEvictionPolicyClassName());
    jedisPoolConfigForProductSku.setBlockWhenExhausted(redisProperties.isBlockWhenExhausted());
    jedisPoolConfigForProductSku.setTestOnBorrow(redisProperties.isTestOnBorrow());
    jedisPoolConfigForProductSku.setLifo(redisProperties.isLifo());
    jedisPoolConfigForProductSku.setMinEvictableIdleTimeMillis(
      redisProperties.getMinEvictableIdleTimeMillis());
    jedisPoolConfigForProductSku.setSoftMinEvictableIdleTimeMillis(
      redisProperties.getSoftMinEvictableIdleTimeMillis());
    jedisPoolConfigForProductSku.setNumTestsPerEvictionRun(
      redisProperties.getNumTestsPerEvictionRun());
    jedisPoolConfigForProductSku.setTestOnReturn(redisProperties.isTestOnReturn());
    jedisPoolConfigForProductSku.setTestWhileIdle(redisProperties.isTestWhileIdle());
    jedisPoolConfigForProductSku.setTimeBetweenEvictionRunsMillis(
      redisProperties.getTimeBetweenEvictionRunsMillis());
    jedisPoolConfigForProductSku.setJmxEnabled(redisProperties.isJmxEnabled());
    jedisPoolConfigForProductSku.setJmxNamePrefix(redisProperties.getJmxNamePrefix());
    return jedisPoolConfigForProductSku;
  }


  @Bean(name = "lettuceProductSkuCombination")
  public LettuceConnectionFactory lettuceProductSkuCombination() {
    RedisStandaloneConfiguration redisStandaloneConfiguration = new RedisStandaloneConfiguration();
    redisStandaloneConfiguration.setHostName(redisProperties.getRedisProductSkuHostName());
    redisStandaloneConfiguration.setPort(redisProperties.getRedisProductSkuPort());
    redisStandaloneConfiguration.setDatabase(redisProperties.getSkuCombination());

    LettuceClientConfiguration lettuceClientConfiguration =
        LettucePoolingClientConfiguration.builder().poolConfig(redisPoolConfigProductSku())
            .commandTimeout(Duration.ofMillis(redisProperties.getSyncTimeout())).build();

    return new LettuceConnectionFactory(redisStandaloneConfiguration, lettuceClientConfiguration);
  }

  // Redis Template For Product Sku
  @Bean(name = "redisTemplateProductSkuCombination")
  public RedisTemplate redisTemplateProductSkuCombination() {
    RedisTemplate redisTemplate = new RedisTemplate();
    redisTemplate.setConnectionFactory(lettuceProductSkuCombination());
    redisTemplate.setKeySerializer(stringRedisSerializer());
    redisTemplate.setValueSerializer(kryoSerializer());
    redisTemplate.afterPropertiesSet();
    return redisTemplate;
  }

  // Redis Cache Manager for product Sku
  @Bean(name = "cacheManagerProductSkuCombination")
  public RedisCacheManager cacheManagerProductSkuCombination() {
    return RedisCacheManager.builder(lettuceProductSkuCombination()).cacheDefaults(
        RedisCacheConfiguration.defaultCacheConfig()
            .entryTtl(Duration.ofSeconds(redisProperties.getCacheSkuExpiration()))
            .computePrefixWith(CustomCacheKeyPrefix.simple()).serializeKeysWith(RedisSerializationContext.SerializationPair.fromSerializer(stringRedisSerializer()))
            .serializeValuesWith(RedisSerializationContext.SerializationPair.fromSerializer(kryoSerializer()))).build();
  }

  @Bean(name = "cacheResolverProductSkuCombination")
  public SimpleCacheResolver cacheResolverProductSkuCombination() {
    return new SimpleCacheResolver(cacheManagerProductSkuCombination());
  }

  // Master Data Related Cache
  @Bean(name = "redisPoolConfigMasterData")
  public JedisPoolConfig redisPoolConfigMasterData() {
    JedisPoolConfig jedisPoolConfig = new JedisPoolConfig();
    jedisPoolConfig.setMinIdle(redisProperties.getDataMinIdle());
    jedisPoolConfig.setMaxIdle(redisProperties.getDataMaxIdle());
    jedisPoolConfig.setMaxTotal(redisProperties.getDataMaxTotal());
    jedisPoolConfig.setMaxWaitMillis(redisProperties.getDataMaxWaitMillis());
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

  @Bean(name = "lettuceMasterDataCombination")
  public LettuceConnectionFactory lettuceMasterDataCombination() {
    RedisStandaloneConfiguration redisStandaloneConfiguration = new RedisStandaloneConfiguration();
    redisStandaloneConfiguration.setHostName(redisProperties.getRedisMasterDataHostName());
    redisStandaloneConfiguration.setPort(redisProperties.getRedisMasterDataPort());
    redisStandaloneConfiguration.setDatabase(redisProperties.getDataCombination());

    LettuceClientConfiguration lettuceClientConfiguration =
        LettucePoolingClientConfiguration.builder().poolConfig(redisPoolConfigMasterData())
            .commandTimeout(Duration.ofMillis(redisProperties.getRedisMasterDataTimeout())).build();

    return new LettuceConnectionFactory(redisStandaloneConfiguration, lettuceClientConfiguration);
  }

  @Bean(name = "redisTemplateMasterDataCombination")
  public RedisTemplate redisTemplateMasterDataCombination() {
    RedisTemplate redisTemplate = new RedisTemplate();
    redisTemplate.setConnectionFactory(lettuceMasterDataCombination());
    redisTemplate.setKeySerializer(stringRedisSerializer());
    redisTemplate.setValueSerializer(kryoSerializer());
    redisTemplate.afterPropertiesSet();
    return redisTemplate;
  }

  // Redis Cache manager for Master Data
  @Bean(name = "cacheManagerMasterDataCombination", autowireCandidate = false)
  public RedisCacheManager cacheManagerMasterDataCombination() {
    return RedisCacheManager.builder(lettuceMasterDataCombination()).cacheDefaults(
        RedisCacheConfiguration.defaultCacheConfig().entryTtl(Duration.ofSeconds(redisProperties.getDataExpiration()))
            .computePrefixWith(CustomCacheKeyPrefix.simple()).serializeKeysWith(RedisSerializationContext.SerializationPair.fromSerializer(stringRedisSerializer()))
            .serializeValuesWith(RedisSerializationContext.SerializationPair.fromSerializer(kryoSerializer()))).build();
  }

  @Bean(name = "cacheResolverMasterDataCombination")
  public SimpleCacheResolver cacheResolverMasterDataCombination() {
    return new SimpleCacheResolver(cacheManagerMasterDataCombination());
  }

  //------------------------------------------------------------------------------------------------------------------------------
  // Redis product count cache connection pool config
  @Bean(name = "redisPoolConfigProductCounts")
  public JedisPoolConfig redisPoolConfigProductCounts() {
    JedisPoolConfig jedisPoolConfig = new JedisPoolConfig();
    jedisPoolConfig.setMinIdle(redisProperties.getRedisPoolProductCountMinIdle());
    jedisPoolConfig.setMaxIdle(redisProperties.getRedisPoolProductCountMaxIdle());
    jedisPoolConfig.setMaxTotal(redisProperties.getRedisPoolProductCountMaxTotal());
    jedisPoolConfig.setMaxWaitMillis(redisProperties.getRedisPoolProductCountMaxWaitMillis());
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

  // Redis product count cache lettuce connection configuration
  @Bean("lettuceProductCountsCombination")
  public LettuceConnectionFactory lettuceProductCountsCombination() {
    RedisStandaloneConfiguration redisStandaloneConfiguration = new RedisStandaloneConfiguration();
    redisStandaloneConfiguration.setHostName(redisProperties.getRedisProductCountHostName());
    redisStandaloneConfiguration.setPort(redisProperties.getRedisProductCountPort());
    redisStandaloneConfiguration.setDatabase(redisProperties.getRedisProductCountDatabase());

    LettuceClientConfiguration lettuceClientConfiguration =
        LettucePoolingClientConfiguration.builder()
            .poolConfig(redisPoolConfigProductCounts())
            .commandTimeout(Duration.ofMillis(redisProperties.getRedisProductCountTimeoutInMillis()))
            .build();

    return new LettuceConnectionFactory(redisStandaloneConfiguration, lettuceClientConfiguration);
  }

  // Redis product count cache redis template
  @Bean(name = "redisTemplateProductCountsCombination")
  public RedisTemplate redisTemplateProductCountsCombination() {
    RedisTemplate redisTemplate = new RedisTemplate();
    redisTemplate.setConnectionFactory(lettuceProductCountsCombination());
    redisTemplate.setKeySerializer(stringRedisSerializer());
    redisTemplate.setValueSerializer(kryoSerializer());
    redisTemplate.afterPropertiesSet();
    return redisTemplate;
  }

  // Redis product count cache redis cache manager
  @Bean(value = "cacheManagerProductCountsCombination", autowireCandidate = false)
  public RedisCacheManager cacheManagerProductCountsCombination() {
    return RedisCacheManager.builder(lettuceProductCountsCombination())
        .cacheDefaults(RedisCacheConfiguration
            .defaultCacheConfig()
            .entryTtl(Duration.ofSeconds(redisProperties.getRedisProductCountCacheExpirationInSecs()))
            .computePrefixWith(CustomCacheKeyPrefix.simple())
            .serializeKeysWith(RedisSerializationContext.SerializationPair.fromSerializer(stringRedisSerializer()))
            .serializeValuesWith(RedisSerializationContext.SerializationPair.fromSerializer(kryoSerializer()))
        )
        .build();
  }

  // Redis product count cache redis cache resolver
  @Bean(name = "cacheResolverProductCountCombination", autowireCandidate = false)
  public SimpleCacheResolver cacheResolverProductCountCombination() {
    SimpleCacheResolver simpleCacheResolver =
        new SimpleCacheResolver(cacheManagerProductCountsCombination());
    return simpleCacheResolver;
  }
  //------------------------------------------------------------------------------------------------------------------------------

  // Size chart detail redis cache resolver

  @Bean(name = "cacheResolverSizeChartDetailCombination", autowireCandidate = false)
  public SimpleCacheResolver cacheResolverSizeChartDetailCombination() {
    return new SimpleCacheResolver(cacheManagerSizeChartDetailCombination());
  }

  @Bean(value = "cacheManagerSizeChartDetailCombination", autowireCandidate = false)
  public RedisCacheManager cacheManagerSizeChartDetailCombination() {
    return RedisCacheManager.builder(lettuceSizeChartDetailCombination())
        .cacheDefaults(RedisCacheConfiguration
            .defaultCacheConfig()
            .entryTtl(Duration.ofSeconds(redisProperties.getRedisSizeChartCacheExpirationInSecs()))
            .computePrefixWith(CustomCacheKeyPrefix.simple())
            .serializeKeysWith(RedisSerializationContext.SerializationPair.fromSerializer(stringRedisSerializer()))
            .serializeValuesWith(RedisSerializationContext.SerializationPair.fromSerializer(kryoSerializer()))
        )
        .build();
  }

  @Bean("lettuceSizeChartDetailCombination")
  public LettuceConnectionFactory lettuceSizeChartDetailCombination() {
    RedisStandaloneConfiguration redisStandaloneConfiguration = new RedisStandaloneConfiguration();
    redisStandaloneConfiguration.setHostName(redisProperties.getRedisSizeChartHostName());
    redisStandaloneConfiguration.setPort(redisProperties.getRedisSizeChartPort());
    redisStandaloneConfiguration.setDatabase(redisProperties.getRedisSizeChartDatabase());
    LettuceClientConfiguration lettuceClientConfiguration =
        LettucePoolingClientConfiguration.builder()
            .poolConfig(redisPoolConfigSizeChartDetail())
            .commandTimeout(Duration.ofMillis(redisProperties.getRedisSizeChartTimeoutInMillis()))
            .build();
    return new LettuceConnectionFactory(redisStandaloneConfiguration, lettuceClientConfiguration);
  }

  @Bean(name = "redisPoolConfigSizeChartDetail")
  public JedisPoolConfig redisPoolConfigSizeChartDetail() {
    JedisPoolConfig jedisPoolConfig = new JedisPoolConfig();
    jedisPoolConfig.setMinIdle(redisProperties.getRedisPoolSizeChartMinIdle());
    jedisPoolConfig.setMaxIdle(redisProperties.getRedisPoolSizeChartMaxIdle());
    jedisPoolConfig.setMaxTotal(redisProperties.getRedisPoolSizeChartMaxTotal());
    jedisPoolConfig.setMaxWaitMillis(redisProperties.getRedisPoolSizeChartMaxWaitMillis());
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
  //----------------------------------------------------------------------------------------------
  // Item pickup point redis cache resolver

  @Bean(name = "cacheResolverItemPickupPointCombination", autowireCandidate = false)
  public SimpleCacheResolver cacheResolverItemPickupPointCombination() {
    return new SimpleCacheResolver(cacheManagerItemPickupPointCombination());
  }

  @Bean(value = "cacheManagerItemPickupPointCombination", autowireCandidate = false)
  public RedisCacheManager cacheManagerItemPickupPointCombination() {
    return RedisCacheManager.builder(lettuceItemPickupPointCombination()).cacheDefaults(
      RedisCacheConfiguration.defaultCacheConfig().entryTtl(
          Duration.ofSeconds(redisProperties.getRedisItemPickupPointCacheExpirationInSecs()))
        .computePrefixWith(CustomCacheKeyPrefix.simple()).serializeKeysWith(
          RedisSerializationContext.SerializationPair.fromSerializer(stringRedisSerializer()))
        .serializeValuesWith(
          RedisSerializationContext.SerializationPair.fromSerializer(kryoSerializer()))).build();
  }

  @Bean("lettuceItemPickupPointCombination")
  public LettuceConnectionFactory lettuceItemPickupPointCombination() {
    RedisStandaloneConfiguration redisStandaloneConfiguration = new RedisStandaloneConfiguration();
    redisStandaloneConfiguration.setHostName(redisProperties.getRedisItemPickupPointHostName());
    redisStandaloneConfiguration.setPort(redisProperties.getRedisItemPickupPointPort());
    redisStandaloneConfiguration.setDatabase(redisProperties.getRedisItemPickupPointDatabase());
    LettuceClientConfiguration lettuceClientConfiguration =
      LettucePoolingClientConfiguration.builder().poolConfig(redisPoolConfigItemPickupPoint())
        .commandTimeout(Duration.ofMillis(redisProperties.getRedisItemPickupPointTimeoutInMillis()))
        .build();
    return new LettuceConnectionFactory(redisStandaloneConfiguration, lettuceClientConfiguration);
  }

  @Bean(name = "redisPoolConfigItemPickupPoint")
  public JedisPoolConfig redisPoolConfigItemPickupPoint() {
    JedisPoolConfig jedisPoolConfig = new JedisPoolConfig();
    jedisPoolConfig.setMinIdle(redisProperties.getRedisPoolItemPickupPointMinIdle());
    jedisPoolConfig.setMaxIdle(redisProperties.getRedisPoolItemPickupPointMaxIdle());
    jedisPoolConfig.setMaxTotal(redisProperties.getRedisPoolItemPickupPointMaxTotal());
    jedisPoolConfig.setMaxWaitMillis(redisProperties.getRedisPoolItemPickupPointMaxWaitMillis());
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
}