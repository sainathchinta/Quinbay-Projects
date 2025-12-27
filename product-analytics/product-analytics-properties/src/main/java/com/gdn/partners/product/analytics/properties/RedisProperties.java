package com.gdn.partners.product.analytics.properties;

import lombok.Data;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.stereotype.Component;

@Component
@Data
@ConfigurationProperties(value = "redis")
public class RedisProperties {
  private String redisSentinelMaster;
  private String redisSentinelHost;
  private int redisPort;
  private int timeout;
  private boolean usePool;
  private int redisDatabase;
  private int minIdle;
  private int maxIdle;
  private int maxTotal;
  private int maxWaitMillis;
  private String evictionPolicyClassName;
  private boolean blockWhenExhausted;
  private boolean testOnBorrow;
  private boolean lifo;
  private long minEvictableIdleTimeMillis;
  private long softMinEvictableIdleTimeMillis;
  private int numTestsPerEvictionRun;
  private boolean testOnReturn;
  private boolean testWhileIdle;
  private long timeBetweenEvictionRunsMillis;
  private boolean jmxEnabled;
  private String jmxNamePrefix;
  private long redisCacheDefaultExpiration;
  private boolean usePrefix;
  private int sellerRedisTimeout;
  private boolean sellerRedisUsePool;
  private int sellerRedisDataBase;
  private int sellerRedisCacheDefaultExpiration;
  private boolean sellerRedisPrefix;
  private int sellerMinIdle;
  private int sellerMaxIdle;
  private int sellerMaxTotal;
  private long sellerMaxWaitMillis;
  private String sellerEvictionPolicyClassName;
  private boolean sellerBlockWhenExhausted;
  private boolean sellerTestOnBorrow;
  private boolean sellerlifo;
  private long sellerMinEvictableIdleTimeMillis;
  private long sellerSoftMinEvictableIdleTimeMillis;
  private int sellerNumTestsPerEvictionRun;
  private boolean sellerTestOnReturn;
  private boolean sellerTestWhileIdle;
  private long sellerTimeBetweenEvictionRunsMillis;
  private boolean sellerJmxEnabled;
  private String sellerJmxNamePrefix;
}