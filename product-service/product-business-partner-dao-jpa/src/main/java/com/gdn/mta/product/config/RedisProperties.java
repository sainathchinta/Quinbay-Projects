package com.gdn.mta.product.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import lombok.Getter;

@Component
@Getter
public class RedisProperties {

  @Value("${redis.hostName}")
  private String redisHostName;

  @Value("${redis.port}")
  private int redisPort;

  @Value("${redis.timeout}")
  private int timeout;

  @Value("${redis.usePool}")
  private boolean usePool;

  @Value("${redis.password}")
  private String redisPassword;

  @Value("${redis.database}")
  private int redisDataBase;

  @Value("${redis.pool.minIdle}")
  private int minIdle;

  @Value("${redis.pool.maxIdle}")
  private int maxIdle;

  @Value("${redis.pool.maxTotal}")
  private int maxTotal;

  @Value("${redis.pool.maxWaitMillis}")
  private int maxWaitMillis;

  @Value("${redis.pool.evictionPolicyClassName}")
  private String evictionPolicyClassName;

  @Value("${redis.pool.blockWhenExhausted}")
  private boolean blockWhenExhausted;

  @Value("${redis.pool.testOnBorrow}")
  private boolean testOnBorrow;

  @Value("${redis.pool.lifo}")
  private boolean lifo;

  @Value("${redis.pool.minEvictableIdleTimeMillis}")
  private long minEvictableIdleTimeMillis;

  @Value("${redis.pool.softMinEvictableIdleTimeMillis}")
  private long softMinEvictableIdleTimeMillis;

  @Value("${redis.pool.numTestsPerEvictionRun}")
  private int numTestsPerEvictionRun;

  @Value("${redis.pool.testOnReturn}")
  private boolean testOnReturn;

  @Value("${redis.pool.testWhileIdle}")
  private boolean testWhileIdle;

  @Value("${redis.pool.timeBetweenEvictionRunsMillis}")
  private long timeBetweenEvictionRunsMillis;

  @Value("${redis.pool.jmxEnabled}")
  private boolean jmxEnabled;

  @Value("${redis.pool.jmxNamePrefix}")
  private String jmxNamePrefix;

  @Value("${redis.cache.defaultExpiration}")
  private long defaultExpiration;

  @Value("${redis.cache.usePrefix}")
  private boolean usePrefix;

  @Value("${redis.product.limit.ttl.in.seconds}")
  private long productLimitsTtl;

  @Value("${redis.product.level3.count.ttl.in.minutes}")
  private long productLevel3CountTimeToLiveInMinutes;

}
