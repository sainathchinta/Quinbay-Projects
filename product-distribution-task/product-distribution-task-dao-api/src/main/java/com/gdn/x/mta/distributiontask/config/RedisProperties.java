package com.gdn.x.mta.distributiontask.config;

import org.springframework.boot.context.properties.ConfigurationProperties;

import lombok.Data;

@Data
@ConfigurationProperties(value = "redis")
public class RedisProperties {

  private String hostName;

  private int port;

  private int timeout;

  private boolean usePool;

  private String password;

  private int dataBase;

  private int poolMinIdle;

  private int poolMaxIdle;

  private int poolMaxTotal;

  private int poolMaxWaitMillis;

  private String poolEvictionPolicyClassName;

  private boolean poolBlockWhenExhausted;

  private boolean poolTestOnBorrow;

  private boolean poolLifo;

  private long poolMinEvictableIdleTimeMillis;

  private long poolSoftMinEvictableIdleTimeMillis;

  private int poolNumTestsPerEvictionRun;

  private boolean poolTestOnReturn;

  private boolean poolTestWhileIdle;

  private long poolTimeBetweenEvictionRunsMillis;

  private boolean poolJmxEnabled;

  private String poolJmxNamePrefix;

  private long cacheDefaultExpiration;

  private boolean cacheUsePrefix;

  private long cacheExpirationHigherTtl;
  private long cacheExpirationMoreThanOneDayTtl;
}