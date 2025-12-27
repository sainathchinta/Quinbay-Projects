package com.gdn.x.product.rest.web.properties;

import lombok.Getter;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
@Getter
public class RedisProperties {
  @Value("${application.base.httpclient.total.max:2000}")
  private int maxTotal;

  @Value("${redis.pool.minIdle}")
  private int minIdle;

  @Value("${redis.hostName}")
  private String redisHostName;

  @Value("${redis.port}")
  private int redisPort;

  @Value("${redis.database}")
  private int redisDataBase;

  @Value("${redis.cache.defaultExpiration}")
  private int redisCacheDefaultExpiration;

  @Value("${redis.pool.category.minIdle}")
  private int redisPoolCategoryMinIdle;

  @Value("${redis.pool.category.maxIdle}")
  private int redisPoolCategoryMaxIdle;

  @Value("${redis.pool.category.maxTotal}")
  private int redisPoolCategoryMaxTotal;

  @Value("${redis.pool.category.maxWaitMillis}")
  private int redisPoolCategoryMaxWaitMillis;

  @Value("${redis.category.hostName}")
  private String redisCategoryHostName;

  @Value("${redis.category.port}")
  private int redisCategoryPort;

  @Value("${redis.category.timeout}")
  private int categoryTimeout;

  @Value("${redis.category.password}")
  private String redisCategoryPassword;

  @Value("${redis.database.category.combination}")
  private int CategoryCombinationDatabase;

  @Value("${redis.category.serializer}")
  private String categoryValueSerializer;

  @Value("${redis.cache.category.expiration}")
  private long categoryCacheExpiration;

  @Value("${redis.pool.maxIdle}")
  private int maxIdle;

  @Value("${redis.system.parameter.hostName}")
  private String redisSystemParameterHostName;

  @Value("${redis.system.parameter.port}")
  private int redisSystemParameterPort;

  @Value("${redis.pool.maxTotal}")
  private String redisMaxTotal;

  @Value("${redis.system.parameter.password}")
  private String redisSystemParameterPassword;

  @Value("${redis.unique.id.hostName}")
  private String redisUniqueIdHostName;

  @Value("${redis.unique.id.port}")
  private int redisUniqueIdPort;

  @Value("${redis.product.hostName}")
  private String redisProductHostName;

  @Value("${redis.product.port}")
  private int redisProductPort;

  @Value("${redis.product.timeout}")
  private int productTimeout;

  @Value("${redis.product.serializer}")
  private String productValueSerializer;

  @Value("${redis.product.password}")
  private String redisProductPassword;

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

  @Value("${redis.pool.system.parameter.minIdle}")
  private int parameterMinIdle;

  @Value("${redis.pool.system.parameter.maxIdle}")
  private int parameterMaxIdle;

  @Value("${redis.pool.system.parameter.maxTotal}")
  private int parameterMaxTotal;

  @Value("${redis.pool.system.parameter.maxWaitMillis}")
  private int parameterMaxWaitMillis;

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

  @Value("${redis.timeout}")
  private int timeout;

  @Value("${redis.usePool}")
  private boolean usePool;

  @Value("${redis.serializer}")
  private String valueSerializer;


  @Value("${redis.cache.usePrefix}")
  private boolean usePrefix;

  @Value("${redis.system.parameter.timeout}")
  private int redisSystemParameterTimeout;

  @Value("${redis.database.system.parameter.combination}")
  private int parameterDatabase;

  @Value("${redis.system.parameter.serializer}")
  private String parameterValueSerializer;

  @Value("${redis.cache.system.parameter.expiration}")
  private long parameterDefaultExpiration;

  @Value("${redis.pool.unique.id.minIdle}")
  private int uniqueMinIdle;

  @Value("${redis.pool.unique.id.maxIdle}")
  private int uniqueMaxIdle;

  @Value("${redis.pool.unique.id.maxTotal}")
  private int uniqueMaxTotal;

  @Value("${redis.pool.unique.id.maxWaitMillis}")
  private int uniqueMaxWaitMillis;

  @Value("${redis.unique.id.timeout}")
  private int uniqueIdTimeout;

  @Value("${redis.database.unique.id.combination}")
  private int uniqueIdCombination;

  @Value("${redis.unique.id.serializer}")
  private String uniqueIdSerializer;

  @Value("${redis.cache.unique.id.expiration}")
  private long uniqueIdExpiration;

  @Value("${redis.pool.product.minIdle}")
  private int productMinIdle;

  @Value("${redis.pool.product.maxIdle}")
  private int productMaxIdle;

  @Value("${redis.pool.product.maxTotal}")
  private int productMaxTotal;

  @Value("${redis.pool.product.maxWaitMillis}")
  private int productMaxWaitMillis;


  @Value("${redis.database.product.combination}")
  private int productCombination;

  @Value("${redis.cache.product.expiration}")
  private long productExpiration;

  @Value("${redis.pool.product.sync.minIdle}")
  private int syncMinIdle;

  @Value("${redis.pool.product.sync.maxIdle}")
  private int syncMaxIdle;

  @Value("${redis.pool.product.sync.maxTotal}")
  private int syncMaxTotal;

  @Value("${redis.pool.product.sync.maxWaitMillis}")
  private int syncMaxWaitMillis;

  @Value("${redis.product.sync.hostName}")
  private String redisProductSyncHostName;

  @Value("${redis.product.sync.timeout}")
  private int syncTimeout;

  @Value("${redis.product.sync.port}")
  private int redisProductSyncPort;

  @Value("${redis.product.sync.password}")
  private String redisProductSyncPassword;

  @Value("${redis.database.product.sync.combination}")
  private int syncCombination;

  @Value("${redis.product.sync.serializer}")
  private String syncSerializer;

  @Value("${redis.cache.product.sync.expiration}")
  private long syncExpiration;

  @Value("${redis.product.sku.hostName}")
  private String redisProductSkuHostName;

  @Value("${redis.product.sku.port}")
  private int redisProductSkuPort;

  @Value("${redis.product.sku.timeout}")
  private int redisProductSkuSyncTimeout;

  @Value("${redis.product.sku.password}")
  private String redisProductSkuPassword;

  @Value("${redis.pool.product.sku.minIdle}")
  private int skuMinIdle;

  @Value("${redis.pool.product.sku.maxIdle}")
  private int skuMaxIdle;

  @Value("${redis.pool.product.sku.maxTotal}")
  private int skuMaxTotal;

  @Value("${redis.pool.product.sku.maxWaitMillis}")
  private int skuMaxWaitMillis;

  @Value("${redis.product.sku.timeout}")
  private int skuTimeout;

  @Value("${redis.database.product.sku.combination}")
  private int skuCombination;

  @Value("${redis.product.sku.serializer}")
  private String skuValueSerializer;

  @Value("${redis.cache.product.sku.expiration}")
  private long cacheSkuExpiration;

  @Value("${redis.master.data.hostName}")
  private String redisMasterDataHostName;

  @Value("${redis.master.data.port}")
  private int redisMasterDataPort;

  @Value("${redis.pool.master.data.minIdle}")
  private int dataMinIdle;

  @Value("${redis.pool.master.data.maxIdle}")
  private int dataMaxIdle;

  @Value("${redis.pool.master.data.maxTotal}")
  private int dataMaxTotal;

  @Value("${redis.pool.master.data.maxWaitMillis}")
  private int dataMaxWaitMillis;

  @Value("${redis.master.data.timeout}")
  private int redisMasterDataTimeout;

  @Value("${redis.database.master.data.combination}")
  private int dataCombination;

  @Value("${redis.master.data.serializer}")
  private String dataValueSerializer;

  @Value("${redis.cache.master.data.expiration}")
  private long dataExpiration;

  //product count cache properties
  @Value("${redis.product.count.hostName}")
  private String redisProductCountHostName;

  @Value("${redis.product.count.port}")
  private int redisProductCountPort;

  @Value("${redis.product.count.database}")
  private int redisProductCountDatabase;

  @Value("${redis.product.count.timeout.in.millis}")
  private int redisProductCountTimeoutInMillis;

  @Value("${redis.product.count.expiration.in.secs}")
  private long redisProductCountCacheExpirationInSecs;

  @Value("${redis.pool.product.count.minIdle}")
  private int redisPoolProductCountMinIdle;

  @Value("${redis.pool.product.count.maxIdle}")
  private int redisPoolProductCountMaxIdle;

  @Value("${redis.pool.product.count.maxTotal}")
  private int redisPoolProductCountMaxTotal;

  @Value("${redis.pool.product.count.maxWaitMillis}")
  private int redisPoolProductCountMaxWaitMillis;

  //Size chart cache properties

  @Value("${redis.size.chart.hostName}")
  private String redisSizeChartHostName;

  @Value("${redis.size.chart.port}")
  private int redisSizeChartPort;

  @Value("${redis.size.chart.database}")
  private int redisSizeChartDatabase;

  @Value("${redis.size.chart.timeout.in.millis}")
  private int redisSizeChartTimeoutInMillis;

  @Value("${redis.size.chart.expiration.in.secs}")
  private long redisSizeChartCacheExpirationInSecs;

  @Value("${redis.pool.size.chart.minIdle}")
  private int redisPoolSizeChartMinIdle;

  @Value("${redis.pool.size.chart.maxIdle}")
  private int redisPoolSizeChartMaxIdle;

  @Value("${redis.pool.size.chart.maxTotal}")
  private int redisPoolSizeChartMaxTotal;

  @Value("${redis.pool.size.chart.maxWaitMillis}")
  private int redisPoolSizeChartMaxWaitMillis;

  // Item pickup point cache properties

  @Value("${redis.item.pickup.point.hostName}")
  private String redisItemPickupPointHostName;

  @Value("${redis.item.pickup.point.port}")
  private int redisItemPickupPointPort;

  @Value("${redis.item.pickup.point.database}")
  private int redisItemPickupPointDatabase;

  @Value("${redis.item.pickup.point.timeout.in.millis}")
  private int redisItemPickupPointTimeoutInMillis;

  @Value("${redis.item.pickup.point.expiration.in.secs}")
  private long redisItemPickupPointCacheExpirationInSecs;

  @Value("${redis.pool.item.pickup.point.minIdle}")
  private int redisPoolItemPickupPointMinIdle;

  @Value("${redis.pool.item.pickup.point.maxIdle}")
  private int redisPoolItemPickupPointMaxIdle;

  @Value("${redis.pool.item.pickup.point.maxTotal}")
  private int redisPoolItemPickupPointMaxTotal;

  @Value("${redis.pool.item.pickup.point.maxWaitMillis}")
  private int redisPoolItemPickupPointMaxWaitMillis;

}