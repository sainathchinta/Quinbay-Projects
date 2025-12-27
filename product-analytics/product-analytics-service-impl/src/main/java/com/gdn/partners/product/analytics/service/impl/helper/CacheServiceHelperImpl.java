package com.gdn.partners.product.analytics.service.impl.helper;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.redis.connection.RedisConnection;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.stereotype.Component;

import com.gdn.partners.product.analytics.model.enums.JobProcessTypes;
import com.gdn.partners.product.analytics.service.helper.CacheServiceHelper;
import lombok.extern.slf4j.Slf4j;

@Component
@Slf4j
public class CacheServiceHelperImpl implements CacheServiceHelper {

  @Autowired
  @Qualifier("jedisConnectionFactory")
  private RedisConnectionFactory jedisConnectionFactory;

  @Autowired
  @Qualifier("sellerJedisConnectionFactory")
  private RedisConnectionFactory sellerJedisConnectionFactory;

  @Override
  public void flushAll(String jobProcessType) {
    RedisConnection redisConnection;
    try {
      if(jobProcessType.equals(JobProcessTypes.SELLER_INFO_BQ_JOB.name())) {
        redisConnection = jedisConnectionFactory.getConnection();
      } else {
        redisConnection = sellerJedisConnectionFactory.getConnection();
      }
      redisConnection.flushDb();
    } catch (Exception e) {
      log.error("#flushAll failed error = ", e);
    }
  }
}
