package com.gdn.partners.product.analytics.service.impl.helper;


import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.mockito.MockitoAnnotations.openMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.redis.connection.RedisConnection;
import org.springframework.data.redis.connection.RedisConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.product.analytics.model.enums.JobProcessTypes;

public class CacheServiceHelperImplTest {

  @InjectMocks
  private CacheServiceHelperImpl cacheServiceHelper;

  @Mock
  private RedisTemplate redisTemplate;

  @Mock
  private RedisConnection redisConnection;

  @Mock
  @Qualifier("jedisConnectionFactory")
  private RedisConnectionFactory jedisConnectionFactory;

  @Mock
  @Qualifier("sellerJedisConnectionFactory")
  private RedisConnectionFactory sellerJedisConnectionFactory;

  @Test
  public void flushAllTest() {
    when(jedisConnectionFactory.getConnection()).thenReturn(redisConnection);

    cacheServiceHelper.flushAll(JobProcessTypes.SELLER_INFO_BQ_JOB.name());

    verify(jedisConnectionFactory).getConnection();
    verify(redisConnection).flushDb();
  }

  @Test
  public void flushAllSellerTypeTest() {
    when(sellerJedisConnectionFactory.getConnection()).thenReturn(redisConnection);

    cacheServiceHelper.flushAll(JobProcessTypes.SELLER_SPECIFIC_INFO_BQ_JOB.name());

    verify(sellerJedisConnectionFactory).getConnection();
    verify(redisConnection).flushDb();
  }

  @Test
  public void flushAllExceptionTest() {
    when(jedisConnectionFactory.getConnection()).thenThrow(ApplicationRuntimeException.class);

    cacheServiceHelper.flushAll(JobProcessTypes.SELLER_INFO_BQ_JOB.name());

    verify(jedisConnectionFactory).getConnection();
  }

  @BeforeEach
  public void setUp() {
    openMocks(this);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(redisTemplate);
    verifyNoMoreInteractions(redisConnection);
    verifyNoMoreInteractions(jedisConnectionFactory);
    verifyNoMoreInteractions(sellerJedisConnectionFactory);
  }
}