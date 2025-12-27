package com.gdn.partners.pcu.internal.service.impl;

import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.junit.jupiter.MockitoSettings;
import org.mockito.quality.Strictness;
import org.springframework.data.redis.core.BoundValueOperations;
import org.springframework.data.redis.core.RedisTemplate;

import com.gdn.partners.pcu.internal.model.Constants;

/**
 * Created by govind on 17/01/2019 AD.
 */
@ExtendWith(MockitoExtension.class)
@MockitoSettings(strictness = Strictness.LENIENT)
public class CacheProductServiceImplTest {

  @InjectMocks
  private CacheProductServiceImpl cacheProductService;

  @Mock
  private BoundValueOperations<String, List<String>> operations;

  @Mock
  private RedisTemplate<String, List<String>> redisTemplate;

  private static final String PRODUCT_CODE = "product-code";
  private static final String CURRENT_USER_NAME = "a@a.com";
  private static final String USER_NAME = "userName";
  private static final long REDIS_TIMEOUT = 30L;

  @BeforeEach
  public void setUp() throws Exception {

  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(operations);
    verifyNoMoreInteractions(redisTemplate);
  }

  @Test
  public void removeCurrentUserFromProductViewTest() {
    Mockito.when(redisTemplate.boundValueOps(Mockito.anyString())).thenReturn(operations);
    Mockito.when(operations.get()).thenReturn(Collections.emptyList());
    Mockito.when(redisTemplate.hasKey(PRODUCT_CODE)).thenReturn(true);
    String response = cacheProductService.removeCurrentUserFromProductView(PRODUCT_CODE, CURRENT_USER_NAME);
    Mockito.verify(redisTemplate).boundValueOps(Mockito.eq(PRODUCT_CODE));
    Mockito.verify(redisTemplate).delete(PRODUCT_CODE);
    Mockito.verify(operations).get();
    Mockito.verify(redisTemplate).hasKey(PRODUCT_CODE);
    Assertions.assertEquals(CURRENT_USER_NAME, response);
  }

  @Test
  public void removeCurrentUserRedisDoesNotContainsUserTest() {
    Mockito.when(redisTemplate.boundValueOps(Mockito.anyString())).thenReturn(operations);
    Mockito.when(operations.get()).thenReturn(Arrays.asList(Constants.USER_NAME));
    Mockito.when(redisTemplate.hasKey(PRODUCT_CODE)).thenReturn(true);
    String response = cacheProductService.removeCurrentUserFromProductView(PRODUCT_CODE, CURRENT_USER_NAME);
    Mockito.verify(redisTemplate).boundValueOps(Mockito.eq(PRODUCT_CODE));
    Mockito.verify(redisTemplate).hasKey(PRODUCT_CODE);
    Mockito.verify(operations).get();
    Assertions.assertEquals(CURRENT_USER_NAME, response);
  }

  @Test
  public void removeCurrentUserRedisContainsUserTest() {
    Mockito.when(redisTemplate.boundValueOps(Mockito.anyString())).thenReturn(operations);
    Mockito.when(operations.get()).thenReturn(Arrays.asList(CURRENT_USER_NAME));
    Mockito.when(redisTemplate.hasKey(PRODUCT_CODE)).thenReturn(true);
    String response = cacheProductService.removeCurrentUserFromProductView(PRODUCT_CODE, CURRENT_USER_NAME);
    Mockito.verify(redisTemplate).boundValueOps(Mockito.eq(PRODUCT_CODE));
    Mockito.verify(redisTemplate).delete(PRODUCT_CODE);
    Mockito.verify(operations).get();
    Mockito.verify(redisTemplate).hasKey(PRODUCT_CODE);
    Assertions.assertEquals(CURRENT_USER_NAME, response);
  }

  @Test
  public void removeCurrentUserRedisContainsMultipleUserTest() {
    Mockito.when(redisTemplate.boundValueOps(Mockito.anyString())).thenReturn(operations);
    Mockito.when(operations.get()).thenReturn(Arrays.asList(CURRENT_USER_NAME, USER_NAME));
    Mockito.when(redisTemplate.hasKey(PRODUCT_CODE)).thenReturn(true);
    String response = cacheProductService.removeCurrentUserFromProductView(PRODUCT_CODE, CURRENT_USER_NAME);
    Mockito.verify(redisTemplate, times(2)).boundValueOps(Mockito.eq(PRODUCT_CODE));
    Mockito.verify(operations).get();
    Mockito.verify(operations).set(Arrays.asList(USER_NAME));
    Mockito.verify(redisTemplate).hasKey(PRODUCT_CODE);
    Assertions.assertEquals(CURRENT_USER_NAME, response);
  }

  @Test
  public void getReviewerListTest() {
    Mockito.when(redisTemplate.boundValueOps(Mockito.anyString())).thenReturn(operations);
    Mockito.when(operations.get()).thenReturn(Arrays.asList(PRODUCT_CODE));
    Mockito.when(redisTemplate.hasKey(PRODUCT_CODE)).thenReturn(true);
    List<String> reviewerList = cacheProductService.getReviewerList(PRODUCT_CODE);
    Mockito.verify(redisTemplate).hasKey(PRODUCT_CODE);
    Mockito.verify(redisTemplate).boundValueOps(Mockito.eq(PRODUCT_CODE));
    Mockito.verify(operations).get();
    Assertions.assertEquals(PRODUCT_CODE, reviewerList.get(0));
  }

  @Test
  public void getReviewerListEmptyVendorTest() {
    Mockito.when(redisTemplate.hasKey(PRODUCT_CODE)).thenReturn(false);
    List<String> reviewerList = cacheProductService.getReviewerList(PRODUCT_CODE);
    Mockito.verify(redisTemplate).hasKey(PRODUCT_CODE);
    Assertions.assertEquals(Collections.emptyList(), reviewerList);
  }

  @Test
  public void addReviewerToListTest() {
    Mockito.when(redisTemplate.boundValueOps(Mockito.anyString())).thenReturn(operations);
    Mockito.when(operations.get()).thenReturn(Collections.emptyList());
    Mockito.when(redisTemplate.hasKey(PRODUCT_CODE)).thenReturn(true);
    cacheProductService.addUserToProductReviewList(PRODUCT_CODE, Constants.USER_NAME);
    Mockito.verify(redisTemplate).hasKey(PRODUCT_CODE);
    Mockito.verify(redisTemplate, times(2)).boundValueOps(Mockito.eq(PRODUCT_CODE));
    Mockito.verify(operations).get();
    Mockito.verify(operations).set(Collections.singletonList(Constants.USER_NAME));
    Mockito.verify(redisTemplate).expire(PRODUCT_CODE, REDIS_TIMEOUT, TimeUnit.MINUTES);
  }

  @Test
  public void addReviewerToListPresentInRedisTest() {
    Mockito.when(redisTemplate.boundValueOps(Mockito.anyString())).thenReturn(operations);
    Mockito.when(operations.get()).thenReturn(Arrays.asList(CURRENT_USER_NAME));
    Mockito.when(redisTemplate.hasKey(PRODUCT_CODE)).thenReturn(true);
    cacheProductService.addUserToProductReviewList(PRODUCT_CODE, CURRENT_USER_NAME);
    Mockito.verify(redisTemplate).hasKey(PRODUCT_CODE);
    Mockito.verify(redisTemplate).boundValueOps(Mockito.eq(PRODUCT_CODE));
    Mockito.verify(operations).get();
  }
}
