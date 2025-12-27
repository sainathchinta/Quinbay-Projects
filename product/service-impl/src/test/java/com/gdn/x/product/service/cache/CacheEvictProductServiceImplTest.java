package com.gdn.x.product.service.cache;


import static org.mockito.MockitoAnnotations.openMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.test.util.ReflectionTestUtils;

public class CacheEvictProductServiceImplTest {

  @InjectMocks
  private CacheEvictProductServiceImpl cacheEvictProductService;

  @Mock
  private RedisTemplate<String, Object> productRedisTemplate;

  private static final String PRODUCT_CODE = "product-code";
  private static final String STORE_ID = "store-id";

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    ReflectionTestUtils.setField(cacheEvictProductService, "redisProductEnabled", true);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(productRedisTemplate);
  }

  @Test
  public void evictFindProductByProductCode() {
    Mockito.when(productRedisTemplate.delete(Mockito.anyString())).thenReturn(Boolean.TRUE);
    cacheEvictProductService.evictFindProductByProductCode(STORE_ID, PRODUCT_CODE);
    Mockito.verify(productRedisTemplate).delete(Mockito.anyString());
  }

  @Test
  public void evictFindProductByProductCodeWithRedisDisabled() {
    ReflectionTestUtils.setField(cacheEvictProductService, "redisProductEnabled", false);
    cacheEvictProductService.evictFindProductByProductCode(STORE_ID, PRODUCT_CODE);
  }

  @Test
  public void evictFindProductByProductCodeUsingCacheEvictTest(){
    cacheEvictProductService.evictFindProductByProductCodeUsingCacheEvict(STORE_ID,PRODUCT_CODE);
  }

}