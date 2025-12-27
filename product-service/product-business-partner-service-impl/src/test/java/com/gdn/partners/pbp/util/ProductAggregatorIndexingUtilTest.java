package com.gdn.partners.pbp.util;



import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.partners.pbp.model.vo.CacheKeys;
import com.gdn.partners.pbp.service.redis.StringRedisService;

public class ProductAggregatorIndexingUtilTest {

  @InjectMocks
  private ProductAggregatorIndexingUtil instance;

  @Mock
  private StringRedisService stringRedisService;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.stringRedisService);
  }

  @Test
  public void testIsCurrentIndexingRunning() {
    Mockito.when(this.stringRedisService.get(CacheKeys.INDEXING_STATE))
        .thenReturn(Boolean.TRUE.toString());
    Boolean response = this.instance.isCurrentIndexingRunning();
    Assertions.assertTrue(response);
    Mockito.verify(this.stringRedisService).get(CacheKeys.INDEXING_STATE);
  }
  
  @Test
  public void testIsCurrentIndexingRunningEmptyKey() {
    Mockito.when(this.stringRedisService.get(CacheKeys.INDEXING_STATE))
        .thenReturn(null);
    Boolean response = this.instance.isCurrentIndexingRunning();
    Assertions.assertFalse(response);
    Mockito.verify(this.stringRedisService).get(CacheKeys.INDEXING_STATE);
  }

  @Test
  public void testSetIndexingRunning() {
    Mockito.doNothing().when(this.stringRedisService).set(CacheKeys.INDEXING_STATE,
        Boolean.TRUE.toString());
    this.instance.setIndexingRunning();
    Mockito.verify(this.stringRedisService).set(CacheKeys.INDEXING_STATE, Boolean.TRUE.toString());
  }

  @Test
  public void testDelIndexingRunningProcess() {
    Mockito.doNothing().when(this.stringRedisService).del(CacheKeys.INDEXING_STATE);
    this.instance.delIndexingRunningProcess();
    Mockito.verify(this.stringRedisService).del(CacheKeys.INDEXING_STATE);
  }

}
