package com.gdn.partners.pbp.service.redis;



import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.redis.core.BoundValueOperations;
import org.springframework.data.redis.core.StringRedisTemplate;

public class StringRedisServiceBeanTest {

  private static final String CACHE_KEY = "1";
  private static final String CACHE_VALUE = "1";

  @InjectMocks
  private StringRedisServiceBean instance;

  @Mock
  private StringRedisTemplate stringRedisTemplate;

  @Mock
  private BoundValueOperations<String, String> boundValueOps;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(this.stringRedisTemplate);
  }

  @Test
  public void testSet() {
    Mockito.when(this.stringRedisTemplate.boundValueOps(Mockito.anyString()))
        .thenReturn(this.boundValueOps);
    this.instance.set(CACHE_KEY, CACHE_VALUE);
    Mockito.verify(this.stringRedisTemplate).boundValueOps(Mockito.anyString());
    Mockito.verify(this.boundValueOps).set(Mockito.anyString());
  }

  @Test
  public void testGet() {
    Mockito.when(this.stringRedisTemplate.boundValueOps(Mockito.anyString()))
        .thenReturn(this.boundValueOps);
    this.instance.get(CACHE_KEY);
    Mockito.verify(this.stringRedisTemplate).boundValueOps(Mockito.anyString());
    Mockito.verify(this.boundValueOps).get();
  }

  @Test
  public void testDel() {
    this.instance.del(CACHE_KEY);
    Mockito.verify(this.stringRedisTemplate).delete(Mockito.anyString());
  }

}
