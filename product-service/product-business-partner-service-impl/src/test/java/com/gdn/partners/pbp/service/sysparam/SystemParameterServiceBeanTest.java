package com.gdn.partners.pbp.service.sysparam;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.redis.core.BoundValueOperations;
import org.springframework.data.redis.core.StringRedisTemplate;

import com.gdn.partners.pbp.model.vo.CacheKeys;

public class SystemParameterServiceBeanTest {

  @Mock
  private StringRedisTemplate redisTemplate;

  @Mock
  private BoundValueOperations<String, String> boundValueOps;

  private Properties sysparamProperties;

  private SystemParameterServiceBean sysparamService;

  private static final String KEY1 = "productlevel3.test1";
  private static final String VALUE1 = "test123";
  private static final String KEY2 = "productlevel3.test2";
  private static final String VALUE2 = "test345";
  Map<String, String> propertyMaps = new HashMap<>();
  Set<String> cacheKeys = new HashSet<>();

  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);

    sysparamProperties = new Properties();
    propertyMaps.put(KEY1, VALUE1);
    propertyMaps.put(KEY2, VALUE2);
    sysparamProperties.putAll(propertyMaps);

    sysparamService = new SystemParameterServiceBean();
    sysparamService.setRedisTemplate(redisTemplate);
    sysparamService.setSysparamProperties(sysparamProperties);

    // Set<String> cacheKeys = new HashSet<>();
    // cacheKeys.add(KEY1);
    cacheKeys.add(CacheKeys.SYSPARAM_GROUP_KEY + KEY1);

    Mockito.when(redisTemplate.boundValueOps(Mockito.anyString())).thenReturn(boundValueOps);
    Mockito.when(redisTemplate.keys(Mockito.anyString())).thenReturn(cacheKeys);
    Mockito.when(boundValueOps.get()).thenReturn(VALUE1);
  }

  @AfterEach
  public void finalizeTest() {
    sysparamService.reset();
  }

  @Test
  public void testReload_NoFullReload() throws Exception {
    sysparamService.reload(false);
  }

  @Test
  public void testReload_FullReload() {
    sysparamService.reload(true);
  }

  @Test
  public void testSetParameter() {
    sysparamService.setParameter(KEY1, VALUE1);
  }

  @Test
  public void testSetParameter_Exception() {
    Mockito.doThrow(new RuntimeException("test redis error")).when(boundValueOps)
        .set(Mockito.anyString());
    sysparamService.setParameter(KEY1, VALUE1);
  }

  @Test
  public void testGetParameter() {
    sysparamService.getParameter(KEY1);
  }

  @Test
  public void testIsInitialized_False() {
    Mockito.when(redisTemplate.keys(Mockito.anyString())).thenReturn(null);
    Assertions.assertFalse(sysparamService.isInitialized());
  }

  @Test
  public void testIsInitialized_True() {
    Set<String> cacheKeys = new HashSet<>();
    cacheKeys.add(KEY1);
    Mockito.when(redisTemplate.keys(Mockito.anyString())).thenReturn(cacheKeys);
    Assertions.assertTrue(sysparamService.isInitialized());
  }

  @Test
  public void test_Keys() {
    sysparamService.setParameter(KEY1, VALUE1);
    sysparamService.setParameter(KEY2, VALUE2);
    List<String> keys = sysparamService.keys();
    Assertions.assertTrue(keys.contains(KEY1));
    Assertions.assertTrue(keys.contains(KEY2));
  }
}
