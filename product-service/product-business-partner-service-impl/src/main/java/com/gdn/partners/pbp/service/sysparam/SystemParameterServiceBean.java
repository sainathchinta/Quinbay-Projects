package com.gdn.partners.pbp.service.sysparam;

import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.stream.Collectors;

import jakarta.annotation.Resource;

import org.apache.commons.collections.CollectionUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import com.gdn.partners.pbp.model.vo.CacheKeys;

@Service("systemParameterService")
public class SystemParameterServiceBean implements SystemParameterService {

  @Autowired
  @Qualifier("stringRedisTemplate")
  private StringRedisTemplate redisTemplate;

  private Properties sysparamProperties;

  private static final Map<String, String> SYSPARAM_MAP = new HashMap<>();
  private static final Object EXCLUSIVE_LOCK = new Object();
  private static final ReadWriteLock RW_LOCK = new ReentrantReadWriteLock();
  private static final Logger LOG = LoggerFactory.getLogger(SystemParameterServiceBean.class);
  // Redis using glob-style regex pattern, please see here: https://redis.io/commands/keys
  private static final String SYSPARAM_GROUP_KEY_PATTERN = CacheKeys.SYSPARAM_GROUP_KEY + "*";

  public void setRedisTemplate(StringRedisTemplate redisTemplate) {
    this.redisTemplate = redisTemplate;
  }

  public void setSysparamProperties(Properties sysparamProperties) {
    this.sysparamProperties = sysparamProperties;
  }

  public StringRedisTemplate getRedisTemplate() {
    return redisTemplate;
  }

  private void setSystemParameterValue(String cacheKey, Object value, boolean updateRedis) {
    try {
      RW_LOCK.writeLock().lock();
      if (updateRedis) {
        redisTemplate.boundValueOps(cacheKey).set(String.valueOf(value));
      }
      SYSPARAM_MAP.put(cacheKey, String.valueOf(value));
    } catch (Exception e) {
      LOG.error("Error when setSystemParameterValue, cacheKey: {}, value: {}, updateRedis: {}",
          cacheKey, value, updateRedis, e);
    } finally {
      RW_LOCK.writeLock().unlock();
    }
  }

  protected String buildCacheKey(String key) {
    return CacheKeys.SYSPARAM_GROUP_KEY + key;
  }

  @SuppressWarnings("unchecked")
  @Override
  public void reload(boolean fullReload) {
    synchronized (EXCLUSIVE_LOCK) {
      if (fullReload) {
        SYSPARAM_MAP.clear();
        Enumeration<String> keys = (Enumeration<String>) sysparamProperties.propertyNames();
        while (keys.hasMoreElements()) {
          String propKey = keys.nextElement();
          String cacheKey = buildCacheKey(propKey);
          String value = String.valueOf(sysparamProperties.get(propKey));
          setSystemParameterValue(cacheKey, value, true);
        }
      } else {
        Set<String> redisCacheKeys = redisTemplate.keys(SYSPARAM_GROUP_KEY_PATTERN);
        for (String cacheKey : redisCacheKeys) {
          setSystemParameterValue(cacheKey, redisTemplate.boundValueOps(cacheKey).get(), false);
        }

        Enumeration<String> propertyFileKeys =
            (Enumeration<String>) sysparamProperties.propertyNames();
        while (propertyFileKeys.hasMoreElements()) {
          String propKey = propertyFileKeys.nextElement();
          String cacheKey = buildCacheKey(propKey);
          String value = String.valueOf(sysparamProperties.get(propKey));
          if (!redisCacheKeys.contains(cacheKey)) {
            setSystemParameterValue(cacheKey, value, true);
          }
        }
      }
    }
  }

  @Override
  public void setParameter(String key, String value) {
    String cacheKey = buildCacheKey(key);
    setSystemParameterValue(cacheKey, value, true);
  }

  @Override
  public String getParameter(String key) {
    try {
      RW_LOCK.readLock().lock();
      return SYSPARAM_MAP.get(CacheKeys.SYSPARAM_GROUP_KEY + key);
    } finally {
      RW_LOCK.readLock().unlock();
    }
  }

  @Override
  public boolean isInitialized() {
    return CollectionUtils.isNotEmpty(redisTemplate.keys(SYSPARAM_GROUP_KEY_PATTERN));
  }

  @Override
  public void reset() {
    try {
      RW_LOCK.writeLock().lock();
      SYSPARAM_MAP.clear();
    } catch (Exception e) {
      LOG.error("reset system parameter failed", e);
    } finally {
      RW_LOCK.writeLock().unlock();
    }
  }

  @Override
  public List<String> keys() {
    return SYSPARAM_MAP.keySet().stream().map(key -> key.replace(CacheKeys.SYSPARAM_GROUP_KEY, ""))
        .collect(Collectors.toList());
  }
}
