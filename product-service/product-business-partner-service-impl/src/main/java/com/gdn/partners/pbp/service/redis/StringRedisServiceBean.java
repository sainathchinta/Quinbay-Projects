package com.gdn.partners.pbp.service.redis;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;

import com.gdn.partners.pbp.model.vo.CacheKeys;

@Service
public class StringRedisServiceBean implements StringRedisService {

  @Autowired
  @Qualifier("stringRedisTemplate")
  private StringRedisTemplate stringRedisTemplate;

  @Override
  public void set(String key, String value) {
    String redisKey = this.buildRedisKey(key);
    stringRedisTemplate.boundValueOps(redisKey).set(value);
  }

  @Override
  public String get(String key) {
    String redisKey = this.buildRedisKey(key);
    return stringRedisTemplate.boundValueOps(redisKey).get();
  }

  @Override
  public void del(String key) {
    String redisKey = this.buildRedisKey(key);
    stringRedisTemplate.delete(redisKey);
  }
  
  private String buildRedisKey(String key) {
    return CacheKeys.ROOT_KEY + key;
  }

}
