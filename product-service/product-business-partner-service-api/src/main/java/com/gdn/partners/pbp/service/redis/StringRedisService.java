package com.gdn.partners.pbp.service.redis;

public interface StringRedisService {
  
  void set(String key, String value);
  
  String get(String key);
  
  void del(String key);

}
