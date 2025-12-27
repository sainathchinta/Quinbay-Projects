package com.gdn.partners.pcu.internal.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.data.redis.serializer.GenericJackson2JsonRedisSerializer;
import org.springframework.data.redis.serializer.StringRedisSerializer;

import com.google.cloud.storage.Storage;

@Configuration
public class BeanConfiguration {

  @Autowired
  private StringRedisTemplate stringRedisTemplate;

  @Bean
  public <K, V> RedisTemplate<K, V> redisTemplate() {
    RedisTemplate<K, V> template =  new RedisTemplate<>();
    template.setConnectionFactory(stringRedisTemplate.getConnectionFactory());
    template.setKeySerializer(new StringRedisSerializer());
    template.setValueSerializer(new GenericJackson2JsonRedisSerializer());
    return template;
  }

}
