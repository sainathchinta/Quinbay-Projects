package com.gdn.partners.pcu.internal.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.TimeUnit;

import org.apache.commons.collections.CollectionUtils;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.core.StringRedisTemplate;
import org.springframework.stereotype.Service;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.service.CacheProductService;

import lombok.extern.slf4j.Slf4j;

/**
 * Created by govind on 17/01/2019 AD.
 */

@Slf4j
@Service
public class CacheProductServiceImpl implements CacheProductService{

  private static final long REDIS_TIMEOUT = 30L;

  @Autowired
  private RedisTemplate<String, List<String>> redisTemplate;

  @Override
  public String removeCurrentUserFromProductView(String productCodeRedisKey, String currentUserName) {
    List<String> vendors = new ArrayList<>();
    if (redisTemplate.hasKey(productCodeRedisKey)) {
      vendors.addAll(this.redisTemplate.boundValueOps(productCodeRedisKey).get());
      if (CollectionUtils.isEmpty(vendors)) {
        this.redisTemplate.delete(productCodeRedisKey);
      } else if (vendors.contains(currentUserName)) {
        vendors.remove(currentUserName);
        if (vendors.isEmpty()) {
          this.redisTemplate.delete(productCodeRedisKey);
          log.debug("Deleting user : {} for product : {}", currentUserName, productCodeRedisKey);
        } else {
          this.redisTemplate.boundValueOps(productCodeRedisKey).set(vendors);
        }
      }
    }
    return currentUserName;
  }

  @Override
  public List<String> getReviewerList(String productCodeRedisKey) {
    if (redisTemplate.hasKey(productCodeRedisKey)) {
      return this.redisTemplate.boundValueOps(productCodeRedisKey).get();
    }
    return Collections.emptyList();
  }

  @Override
  public void addUserToProductReviewList(String productCodeRedisKey, String userName) {
    List<String> vendors = new ArrayList<>();
    if (redisTemplate.hasKey(productCodeRedisKey)) {
      vendors.addAll(this.redisTemplate.boundValueOps(productCodeRedisKey).get());
    }
    if (!vendors.contains(userName)) {
      vendors.add(userName);
      this.redisTemplate.boundValueOps(productCodeRedisKey).set(vendors);
      this.redisTemplate.expire(productCodeRedisKey, REDIS_TIMEOUT, TimeUnit.MINUTES);
    }
  }
}
