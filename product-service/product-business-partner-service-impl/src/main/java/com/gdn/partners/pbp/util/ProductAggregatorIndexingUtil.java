package com.gdn.partners.pbp.util;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pbp.model.vo.CacheKeys;
import com.gdn.partners.pbp.service.redis.StringRedisService;

@Component
public class ProductAggregatorIndexingUtil {
  
  @Autowired
  private StringRedisService stringRedisService;
  
  public boolean isCurrentIndexingRunning() {
    String state = this.stringRedisService.get(CacheKeys.INDEXING_STATE);
    if (StringUtils.isNotEmpty(state)) {
      return Boolean.valueOf(state);
    }
    return false;
  }

  public void setIndexingRunning() {
    this.stringRedisService.set(CacheKeys.INDEXING_STATE, Boolean.TRUE.toString());
  }

  public void delIndexingRunningProcess() {
    this.stringRedisService.del(CacheKeys.INDEXING_STATE);
  }
  
}
