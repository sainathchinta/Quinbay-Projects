package com.gdn.partners.pbp.service.sysparam;

import org.springframework.stereotype.Service;

@Service("globalSystemParameterService")
public class GlobalSystemParameterServiceBean extends SystemParameterServiceBean {
  @Override
  public String getParameter(String key) {
    String cacheKey = buildCacheKey(key);
    return getRedisTemplate().boundValueOps(cacheKey).get();
  }
}
