package com.gdn.partners.pcu.internal.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.internal.client.fallback.MTAFeignFallback;
import com.gdn.partners.pcu.internal.client.feign.MTAFeign;
import com.gdn.partners.pcu.internal.properties.ApplicationProperties;

@Component
public class MTAFeignFallbackFactory extends AbstractFallbackFactory<MTAFeign> {

  @Autowired
  private MTAFeignFallback fallback;

  @Autowired
  public MTAFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override public MTAFeign doCreate(Throwable cause) {
    return fallback;
  }
}