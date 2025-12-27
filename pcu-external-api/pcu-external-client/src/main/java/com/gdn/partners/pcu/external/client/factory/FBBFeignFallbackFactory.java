package com.gdn.partners.pcu.external.client.factory;

import com.gdn.partners.pcu.external.client.fallback.FbbFeignFallback;
import com.gdn.partners.pcu.external.client.feign.FbbFeign;
import com.gdn.partners.pcu.external.properties.ApplicationProperties;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class FBBFeignFallbackFactory extends AbstractFallbackFactory<FbbFeign>{
  @Autowired
  private FbbFeignFallback fallback;

  @Autowired
  public FBBFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public FbbFeign doCreate(Throwable cause) {
    return fallback;
  }
}