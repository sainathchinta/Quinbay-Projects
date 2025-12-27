package com.gdn.partners.pcu.external.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.external.client.fallback.AGPQueryQueryFeignFallback;
import com.gdn.partners.pcu.external.client.feign.AGPQueryFeign;
import com.gdn.partners.pcu.external.properties.ApplicationProperties;

@Component
public class AGPQueryFeignFallbackFactory extends AbstractFallbackFactory<AGPQueryFeign> {

  @Autowired
  private AGPQueryQueryFeignFallback fallback;

  @Autowired
  public AGPQueryFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public AGPQueryFeign doCreate(Throwable cause) {
    return fallback;
  }
}
