package com.gdn.partners.pcu.internal.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.internal.client.fallback.BPJPHFeignFallback;
import com.gdn.partners.pcu.internal.client.feign.BPJPHFeign;
import com.gdn.partners.pcu.internal.properties.ApplicationProperties;

@Component
public class BPJPHFeignFallbackFactory extends AbstractFallbackFactory<BPJPHFeign>{

  @Autowired
  private BPJPHFeignFallback fallback;

  @Autowired
  public BPJPHFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public BPJPHFeign doCreate(Throwable cause) {
    return fallback;
  }
}
