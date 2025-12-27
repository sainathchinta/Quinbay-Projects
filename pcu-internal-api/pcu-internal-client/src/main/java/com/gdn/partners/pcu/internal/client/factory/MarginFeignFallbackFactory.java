package com.gdn.partners.pcu.internal.client.factory;

import com.gdn.partners.pcu.internal.client.fallback.MarginFeignFallback;
import com.gdn.partners.pcu.internal.client.feign.MarginFeign;
import com.gdn.partners.pcu.internal.properties.ApplicationProperties;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class MarginFeignFallbackFactory extends AbstractFallbackFactory<MarginFeign> {

  @Autowired
  private MarginFeignFallback fallback;

  @Autowired
  public MarginFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public MarginFeign doCreate(Throwable cause) {
    return fallback;
  }
}
