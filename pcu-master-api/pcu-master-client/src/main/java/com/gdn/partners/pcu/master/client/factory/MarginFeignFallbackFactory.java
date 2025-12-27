package com.gdn.partners.pcu.master.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.master.client.fallback.MarginFeignFallback;
import com.gdn.partners.pcu.master.client.feign.MarginFeign;
import com.gdn.partners.pcu.master.properties.ApplicationProperties;

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
