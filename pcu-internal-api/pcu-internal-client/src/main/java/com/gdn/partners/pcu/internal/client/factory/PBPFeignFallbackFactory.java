package com.gdn.partners.pcu.internal.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import com.gdn.partners.pcu.internal.client.fallback.PBPFeignFallback;
import com.gdn.partners.pcu.internal.client.feign.PBPFeign;
import com.gdn.partners.pcu.internal.properties.ApplicationProperties;

/**
 * Created by govind on 14/01/2019 AD.
 */

@Component
public class PBPFeignFallbackFactory extends AbstractFallbackFactory<PBPFeign>{

  @Autowired
  private PBPFeignFallback fallback;

  @Autowired
  public PBPFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override public PBPFeign doCreate(Throwable cause) {
    return fallback;
  }
}
