package com.gdn.partners.pcu.external.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import com.gdn.partners.pcu.external.client.fallback.BPServiceFeignFallback;
import com.gdn.partners.pcu.external.client.feign.BPServiceFeign;
import com.gdn.partners.pcu.external.properties.ApplicationProperties;

/**
 * Created by govind on 13/12/2018 AD.
 */
@Component
public class BPServiceFeignFallbackFactory extends AbstractFallbackFactory<BPServiceFeign>{

  @Autowired
  private BPServiceFeignFallback fallback;

  @Autowired
  public BPServiceFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public BPServiceFeign doCreate(Throwable cause) {
    return fallback;
  }
}
