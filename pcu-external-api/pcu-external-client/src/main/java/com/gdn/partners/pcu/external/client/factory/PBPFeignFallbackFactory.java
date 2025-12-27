package com.gdn.partners.pcu.external.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import com.gdn.partners.pcu.external.client.fallback.PBPFeignFallback;
import com.gdn.partners.pcu.external.client.feign.PBPFeign;
import com.gdn.partners.pcu.external.properties.ApplicationProperties;

/**
 * @author Pradeep Reddy
 */
@Component
public class PBPFeignFallbackFactory extends AbstractFallbackFactory<PBPFeign> {

  @Autowired
  private PBPFeignFallback fallback;

  @Autowired
  public PBPFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public PBPFeign doCreate(Throwable cause) {
    return fallback;
  }
}
