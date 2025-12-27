package com.gdn.partners.pcu.internal.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.internal.client.fallback.PCBFeignFallback;
import com.gdn.partners.pcu.internal.client.feign.PCBFeign;
import com.gdn.partners.pcu.internal.properties.ApplicationProperties;

/**
 * @author Pradeep Reddy
 */
@Component
public class PCBFeignFallbackFactory extends AbstractFallbackFactory<PCBFeign> {

  @Autowired
  private PCBFeignFallback fallback;

  @Autowired
  public PCBFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public PCBFeign doCreate(Throwable cause) {
    return fallback;
  }
}
