package com.gdn.partners.pcu.external.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.external.client.fallback.PCBFeignFallback;
import com.gdn.partners.pcu.external.client.feign.PCBFeign;
import com.gdn.partners.pcu.external.properties.ApplicationProperties;

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
