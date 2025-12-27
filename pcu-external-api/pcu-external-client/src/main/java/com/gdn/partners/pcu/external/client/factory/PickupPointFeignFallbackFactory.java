package com.gdn.partners.pcu.external.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import com.gdn.partners.pcu.external.client.fallback.PickupPointFeignFallback;
import com.gdn.partners.pcu.external.client.feign.PickupPointFeign;
import com.gdn.partners.pcu.external.properties.ApplicationProperties;

/**
 * Created by govind on 13/12/2018 AD.
 */
@Component
public class PickupPointFeignFallbackFactory extends AbstractFallbackFactory<PickupPointFeign>{

  @Autowired
  private PickupPointFeignFallback fallback;

  @Autowired
  public PickupPointFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public PickupPointFeign doCreate(Throwable cause) {
    return fallback;
  }
}
