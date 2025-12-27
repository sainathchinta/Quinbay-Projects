package com.gdn.partners.pcu.external.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.external.client.fallback.XInventoryFeignFallback;
import com.gdn.partners.pcu.external.client.feign.XInventoryFeign;
import com.gdn.partners.pcu.external.properties.ApplicationProperties;

@Component
public class XInventoryFeignFallbackFactory extends AbstractFallbackFactory<XInventoryFeign> {

  @Autowired
  private XInventoryFeignFallback fallback;

  @Autowired
  public XInventoryFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public XInventoryFeign doCreate(Throwable cause) {
    return fallback;
  }
}
