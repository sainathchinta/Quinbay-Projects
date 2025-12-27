package com.gdn.partners.pcu.internal.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.internal.client.fallback.XGPFeignFallback;
import com.gdn.partners.pcu.internal.client.feign.XGPFeign;
import com.gdn.partners.pcu.internal.properties.ApplicationProperties;

@Component
public class XGPFeignFallbackFactory extends AbstractFallbackFactory<XGPFeign> {

  @Autowired
  private XGPFeignFallback fallback;

  @Autowired
  public XGPFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public XGPFeign doCreate(Throwable cause) {
    return fallback;
  }
}
