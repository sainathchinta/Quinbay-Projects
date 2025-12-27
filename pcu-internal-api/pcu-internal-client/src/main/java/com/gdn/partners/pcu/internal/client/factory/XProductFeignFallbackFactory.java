package com.gdn.partners.pcu.internal.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.internal.client.factory.AbstractFallbackFactory;
import com.gdn.partners.pcu.internal.client.fallback.XProductFeignFallback;
import com.gdn.partners.pcu.internal.client.feign.XProductFeign;
import com.gdn.partners.pcu.internal.properties.ApplicationProperties;

@Component
public class XProductFeignFallbackFactory extends AbstractFallbackFactory<XProductFeign> {

  @Autowired
  private XProductFeignFallback fallback;

  @Autowired
  public XProductFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public XProductFeign doCreate(Throwable cause) {
    return fallback;
  }

}
