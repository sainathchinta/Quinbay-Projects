package com.gdn.partners.pcu.master.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.master.client.fallback.XProductFeignFallBack;
import com.gdn.partners.pcu.master.client.feign.XProductFeign;
import com.gdn.partners.pcu.master.properties.ApplicationProperties;

@Component
public class XProductFeignFallBackFactory extends AbstractFallbackFactory<XProductFeign> {

  @Autowired
  private XProductFeignFallBack fallback;

  @Autowired
  public XProductFeignFallBackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public XProductFeign doCreate(Throwable cause) {
    return fallback;
  }
}
