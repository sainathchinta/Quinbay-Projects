package com.gdn.partners.pcu.master.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.master.client.fallback.XBPFeignFallback;
import com.gdn.partners.pcu.master.client.feign.XBPFeign;
import com.gdn.partners.pcu.master.properties.ApplicationProperties;

@Component
public class XBPFeignFallbackFactory extends AbstractFallbackFactory<XBPFeign> {

  @Autowired
  private XBPFeignFallback fallback;

  @Autowired
  public XBPFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public XBPFeign doCreate(Throwable cause) {
    return fallback;
  }

}
