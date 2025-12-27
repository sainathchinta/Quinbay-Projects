package com.gdn.partners.pcu.external.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import com.gdn.partners.pcu.external.client.fallback.XBPFeignFallback;
import com.gdn.partners.pcu.external.client.feign.XBPFeign;
import com.gdn.partners.pcu.external.properties.ApplicationProperties;

/**
 * Created by govind on 13/12/2018 AD.
 */
@Component
public class XBPFeignFallbackFactory extends AbstractFallbackFactory<XBPFeign>{

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
