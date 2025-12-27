package com.gdn.partners.pcu.external.client.factory;

import com.gdn.partners.pcu.external.client.fallback.XgpFeignFallback;
import com.gdn.partners.pcu.external.client.feign.XgpFeign;
import com.gdn.partners.pcu.external.properties.ApplicationProperties;
import org.springframework.stereotype.Component;

@Component
public class XgpFeignFallbackFactory extends AbstractFallbackFactory<XgpFeign> {

  public XgpFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public XgpFeign doCreate(Throwable cause) {
    return new XgpFeignFallback();
  }

}
