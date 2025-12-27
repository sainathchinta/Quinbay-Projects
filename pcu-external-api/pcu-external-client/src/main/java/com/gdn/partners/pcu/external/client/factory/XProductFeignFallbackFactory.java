package com.gdn.partners.pcu.external.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.external.client.fallback.XProductFeignFallback;
import com.gdn.partners.pcu.external.client.feign.XProductFeign;
import com.gdn.partners.pcu.external.properties.ApplicationProperties;

/**
 * @author Parvej
 */
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
