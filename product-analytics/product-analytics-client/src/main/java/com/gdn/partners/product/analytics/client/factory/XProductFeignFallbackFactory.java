package com.gdn.partners.product.analytics.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.product.analytics.client.fallback.XProductFeignFallback;
import com.gdn.partners.product.analytics.client.XProduct.feign.XProductFeign;
import com.gdn.partners.product.analytics.properties.ApplicationProperties;

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
