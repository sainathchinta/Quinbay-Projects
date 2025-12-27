package com.gdn.partners.pcu.internal.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.internal.client.fallback.XBulkFeignFallback;
import com.gdn.partners.pcu.internal.client.feign.XBulkFeign;
import com.gdn.partners.pcu.internal.properties.ApplicationProperties;

@Component
public class XBulkFeignFallbackFactory extends AbstractFallbackFactory<XBulkFeign> {

  @Autowired
  private XBulkFeignFallback fallback;

  @Autowired
  public XBulkFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public XBulkFeign doCreate(Throwable cause) {
    return fallback;
  }

}
