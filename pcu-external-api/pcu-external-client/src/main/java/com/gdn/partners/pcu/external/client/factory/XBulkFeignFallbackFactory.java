package com.gdn.partners.pcu.external.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.external.client.fallback.XBulkFeignFallback;
import com.gdn.partners.pcu.external.client.feign.XBulkFeign;
import com.gdn.partners.pcu.external.properties.ApplicationProperties;

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
