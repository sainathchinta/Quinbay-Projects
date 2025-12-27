package com.gdn.partners.pcu.internal.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.internal.client.fallback.ExtCatalogFeignFallback;
import com.gdn.partners.pcu.internal.client.feign.ExtCatalogFeign;
import com.gdn.partners.pcu.internal.properties.ApplicationProperties;

@Component
public class ExtCatalogFallbackFactory extends AbstractFallbackFactory<ExtCatalogFeign> {

  @Autowired
  private ExtCatalogFeignFallback fallback;

  @Autowired
  public ExtCatalogFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override public ExtCatalogFeign doCreate(Throwable cause) {
    return fallback;
  }
}
