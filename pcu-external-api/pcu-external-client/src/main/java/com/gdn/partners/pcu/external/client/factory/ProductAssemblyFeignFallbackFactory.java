package com.gdn.partners.pcu.external.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.external.client.fallback.ProductAssemblyFeignFallback;
import com.gdn.partners.pcu.external.client.feign.ProductAssemblyFeign;
import com.gdn.partners.pcu.external.properties.ApplicationProperties;

@Component
public class ProductAssemblyFeignFallbackFactory extends AbstractFallbackFactory<ProductAssemblyFeign> {

  @Autowired
  private ProductAssemblyFeignFallback fallback;

  @Autowired
  public ProductAssemblyFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public ProductAssemblyFeign doCreate(Throwable cause) {
    return fallback;
  }
}
