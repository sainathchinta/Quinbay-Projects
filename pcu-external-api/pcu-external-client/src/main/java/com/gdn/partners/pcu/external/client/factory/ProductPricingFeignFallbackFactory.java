package com.gdn.partners.pcu.external.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.external.client.fallback.ProductPricingFeignFallback;
import com.gdn.partners.pcu.external.client.feign.ProductPricingFeign;
import com.gdn.partners.pcu.external.properties.ApplicationProperties;

@Component
public class ProductPricingFeignFallbackFactory extends AbstractFallbackFactory<ProductPricingFeign> {

  @Autowired
  private ProductPricingFeignFallback fallback;

  @Autowired
  public ProductPricingFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public ProductPricingFeign doCreate(Throwable cause) {
    return fallback;
  }
}
