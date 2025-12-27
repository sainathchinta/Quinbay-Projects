package com.gdn.partners.pcu.external.client.factory;

import com.gdn.partners.pcu.external.client.fallback.ProductAnalyticsFeignFallback;
import com.gdn.partners.pcu.external.client.feign.ProductAnalyticsFeign;
import com.gdn.partners.pcu.external.properties.ApplicationProperties;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ProductAnalyticsFeignFallbackFactory extends AbstractFallbackFactory<ProductAnalyticsFeign> {

  @Autowired
  private ProductAnalyticsFeignFallback fallback;

  @Autowired
  public ProductAnalyticsFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public ProductAnalyticsFeign doCreate(Throwable cause) {
    return fallback;
  }
}
