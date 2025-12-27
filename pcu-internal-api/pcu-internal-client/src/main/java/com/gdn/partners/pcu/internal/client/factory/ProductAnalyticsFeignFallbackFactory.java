package com.gdn.partners.pcu.internal.client.factory;

import com.gdn.partners.pcu.internal.client.fallback.ProductAnalyticsFeignFallback;
import com.gdn.partners.pcu.internal.client.feign.ProductAnalyticsFeign;
import com.gdn.partners.pcu.internal.properties.ApplicationProperties;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class ProductAnalyticsFeignFallbackFactory extends AbstractFallbackFactory<ProductAnalyticsFeign> {

  @Autowired
  private ProductAnalyticsFeignFallback productAnalyticsFeignFallback;

  @Autowired
  public ProductAnalyticsFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public ProductAnalyticsFeign doCreate(Throwable cause) {
    return productAnalyticsFeignFallback;
  }
}
