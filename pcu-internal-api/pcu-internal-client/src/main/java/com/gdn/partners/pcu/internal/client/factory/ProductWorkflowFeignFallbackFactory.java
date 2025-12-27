package com.gdn.partners.pcu.internal.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import com.gdn.partners.pcu.internal.client.fallback.ProductWorkflowFeignFallback;
import com.gdn.partners.pcu.internal.client.feign.ProductWorkflowFeign;
import com.gdn.partners.pcu.internal.properties.ApplicationProperties;

/**
 * Created by govind on 11/01/2019 AD.
 */

@Component
public class ProductWorkflowFeignFallbackFactory extends AbstractFallbackFactory<ProductWorkflowFeign> {

  @Autowired
  private ProductWorkflowFeignFallback fallback;

  @Autowired
  public ProductWorkflowFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override public ProductWorkflowFeign doCreate(Throwable cause) {
    return fallback;
  }
}
