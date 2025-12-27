package com.gdn.partners.pcu.master.client.factory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.master.client.fallback.ProductCatgeorySuggestionFeignFallback;
import com.gdn.partners.pcu.master.client.feign.ProductCategorySuggestionFeign;
import com.gdn.partners.pcu.master.properties.ApplicationProperties;

@Component
public class ProductCategorySuggestionFeignFallbackFactory extends AbstractFallbackFactory<ProductCategorySuggestionFeign> {

  @Autowired
  private ProductCatgeorySuggestionFeignFallback fallback;

  @Autowired
  public ProductCategorySuggestionFeignFallbackFactory(ApplicationProperties applicationProperties) {
    super(applicationProperties);
  }

  @Override
  public ProductCategorySuggestionFeign doCreate(Throwable cause) {
    return fallback;
  }
}
