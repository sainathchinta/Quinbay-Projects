package com.gdn.partners.pcu.master.client.fallback;

import java.util.ArrayList;

import org.springframework.stereotype.Component;

import com.gdn.partners.pcu.master.client.feign.ProductCategorySuggestionFeign;
import com.gdn.partners.pcu.master.client.model.ProductCategorySuggestionResponse;

@Component
public class ProductCatgeorySuggestionFeignFallback implements ProductCategorySuggestionFeign {

  @Override
  public ProductCategorySuggestionResponse predictProductCategoriesByProductName(String query, int n,
      double threshold) {
    return new ProductCategorySuggestionResponse(new ArrayList<>(), query);
  }
}
