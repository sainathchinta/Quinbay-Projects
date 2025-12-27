package com.gdn.partners.pcu.master.client.fallback;


import com.gdn.partners.pcu.master.client.feign.ProductCategorySuggestionFeign;
import com.gdn.partners.pcu.master.client.model.ProductCategorySuggestionResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class ProductCatgeorySuggestionFeignFallbackTest {

  private ProductCategorySuggestionFeign productCategorySuggestionFeign = new ProductCatgeorySuggestionFeignFallback();

  private static final String PRODUCT_NAME = "product_name";

  @Test
  void predictProductCategoriesByProductNameTest() {
    ProductCategorySuggestionResponse productCategoryPredictionResponse =
        productCategorySuggestionFeign.predictProductCategoriesByProductName(PRODUCT_NAME, 5, 0);
    Assertions.assertTrue(productCategoryPredictionResponse.getPredicted_categories().isEmpty());
    Assertions.assertEquals(PRODUCT_NAME, productCategoryPredictionResponse.getQuery());
  }

}
