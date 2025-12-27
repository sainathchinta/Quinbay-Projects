package com.gdn.partners.pcu.master.client.feign;


import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.partners.pcu.master.client.factory.ProductCategorySuggestionFeignFallbackFactory;
import com.gdn.partners.pcu.master.client.model.ProductCategorySuggestionResponse;

@FeignClient(name = "productCatgeoryPredictionFeign", url = "${service.product-category-prediction.endpoint}", fallbackFactory = ProductCategorySuggestionFeignFallbackFactory.class)
public interface ProductCategorySuggestionFeign {

  @RequestMapping(value = "/get-category", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  ProductCategorySuggestionResponse predictProductCategoriesByProductName(@RequestParam("query") String query,
      @RequestParam("n") int n, @RequestParam("threshold") double threshold);

}
