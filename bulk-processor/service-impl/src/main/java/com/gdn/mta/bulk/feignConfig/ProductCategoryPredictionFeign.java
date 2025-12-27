package com.gdn.mta.bulk.feignConfig;


import com.gdn.mta.bulk.dto.ProductCategorySuggestionResponse;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface ProductCategoryPredictionFeign {

    @RequestLine("GET /get-category/?query={query}&other_ecommerce_cat={other_ecommerce_cat}")
    @Headers({"Content-Type: application/json", "Accept: application/json"})
    ProductCategorySuggestionResponse predictProductCategoriesByProductName(
        @Param("query") String query, @Param("other_ecommerce_cat") String externalCategory);

}
