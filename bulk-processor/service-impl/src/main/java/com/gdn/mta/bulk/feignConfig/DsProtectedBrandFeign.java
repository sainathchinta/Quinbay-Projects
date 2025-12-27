package com.gdn.mta.bulk.feignConfig;

import com.gdn.mta.bulk.models.BrandPredictionRequest;
import com.gdn.mta.bulk.dto.BrandSuggestionResponse;
import feign.Headers;
import feign.RequestLine;

public interface DsProtectedBrandFeign {
    @RequestLine("POST /get-predictions")
    @Headers({"Content-Type: application/json", "Accept: application/json"})
    BrandSuggestionResponse predictProductBrandByProductName(BrandPredictionRequest brandPredictionRequest);
}
