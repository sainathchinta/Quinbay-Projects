package com.gdn.mta.bulk.feignConfig;

import com.gdn.mta.bulk.request.ShortUrlMappingRequest;
import com.gdn.mta.bulk.response.GenerateShortUrlResponse;
import com.gdn.mta.bulk.response.Response;

import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface CMSFeign {

  @RequestLine("GET /api/url-mapping/generate-shorturl")
  @Headers({"Accept: application/json"})
  Response<GenerateShortUrlResponse> generateShortUrl();

  @RequestLine("POST /api/url-mapping")
  @Headers({"Accept: application/json", "Content-Type: application/json", "Baggage-Blibli-Internal-User-Id: {userId}"})
  Response<Boolean> mapShortAndLongUrl(@Param("userId") String userId, ShortUrlMappingRequest shortUrlMappingRequest);
}
