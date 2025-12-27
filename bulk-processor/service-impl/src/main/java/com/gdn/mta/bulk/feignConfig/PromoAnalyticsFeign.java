package com.gdn.mta.bulk.feignConfig;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.promo.analytics.web.model.request.LowestPriceRecommendationRequest;
import com.gdn.partners.promo.analytics.web.model.response.LowestPriceRecommendationL5Response;
import com.gdn.partners.promo.analytics.web.model.response.LowestPriceRecommendationResponse;

import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface PromoAnalyticsFeign {

  @RequestLine("POST /api/lowest-price-recommendation/get-lowest-price?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<LowestPriceRecommendationResponse> getLowestPriceRecommendation(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      LowestPriceRecommendationRequest lowestPriceRecommendationRequest);

  @RequestLine("POST /api/v2/lowest-price-recommendation/get-lowest-price?"
      + "storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<LowestPriceRecommendationL5Response> getLowestPriceRecommendationV2(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      LowestPriceRecommendationRequest lowestPriceRecommendationRequest);
}
