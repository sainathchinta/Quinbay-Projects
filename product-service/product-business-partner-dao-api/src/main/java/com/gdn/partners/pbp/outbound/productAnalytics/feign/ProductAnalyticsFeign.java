package com.gdn.partners.pbp.outbound.productAnalytics.feign;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.product.analytics.web.model.AutoQCDetailResponse;
import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface ProductAnalyticsFeign {

  @RequestLine("GET /api/auto-qc/find-by-merchant-category?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}&merchantCode={merchantCode}&categoryCode={categoryCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<AutoQCDetailResponse> getAutoQCDetails(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("merchantCode") String merchantCode, @Param("categoryCode") String categoryCode);

  @RequestLine("GET /api/seller/detail?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}&merchantCode={merchantCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SellerDetailResponse> getSellerDetail(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("merchantCode") String merchantCode);
}
