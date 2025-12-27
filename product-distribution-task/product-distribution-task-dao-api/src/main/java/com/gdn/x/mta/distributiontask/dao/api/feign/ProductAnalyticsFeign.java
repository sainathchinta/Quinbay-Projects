package com.gdn.x.mta.distributiontask.dao.api.feign;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.mta.distributiontask.model.dto.SellerAnalyticsResponse;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface ProductAnalyticsFeign {

  @RequestLine("GET /api/seller-analytics/details/{sellerCode}?storeId={storeId}&channelId="
      + "{channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<SellerAnalyticsResponse> getSellerAnalyticsDetailsBySellerCode(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("sellerCode") String sellerCode);
}
