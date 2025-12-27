package com.gdn.x.mta.distributiontask.dao.api.feign;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface XBPFeign {

  @RequestLine("GET /api/v2/businesspartner/filter/code?storeId={storeId}&channelId={channelId"
      + "}&clientId={clientId}&requestId={requestId}&username={username}&businessPartnerCode"
      + "={businessPartnerCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProfileResponse> filterByBusinessPartnerCode(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("businessPartnerCode") String businessPartnerCode);
}
