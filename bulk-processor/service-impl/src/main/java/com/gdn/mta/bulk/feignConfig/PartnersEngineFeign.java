package com.gdn.mta.bulk.feignConfig;

import com.gdn.mta.bulk.dto.product.UserResponse;
import com.gdn.mta.bulk.request.UserFilter;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface PartnersEngineFeign {

  @RequestLine("POST /api/user/filter?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  ListBaseResponse<UserResponse> userFilter(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("page") int page, @Param("size") int size, UserFilter request);

}