package com.gdn.partners.pbp.outbound.merchantEducation.feign;

import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pbp.outbound.merchantEducation.NotificationSettings;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface MerchantEducationFeign {

  @RequestLine("GET /api/user-notification-settings?storeId={storeId}&channelId={channelId}&clientId={clientId}&"
      + "requestId={requestId}&username={username}&targetUsername={targetUsername}&storeCode={storeCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  SingleBaseResponse<NotificationSettings> findByUsernameAndStoreCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("targetUsername") String targetUserName, @Param("storeCode") String storeCode);
}
