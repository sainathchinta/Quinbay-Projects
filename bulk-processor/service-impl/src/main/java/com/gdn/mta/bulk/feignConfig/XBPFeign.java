package com.gdn.mta.bulk.feignConfig;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.businesspartner.dto.BusinessPartnerCodesRequest;
import com.gdn.x.businesspartner.dto.MerchantNameResponse;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;

import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import feign.Headers;
import feign.Param;
import feign.RequestLine;
import org.springframework.web.bind.annotation.RequestBody;

public interface XBPFeign {

  @RequestLine(value = "GET /api/v2/businesspartner/filter/code?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&businessPartnerCode={businessPartnerCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProfileResponse> filterByBusinessPartnerCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("businessPartnerCode") String businessPartnerCode);

  @RequestLine(value = "POST /api/v2/businesspartner/filter?storeId={storeId}&channelId"
    + "={channelId}&clientId={clientId}&requestId={requestId}&username={username}&page={page"
    + "}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProfileResponse> getBusinessPartnerDetailsByList(
    @Param("storeId") String storeId, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("username") String username, @Param("page") int page, @Param("size") int size,
    BusinessPartnerFilterRequest request);

  @RequestLine(value = "POST /api/businesspartner/getNamesByBusinessPartnerCodes?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<MerchantNameResponse> getNamesByBusinessPartnerCodes(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, BusinessPartnerCodesRequest request);

  @RequestLine(value = "POST /api/v2/pickuppoint/filter?username={username}&page={page}&size"
    + "={size}&storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<PickupPointResponse> getPickupPointList(@Param("storeId") String storeId,
    @Param("username") String username, @Param("channelId") String channelId,
    @Param("clientId") String clientId, @Param("requestId") String requestId,
    @Param("page") int page, @Param("size") int size,
    @RequestBody PickupPointFilterRequest request);
}
