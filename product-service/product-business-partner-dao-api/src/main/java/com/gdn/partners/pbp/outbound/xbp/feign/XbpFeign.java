package com.gdn.partners.pbp.outbound.xbp.feign;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.businesspartner.dto.BulkRequest;
import com.gdn.x.businesspartner.dto.ExternalPickupPointCodeResponse;
import com.gdn.x.businesspartner.dto.ExternalPickupPointCodesRequest;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProductCounterResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface XbpFeign {
  @RequestLine("GET /api/v2/businesspartner/filter/code?businessPartnerCode={businessPartnerCode}&username={username}&accessibility={accessibility}&dormant={dormant}&storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProfileResponse> getBusinessPartnerDetails(
      @Param("businessPartnerCode") String businessPartnerCode, @Param("username") String username,
      @Param("accessibility") boolean accessibility, @Param("dormant") boolean dormant,
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId);

  @RequestLine("POST /api/v2/businesspartner/filter?username={username}&page={page}&size={size}&storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<ProfileResponse> getBusinessPartnersDetails(@Param("username") String username,
      BusinessPartnerFilterRequest businessPartnerFilterRequest, @Param("page") int page, @Param("size") int size,
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId);

  @RequestLine("POST /api/v2/pickuppoint/filter?username={username}&page={page}&size={size}&storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<PickupPointResponse> filter(@Param("username") String username, @Param("page") int page,
      @Param("size") int size, @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, PickupPointFilterRequest request);

  @RequestLine("POST /api/externalPickupPoint/filterByExternalPickupPointCodes?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&businessPartnerCode={businessPartnerCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ExternalPickupPointCodeResponse> filterPickupPointByExternalPickupPointCodes(
      @Param("storeId") String storeId, @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("businessPartnerCode") String businessPartnerCode,
      ExternalPickupPointCodesRequest externalPickupPointCodesRequest);

  @RequestLine("POST /api/pickupoint/getByPickupPointCodes?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<PickupPointResponse> getByPickupPointCodes(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      BulkRequest<String> pickupPointCodes);

  @RequestLine("GET /api/pickupoint/getByPickupPointCode?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&code={code}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<PickupPointResponse> getByPickupPointCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("code") String code);

  @RequestLine("GET /api/product-counter/increment-and-get?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&businessPartnerCode={businessPartnerCode}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<ProductCounterResponse> incrementAndGetProductCounter(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("businessPartnerCode") String businessPartnerCode);

}
