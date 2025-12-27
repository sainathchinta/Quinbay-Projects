package com.gdn.partners.pbp.outbound.sellerLogistics.feign;

import java.util.List;

import com.blibli.oss.backend.common.model.response.Response;
import com.gdn.seller.logistics.web.model.request.SaveSkuLogisticProductRequest;
import com.gdn.seller.logistics.web.model.response.GetSkuLogisticProductResponse;
import com.gdn.seller.logistics.web.model.response.SaveSkuLogisticProductResponse;

import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface SellerLogisticsFeign {

  @RequestLine("GET /api/sku/logistics/get?itemSku={itemSku}&merchantCode={merchantCode}&merchantDeliveryType={merchantDeliveryType}&"
      + "requestId={requestId}&channelId={channelId}&storeId={storeId}"
      + "&clientId={clientId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  Response<List<GetSkuLogisticProductResponse>> getSkuLogistics(@Param("itemSku") String itemSku,
      @Param("merchantCode") String merchantCode,
      @Param("merchantDeliveryType") String merchantDeliveryType,
      @Param("requestId") String requestId, @Param("channelId") String channelId,
      @Param("storeId") String storeId, @Param("clientId") String clientId,
      @Param("username") String username);

  @RequestLine("POST /api/sku/logistics/save?storeId={storeId}&channelId={channelId}&clientId={clientId"
      + "}&requestId={requestId}&username={username}&isActive={isActive}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  Response<SaveSkuLogisticProductResponse> saveSkuLogistics(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @Param("isActive") boolean isActive,
      SaveSkuLogisticProductRequest saveSkuLogisticProductRequest);

}
