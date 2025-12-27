package com.gdn.mta.bulk.feignConfig;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.models.MasterWarehouseResponse;
import com.gdn.mta.bulk.models.SimpleListAssemblyDisassemblyRequest;
import com.gdn.mta.bulk.models.TransferRequest;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface ProductAssemblyFeign {

  @RequestLine("GET /api/warehouse/getWarehouseCodeAndFulfillmentCenter?storeId={storeId}"
      + "&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}&page={page}&limit={limit}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<MasterWarehouseResponse> getWarehouseCodeAndFulfillmentCenter(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") String page, @Param("limit") String limit);

  @RequestLine("POST /api/product-assembly/{type}/assembly-disassembly-request?storeId={storeId}"
      + "&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse assemblyDisassemblyRequest(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      @Param("type") String type, SimpleListAssemblyDisassemblyRequest simpleListAssemblyDisassemblyRequest);

  @RequestLine("POST /api/product-assembly/transfer-request?storeId={storeId}"
      + "&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse transferRequest(@Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId, @Param("username") String username,
      TransferRequest transferRequest);
}
