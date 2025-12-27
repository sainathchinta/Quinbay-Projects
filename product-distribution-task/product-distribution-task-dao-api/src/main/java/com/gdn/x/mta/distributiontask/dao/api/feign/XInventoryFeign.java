package com.gdn.x.mta.distributiontask.dao.api.feign;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.mta.distributiontask.model.dto.L2StockDetailResponse;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface XInventoryFeign {

  @RequestLine(
      "GET /inventoryInfo/findStockAvailabilityByWarehouseItemSku?storeId={storeId}&channelId"
          + "={channelId}&clientId={clientId}&requestId={requestId}&username={username"
          + "}&warehouseItemSku={warehouseItemSku}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestSingleResponse<L2StockDetailResponse> getStockDetailsByWarehouseItemSku(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("warehouseItemSku") String warehouseItemSku);
}
