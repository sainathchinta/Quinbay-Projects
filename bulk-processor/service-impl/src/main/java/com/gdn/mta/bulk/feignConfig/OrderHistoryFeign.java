package com.gdn.mta.bulk.feignConfig;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.mta.bulk.models.download.responsedata.OfflinePackageSummaryResponse;
import com.gdn.mta.bulk.models.download.responsedata.OrderItemSummaryResponse;
import com.gdn.x.neo.order.client.sdk.web.model.request.OrderItemSummaryRequest;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface OrderHistoryFeign {

  @RequestLine(
      "POST /api/summary/bulk-download?storeId={storeId}&channelId={channelId}&clientId={clientId}"
          + "&requestId={requestId}&username={username}&page={page}&size={size}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<OrderItemSummaryResponse> getOrderItemsForBulkDownload(
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("page") int page, @Param("size") int size,
      OrderItemSummaryRequest orderItemSummaryRequest);

  @RequestLine("POST /api/summary/offline/listing?storeId={storeId}&channelId={channelId}&clientId={clientId}"
      + "&requestId={requestId}&username={username}&orderBy={orderBy}&sortBy={sortBy}&page={page}&size={size}"
      + "&bulk=true")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<OfflinePackageSummaryResponse> findOfflinePackageSummary(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, @Param("orderBy") String orderBy, @Param("sortBy") String sortBy,
      @Param("page") int page, @Param("size") int size, OrderItemSummaryRequest filterRequest);
}
