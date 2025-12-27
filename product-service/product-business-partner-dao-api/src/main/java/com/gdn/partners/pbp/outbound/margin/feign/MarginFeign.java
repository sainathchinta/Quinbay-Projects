package com.gdn.partners.pbp.outbound.margin.feign;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.margin.webmodel.MarginCategoryResponse;
import org.springframework.web.bind.annotation.RequestBody;

import com.gdn.mta.product.valueobject.FilterMarginsByOrderItemsRequest;
import com.gdn.mta.product.valueobject.OrderItemMarginsResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface MarginFeign {

  @RequestLine("POST /api/v2/margin/order-item/filter?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  ListBaseResponse<OrderItemMarginsResponse> filterMargin(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId,
      @Param("requestId") String requestId, @Param("username") String username,
      @RequestBody FilterMarginsByOrderItemsRequest marginOrderItem);

  @RequestLine("GET /api/margin-category/filter/category/{categoryId}/order/date"
      + "/{orderDate}?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId"
      + "={requestId}")
  @Headers("Accept: application/json")
  GdnRestSingleResponse<MarginCategoryResponse> filterMarginCategoryByCategoryIdAndOrderDate(
      @Param("categoryId") String categoryId, @Param("orderDate") String orderDate,
      @Param("storeId") String storeId, @Param("channelId") String channelId,
      @Param("clientId") String clientId, @Param("requestId") String requestId);

}
