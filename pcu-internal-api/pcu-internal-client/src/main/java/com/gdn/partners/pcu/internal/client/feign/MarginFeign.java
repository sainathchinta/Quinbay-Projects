package com.gdn.partners.pcu.internal.client.feign;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.margin.webmodel.MarginCategoryResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.pcu.internal.client.factory.MarginFeignFallbackFactory;
import com.gdn.partners.pcu.internal.client.model.request.FilterMarginsByOrderItemsRequest;
import com.gdn.partners.pcu.internal.client.model.response.OrderItemMarginsResponse;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "marginFeign", url = "${service.margin.endpoint}", fallbackFactory = MarginFeignFallbackFactory.class)
public interface MarginFeign {

  @RequestMapping(value = "/api/margin-category/filter/category/{categoryId}/order/date/{orderDate}",
      method = RequestMethod.GET)
  GdnRestSingleResponse<MarginCategoryResponse> filterMarginCategoryByCategoryCodeAndOrderDate(
      @PathVariable("categoryId") String categoryCode, @PathVariable("orderDate") String orderDate);

  @RequestMapping(value = "/api/v2/margin/order-item/filter?storeId={storeId}&channelId={channelId}&clientId={clientId}&requestId={requestId}&username={username}", method = RequestMethod.POST)
  ListBaseResponse<OrderItemMarginsResponse> filterMargin(@RequestParam("storeId") String storeId,
      @RequestParam("channelId") String channelId, @RequestParam("clientId") String clientId,
      @RequestParam("requestId") String requestId, @RequestParam("username") String username,
      @RequestBody FilterMarginsByOrderItemsRequest marginOrderItem);

}
