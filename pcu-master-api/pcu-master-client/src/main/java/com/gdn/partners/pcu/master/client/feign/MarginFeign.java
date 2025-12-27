package com.gdn.partners.pcu.master.client.feign;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.margin.webmodel.MarginCategoryResponse;
import com.gdn.mta.margin.webmodel.MarginOrderResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.pcu.master.client.factory.MarginFeignFallbackFactory;
import com.gdn.partners.pcu.master.client.model.BaseMarginResponse;
import com.gdn.partners.pcu.master.client.model.CategoryCodesListRequest;
import com.gdn.partners.pcu.master.client.model.FilterMarginsByOrderItemsRequest;
import com.gdn.partners.pcu.master.client.model.OrderItemMarginsResponse;

@FeignClient(name = "marginFeign", url = "${service.margin.endpoint}", fallbackFactory = MarginFeignFallbackFactory.class)
public interface MarginFeign {

  @RequestMapping(value = "/api/margin-category/filter/category/{categoryId}/order/date/{orderDate}", method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<MarginCategoryResponse> filterMarginCategoryByCategoryCodeAndOrderDate(
      @PathVariable("categoryId") String categoryCode, @PathVariable("orderDate") String orderDate);


  @RequestMapping(value = "/api/margin-order/filter/business-partner-sku/{businessPartnerId}/gdnsku/{gdnSku}/category/{categoryId}/order/date/{orderDate}", method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<MarginOrderResponse> filterMarginBusinessPartnerByBusinessPartnerIdAndCategoryIdAndOrderDate(
      @PathVariable("businessPartnerId") String businessPartnerId, @PathVariable("categoryId") String categoryCode,
      @PathVariable("orderDate") String orderDate, @PathVariable("gdnSku") String gdnSku);

  @RequestMapping(value = "/api/base-margin/active/_filter", method = RequestMethod.POST)
  ListBaseResponse<BaseMarginResponse> filterActiveBasicMargin(
      @RequestBody CategoryCodesListRequest request);

  @RequestMapping(value = "/api/v2/margin/order-item/filter",method = RequestMethod.POST)
  ListBaseResponse<OrderItemMarginsResponse> filterMargin(@RequestParam("storeId") String storeId,
      @RequestParam("channelId") String channelId,
      @RequestParam("clientId") String clientId,
      @RequestParam("requestId") String requestId,
      @RequestParam("username") String username,
      @RequestBody FilterMarginsByOrderItemsRequest marginOrderItem);

}
