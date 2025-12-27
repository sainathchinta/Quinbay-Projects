package com.gdn.partners.pbp.outbound.AGPQuery;

import com.gda.mta.product.dto.response.AgpResponseVo;
import com.gda.mta.product.dto.response.AgpSimpleQueryResponse;

import feign.Headers;
import feign.Param;
import feign.RequestLine;

import java.util.Map;

public interface AGPQueryFeign {
  @RequestLine(
      "GET /api/order_retail_order_items/_simple-search?query[productGdnSku][is]={productSku}&from={from}&size={size}"
          + "&query[orderItemStatus][!in]={itemStatus}")
  @Headers({"Content-Type: application/json", "Accept: application/json","X-Service-Id: product"
    + "-business-partner"})
  AgpSimpleQueryResponse findNumberOfOrder(@Param("productSku") String productSku, @Param("from") String from,
      @Param("size") String size, @Param("itemStatus") String itemStatus);


  @RequestLine("POST /api-native/order_retail_order_items/_search")
  @Headers({"Content-Type: application/json", "Accept: application/json", "X-Service-Id: product-business-partner"})
  AgpResponseVo findNumberOfOrderByProductSkuList(Map<String, Object> jsonQuery);

}
