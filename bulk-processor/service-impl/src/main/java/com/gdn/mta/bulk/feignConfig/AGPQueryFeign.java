package com.gdn.mta.bulk.feignConfig;

import com.gdn.mta.bulk.response.AgpSimpleQueryResponse;

import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface AGPQueryFeign {

  @RequestLine(
      "GET /api/order_retail_order_items/_simple-search?query[productGdnSku][is]={productSku}&from={from}&size={size}"
          + "&query[orderItemStatus][!in]={itemStatus}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  AgpSimpleQueryResponse findNumberOfOrder(@Param("productSku") String productSku, @Param("from") String from,
      @Param("size") String size, @Param("itemStatus") String itemStatus);

}
