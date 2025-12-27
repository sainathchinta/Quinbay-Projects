package com.gdn.partners.pcu.external.client.feign;

import com.gdn.partners.pcu.external.client.factory.AGPQueryFeignFallbackFactory;
import com.gdn.partners.pcu.external.client.helper.AgpSimpleQueryResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "AGPQueryFeign", url = "${service.agp-query.endpoint}", fallbackFactory = AGPQueryFeignFallbackFactory.class)
public interface AGPQueryFeign {

  @GetMapping(value = "/api/order_retail_order_items/_simple-search", produces = MediaType.APPLICATION_JSON_VALUE, headers = {"X-Service-Id=pcu-external-app"})
  AgpSimpleQueryResponse findNumberOfOrder(@RequestParam("query[productGdnSku][is]") String productSku,
      @RequestParam("from") String from, @RequestParam("size") String size,
      @RequestParam("query[orderItemStatus][!in]") String itemStatus);

}
