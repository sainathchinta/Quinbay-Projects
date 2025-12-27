package com.gdn.partners.product.analytics.client.XProduct.feign;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.product.analytics.client.factory.XProductFeignFallbackFactory;
import com.gdn.partners.product.analytics.client.response.SimpleBooleanResponse;

@FeignClient(name = "xProductFeign", url = "${service.x-product.endpoint}", fallbackFactory = XProductFeignFallbackFactory.class)
public interface XProductFeign {

  @RequestMapping(value = "/api/product/{sellerCode}/{productCode}/sharedProduct", method =
    RequestMethod.GET
    , produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<SimpleBooleanResponse> checkIfProductIsShared(
    @PathVariable("sellerCode") String sellerCode, @PathVariable("productCode") String productCode);
}
