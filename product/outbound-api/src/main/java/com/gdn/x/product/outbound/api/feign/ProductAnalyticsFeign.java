package com.gdn.x.product.outbound.api.feign;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;
import com.gdn.x.product.outbound.api.feign.config.ProductAnalyticsProperties;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "productAnalyticsFeign", url = "${product.analytics.feign.host}", configuration = ProductAnalyticsProperties.class)
public interface ProductAnalyticsFeign {

  @GetMapping(value = "/product-analytics/api/seller/detail", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<SellerDetailResponse> findByMerchantAndCategory(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @RequestParam("merchantCode") String merchantCode);

}

