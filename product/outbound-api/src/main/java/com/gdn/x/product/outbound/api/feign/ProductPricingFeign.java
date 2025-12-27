package com.gdn.x.product.outbound.api.feign;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.product.pricing.web.model.promo.bundling.request.PromoBundlingRequest;
import com.gdn.partners.product.pricing.web.model.promo.bundling.response.PromoBundlingSkuByItemSkuAndItemCodesResponse;
import com.gdn.partners.product.pricing.web.model.promo.bundling.response.PromoBundlingSkuDetailResponse;
import com.gdn.x.product.outbound.api.feign.config.PricingFeignProperties;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "pricingClient", url = "${product.pricing.host}", configuration = PricingFeignProperties.class)
public interface ProductPricingFeign {

  @PostMapping(value = "/api/promo-bundling/get-active-bundling-promos", consumes =
    MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<PromoBundlingSkuDetailResponse> getActiveBundlingPromos(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username,
    @RequestBody PromoBundlingRequest promoBundlingRequest);

  @PostMapping(value = "/api/promo-bundling/get-bundling-promos-by-ids", consumes =
    MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<PromoBundlingSkuDetailResponse> getBundlingPromosByIds(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username,
    @RequestBody PromoBundlingRequest promoBundlingRequest);

  @PostMapping(value = "/api/promo-bundling/get-active-bundling-promos-by-itemcodes-and-itemSku",
    produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<PromoBundlingSkuByItemSkuAndItemCodesResponse> getActiveAndPromoBundlingTotalByItemCodes(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @RequestParam("itemSku") String itemSku,
    @RequestBody PromoBundlingRequest promoBundlingRequest);

  @PostMapping(value = "/api/promo-bundling/get-active-bundling-promos-by-itemcodes", consumes =
    MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<PromoBundlingSkuDetailResponse> getActiveBundlingPromosByTypeAndItemCodes(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @RequestParam("page") int page,
    @RequestParam("size") int size, @RequestParam("sortBy") String sortBy,
    @RequestParam("sortType") String sortType,
    @RequestBody PromoBundlingRequest promoBundlingRequest);
}
