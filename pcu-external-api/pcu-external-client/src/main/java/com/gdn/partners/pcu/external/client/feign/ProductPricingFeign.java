package com.gdn.partners.pcu.external.client.feign;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.client.factory.ProductPricingFeignFallbackFactory;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceSkuDetailListRequest;
import com.gdn.partners.product.pricing.web.model.response.PromoSkuDetailResponse;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "productPricingFeign", url = "${service.product-pricing.endpoint}", fallbackFactory = ProductPricingFeignFallbackFactory.class)
public interface ProductPricingFeign {

  @RequestMapping(value = "/api/promo-adjustment/skuDetail/{itemSku}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<PromoSkuDetailResponse> getPromoSkuDetail(@RequestParam("storeId") String storeId,
      @RequestParam("requestId") String requestId, @PathVariable("itemSku") String itemSku);

  @RequestMapping(value = "/api/wholesale-price/sku/detail/list", method = RequestMethod.POST,
      produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<WholesalePriceSkuResponse> getWholesalePriceSkuDetail(@RequestParam("storeId") String storeId,
      @RequestParam("requestId") String requestId, @RequestBody WholesalePriceSkuDetailListRequest request);

  @RequestMapping(value = "/api/wholesale-price/sku/detail/{itemSku}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<WholesalePriceSkuResponse> getWholesalePriceDetail(@PathVariable("itemSku") String itemSku);

  @RequestMapping(value = "/api/v2/wholesale-price/sku/detail/list", method = RequestMethod.POST,
    produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<WholesalePriceSkuResponse> getWholesalePriceSkuDetailV2(@RequestParam(
    "storeId") String storeId,
    @RequestParam("requestId") String requestId, @RequestBody WholesalePriceSkuDetailListRequest request);

  @RequestMapping(value = "api/v2/promo-adjustment/skuDetail/{itemSku}", method =
    RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<PromoSkuDetailResponse> getPromoSkuDetailV2(@RequestParam("storeId") String storeId,
    @RequestParam("requestId") String requestId, @PathVariable("itemSku") String itemSku);
}
