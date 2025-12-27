package com.gdn.x.product.outbound.api.feign;


import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.product.model.response.AdjustmentProductChangeResponseVO;
import com.gdn.x.product.model.response.AdjustmentProductResponse;
import com.gdn.x.product.outbound.api.feign.config.PromotionFeignProperties;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.promotion.rest.web.model.dto.request.AdjustmentProductSkuRequest;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

import java.util.List;

@FeignClient(name = "promotionClient", url = "${api.promotion.host}", configuration = PromotionFeignProperties.class)
public interface PromotionFeign {
  @PostMapping(value = "/api/adjustmentProduct/findBySku", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<AdjustmentProductResponse> getPromosBySku(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username,
    @RequestBody AdjustmentProductSkuRequest campaignPriceRequest);

  @PostMapping(value = "/api/adjustmentProduct/findBySkuAndPickupPointCodeList", consumes =
    MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<AdjustmentProductChangeResponseVO> getAdjustmentProductBySkuAndPickupPointCodeList(
    @RequestParam("storeId") String storeId, @RequestParam("channelId") String channelId,
    @RequestParam("clientId") String clientId, @RequestParam("requestId") String requestId,
    @RequestParam("username") String username, @RequestBody List<ItemPickupPointRequest> itemRequests);

}
