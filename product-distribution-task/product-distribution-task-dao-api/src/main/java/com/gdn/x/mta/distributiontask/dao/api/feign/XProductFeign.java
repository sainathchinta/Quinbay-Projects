package com.gdn.x.mta.distributiontask.dao.api.feign;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.product.rest.web.model.request.ProductSkuAndProductCodeRequest;
import com.gdn.x.product.rest.web.model.response.PrdProductResponse;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface XProductFeign {
  @RequestLine("GET /api/product-v2/getProductL3DetailByProductSkuOrProductCode?storeId={storeId}&channelId={channelId}"
      + "&clientId={clientId}&requestId={requestId}&username={username}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnRestListResponse<PrdProductResponse> getPrdProductDetailByProductSkuOrProductCode(@Param("storeId") String storeId,
      @Param("channelId") String channelId, @Param("clientId") String clientId, @Param("requestId") String requestId,
      @Param("username") String username, ProductSkuAndProductCodeRequest request);
}
