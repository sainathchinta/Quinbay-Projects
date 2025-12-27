package com.gdn.mta.bulk.feignConfig;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.bulk.request.XgpImageScaleRequest;
import feign.Headers;
import feign.Param;
import feign.RequestLine;

public interface XgpFeign {
  @RequestLine("POST /api/operation/scale-active-product-new-images?"
      + "storeId={storeId}&clientId={clientId}" + "&requestId={requestId}")
  @Headers({"Content-Type: application/json", "Accept: application/json"})
  GdnBaseRestResponse scaleActiveProductNewImages(@Param("storeId") String storeId,
      @Param("clientId") String clientId, @Param("requestId") String requestId,
      XgpImageScaleRequest request);
}
