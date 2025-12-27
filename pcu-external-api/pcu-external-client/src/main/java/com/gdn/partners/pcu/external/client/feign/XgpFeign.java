package com.gdn.partners.pcu.external.client.feign;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.external.client.factory.XgpFeignFallbackFactory;
import com.gdn.partners.pcu.external.service.model.request.XgpImageScaleRequest;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;

@FeignClient(name = "XgpFeign", url = "${service.graphics.processor.endpoint}", fallbackFactory =
    XgpFeignFallbackFactory.class)
public interface XgpFeign {
  @PostMapping(value = "/api/operation/scale-active-product-new-images", produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnBaseRestResponse scaleActiveProductNewImages(
      @RequestParam(value = "clientId", defaultValue = "pcu-external-api") String clientId,
      @RequestBody XgpImageScaleRequest request);
}
