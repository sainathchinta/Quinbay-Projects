package com.gdn.partners.pcu.internal.client.feign;


import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.internal.client.factory.XGPFeignFallbackFactory;

@FeignClient(name = "xgpFeign", url = "${service.xgp.endpoint}", fallbackFactory = XGPFeignFallbackFactory.class)
public interface XGPFeign {

  @RequestMapping(value = "/api/image/check-size", method = RequestMethod.GET,
      produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSimpleResponse<String> checkImageSizeByImageFilename(@RequestParam("imageFilename") String imageFilename);
}
