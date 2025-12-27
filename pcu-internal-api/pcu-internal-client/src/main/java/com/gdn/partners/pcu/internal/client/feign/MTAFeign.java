package com.gdn.partners.pcu.internal.client.feign;

import java.util.List;
import java.util.Map;


import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.internal.client.factory.MTAFeignFallbackFactory;

@FeignClient(name = "mtaFeign", url = "${service.mta.endpoint}", fallbackFactory = MTAFeignFallbackFactory.class)
public interface MTAFeign {

  @RequestMapping(value = "/api/internal/image-or-content-reviewers", method = RequestMethod.GET)
  GdnRestSimpleResponse<Map<String, List<String>>> getImageOrContentReviewers(
      @RequestParam("reviewerType") String reviewerType);
}