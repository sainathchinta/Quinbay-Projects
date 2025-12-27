package com.gdn.partners.pcu.internal.client.feign;


import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.partners.pcu.internal.client.factory.BPJPHFeignFallbackFactory;
import com.gdn.partners.pcu.internal.client.model.response.BPJPHListResponse;
import com.gdn.partners.pcu.internal.client.model.response.HalalCertificationDetailResponse;

@FeignClient(name = "bpjphFeign", url = "${service.bpjph.endpoint}", fallbackFactory = BPJPHFeignFallbackFactory.class)
public interface BPJPHFeign {

  @RequestMapping(value = "/sertifikat", method = RequestMethod.GET)
  BPJPHListResponse<HalalCertificationDetailResponse> getHalalCertificationDetails(
      @RequestHeader("Authorization") String apiKey, @RequestParam("page") int page, @RequestParam("size") int size,
      @RequestParam("no_sertifikat") String no_sertifikat);
}
