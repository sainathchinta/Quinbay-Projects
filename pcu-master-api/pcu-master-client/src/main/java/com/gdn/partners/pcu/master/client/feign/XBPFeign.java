package com.gdn.partners.pcu.master.client.feign;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.master.client.factory.XBPFeignFallbackFactory;
import com.gdn.x.businesspartner.dto.ProfileResponse;

@FeignClient(name = "XBPFeign", url = "${service.xbp.endpoint}", fallbackFactory = XBPFeignFallbackFactory.class)
public interface XBPFeign {

  @RequestMapping(value = "/api/v2/businesspartner/filter/code", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProfileResponse> filterByBusinessPartnerCode(@RequestParam("businessPartnerCode") String businessPartnerCode);

}
