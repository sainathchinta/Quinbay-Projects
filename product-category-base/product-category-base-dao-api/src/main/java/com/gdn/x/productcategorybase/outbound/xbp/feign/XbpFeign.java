package com.gdn.x.productcategorybase.outbound.xbp.feign;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;


@FeignClient(name = "xbpFeign", url = "${service.xbp.endpoint}")
public interface XbpFeign {
  @GetMapping(value = "/api/v2/businesspartner/filter/code", produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProfileResponse> getBusinessPartnerDetails(@RequestParam("storeId") String storeId,
      @RequestParam("channelId") String channelId, @RequestParam("clientId") String clientId,
      @RequestParam("requestId") String requestId, @RequestParam("username") String username,
      @RequestParam("businessPartnerCode") String businessPartnerCode);
}
