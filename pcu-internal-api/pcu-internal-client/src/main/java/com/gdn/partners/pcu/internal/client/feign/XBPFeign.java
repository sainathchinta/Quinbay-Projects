package com.gdn.partners.pcu.internal.client.feign;


import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.client.factory.XBPFeignFallbackFactory;
import com.gdn.x.businesspartner.dto.BusinessPartnerCodesRequest;
import com.gdn.x.businesspartner.dto.MerchantNameResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;

/**
 * Created by parvej on 29/10/2019.
 */
@FeignClient(name = "xbpFeign", url = "${service.xbp.endpoint}", fallbackFactory = XBPFeignFallbackFactory.class)
public interface XBPFeign {

  @RequestMapping(value = "/api/v2/businesspartner/filter", method = RequestMethod.POST)
  GdnRestListResponse<ProfileResponse> getAllActiveMerchantList(
      @RequestBody BusinessPartnerFilterRequest request, @RequestParam("page") int page, @RequestParam("size") int size);

  @RequestMapping(value = "/api/v2/businesspartner/filter/code", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProfileResponse> filterByBusinessPartnerCode(
      @RequestParam("businessPartnerCode") String businessPartnerCode);
}
