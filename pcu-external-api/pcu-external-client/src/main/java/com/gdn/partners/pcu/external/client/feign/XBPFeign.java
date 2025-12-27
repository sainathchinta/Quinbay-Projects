package com.gdn.partners.pcu.external.client.feign;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.pcu.external.client.factory.XBPFeignFallbackFactory;
import com.gdn.partners.pcu.external.client.model.PickupPointOutboundResponse;
import com.gdn.x.businesspartner.dto.ProfileRequest;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.MarkPickupPointAsDefaultRequest;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * Created by govind on 10/12/2018 AD.
 */
@FeignClient(name = "xbpFeign", url = "${service.xbp.endpoint}", fallbackFactory = XBPFeignFallbackFactory.class)
public interface XBPFeign {

  @RequestMapping(value = "/api/businesspartner/{id}", method = RequestMethod.GET, produces = MediaType
      .APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProfileResponse> getProfileDetailById(@PathVariable("id") String profileId);

  @RequestMapping(value = "/api/v2/pickuppoint/filter", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<PickupPointOutboundResponse> filter(@RequestParam("page") int page,
    @RequestParam("size") int size, @RequestBody PickupPointFilterRequest request);

  @RequestMapping(value = "/api/v2/pickuppoint/mark-as-default", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
  BaseResponse updateDefaultPickupPointCode(@RequestBody MarkPickupPointAsDefaultRequest request);

  @RequestMapping(value = "/api/v2/businesspartner/update", method = RequestMethod.PUT, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProfileResponse> updateDefaultConfiguration(@RequestParam("type") String type, @RequestBody ProfileRequest profileRequest);

  @RequestMapping(value = "/api/v2/businesspartner/filter/code", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<ProfileResponse> filterByBusinessPartnerCode(
      @RequestParam("businessPartnerCode") String businessPartnerCode);

  @RequestMapping(value = "/api/v2/pickuppoint/filter/code", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
  GdnRestSingleResponse<PickupPointOutboundResponse> filterByCode(@RequestParam("code") String code);

  @RequestMapping(value = "/api/v2/businesspartner/filter", method = RequestMethod.POST,
                  consumes = MediaType.APPLICATION_JSON_VALUE, produces =
                      MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<ProfileResponse> getBusinessPartnerDetailsByList(
      @RequestParam("page") int page, @RequestParam("size") int size,
      BusinessPartnerFilterRequest request);

}
