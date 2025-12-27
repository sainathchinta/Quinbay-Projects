package com.gdn.partners.pcu.external.client.feign;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.external.client.factory.PickupPointFeignFallbackFactory;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;

/**
 * Created by govind on 12/12/2018 AD.
 */
@FeignClient(name = "pickupPointFeign", url = "${service.xbp.endpoint}", fallbackFactory = PickupPointFeignFallbackFactory.class)
public interface PickupPointFeign {

  @RequestMapping(value = "/api/pickupoint/markDefaultAddress", method = RequestMethod.PUT,
                  produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType
      .APPLICATION_JSON_VALUE)
  GdnBaseRestResponse markDefaultAddress(@RequestParam("businessPartnerCode") String businessPartnerCode,
      @RequestParam("pickupPointCode") String pickupPointCode);

  @RequestMapping(value = "api/v2/pickuppoint/filter", method = RequestMethod.POST, produces =
      MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
  GdnRestListResponse<PickupPointResponse> fetchAccessiblePickupPoints(@RequestParam Integer page,
      @RequestParam Integer size, @RequestBody PickupPointFilterRequest request);
}
