package com.gdn.partners.pcu.external.client.fallback;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.external.client.feign.PickupPointFeign;
import com.gdn.partners.pcu.external.model.ErrorMessages;

/**
 * Created by govind on 13/12/2018 AD.
 */
@Component
public class PickupPointFeignFallback implements PickupPointFeign {

  @Override
  public GdnBaseRestResponse markDefaultAddress(@RequestParam String businessPartnerCode,
      @RequestParam String pickupPointCode){
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnRestListResponse<PickupPointResponse> fetchAccessiblePickupPoints(@RequestParam Integer page, @RequestParam Integer size,
      @RequestBody PickupPointFilterRequest request){
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }
}
