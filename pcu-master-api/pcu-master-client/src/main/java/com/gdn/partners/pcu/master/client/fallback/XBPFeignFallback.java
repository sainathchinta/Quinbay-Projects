package com.gdn.partners.pcu.master.client.fallback;

import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.master.client.feign.XBPFeign;
import com.gdn.partners.pcu.master.model.ErrorMessages;
import com.gdn.x.businesspartner.dto.ProfileResponse;

@Component
public class XBPFeignFallback implements XBPFeign {

  @Override
  public GdnRestSingleResponse<ProfileResponse> filterByBusinessPartnerCode(String businessPartnerCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }
}
