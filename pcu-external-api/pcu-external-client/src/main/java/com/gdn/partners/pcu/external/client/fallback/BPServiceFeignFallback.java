package com.gdn.partners.pcu.external.client.fallback;

import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.RequestBody;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.external.client.feign.BPServiceFeign;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.x.bpservice.dto.request.BusinessPartnerHistoryRequest;

/**
 * Created by govind on 13/12/2018 AD.
 */
@Component
public class BPServiceFeignFallback implements BPServiceFeign {

  @Override
  public GdnBaseRestResponse saveBusinessPartnerHistory(
      @RequestBody BusinessPartnerHistoryRequest businessPartnerHistoryRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null);
  }
}
