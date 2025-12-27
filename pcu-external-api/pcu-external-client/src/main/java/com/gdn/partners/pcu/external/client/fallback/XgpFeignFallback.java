package com.gdn.partners.pcu.external.client.fallback;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.external.client.feign.XgpFeign;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.partners.pcu.external.service.model.request.XgpImageScaleRequest;
import org.springframework.stereotype.Component;

@Component
public class XgpFeignFallback implements XgpFeign {

  @Override
  public GdnBaseRestResponse scaleActiveProductNewImages(String clientHost,
      XgpImageScaleRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }
}
