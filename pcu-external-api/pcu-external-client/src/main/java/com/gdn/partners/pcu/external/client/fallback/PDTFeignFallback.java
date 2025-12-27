package com.gdn.partners.pcu.external.client.fallback;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.external.client.feign.PDTFeign;
import com.gdn.partners.pcu.external.client.model.SubmitEvidenceIPRRequest;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import org.springframework.stereotype.Component;

@Component
public class PDTFeignFallback implements PDTFeign {
  @Override
  public GdnBaseRestResponse submitEvidence(String requestId,
      SubmitEvidenceIPRRequest submitEvidenceIPRRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }
}