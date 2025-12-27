package com.gdn.partners.pcu.external.client.fallback;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.external.client.model.SubmitEvidenceIPRRequest;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

public class PDTFeignFallbackTest {

  private PDTFeignFallback pdtFeignFallback = new PDTFeignFallback();
  private  static final String REQUEST_ID ="requestId";

  @Test
  public void submitEvidence_test() {
    GdnBaseRestResponse response =
        pdtFeignFallback.submitEvidence(REQUEST_ID, SubmitEvidenceIPRRequest.builder().build());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }
}
