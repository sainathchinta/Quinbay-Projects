package com.gdn.partners.pcu.external.client.fallback;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.x.bpservice.dto.request.BusinessPartnerHistoryRequest;

/**
 * Created by govind on 19/12/2018 AD.
 */
public class BPServiceFeignFallbackTest {

  private BPServiceFeignFallback bpServiceFeignFallback = new BPServiceFeignFallback();

  @Test
  public void saveTest() {
    GdnBaseRestResponse response = bpServiceFeignFallback.saveBusinessPartnerHistory(new BusinessPartnerHistoryRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }
}
