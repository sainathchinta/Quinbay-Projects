package com.gdn.partners.pcu.external.client.fallback;


import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.external.service.model.request.XgpImageScaleRequest;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

class XgpFeignFallbackTest {

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);
  }

  @InjectMocks
  private XgpFeignFallback xgpFeignFallback;

  @Test
  public void scaleActiveProductNewImagesTest() {
    GdnBaseRestResponse response = xgpFeignFallback.scaleActiveProductNewImages("pcu-external-api",
        new XgpImageScaleRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }
}