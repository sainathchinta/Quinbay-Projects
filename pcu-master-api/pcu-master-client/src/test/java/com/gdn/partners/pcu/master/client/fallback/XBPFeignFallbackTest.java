package com.gdn.partners.pcu.master.client.fallback;


import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.master.model.ErrorMessages;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class XBPFeignFallbackTest {

  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";

  private XBPFeignFallback xbpFeignFallback = new XBPFeignFallback();

  @Test
  void filterByBusinessPartnerCodeTest() {
    GdnRestSingleResponse<ProfileResponse> gdnRestSingleResponse =
        xbpFeignFallback.filterByBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    Assertions.assertFalse(gdnRestSingleResponse.isSuccess());
    Assertions.assertEquals(gdnRestSingleResponse.getErrorMessage(), ErrorMessages.FALLBACK_ERR_MESSAGE);
  }
}
