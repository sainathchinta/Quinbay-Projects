package com.gdn.partners.pcu.internal.client.fallback;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.x.businesspartner.dto.BusinessPartnerCodesRequest;
import com.gdn.x.businesspartner.dto.MerchantNameResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;

/**
 * Created by parvej on 29/10/2019.
 */
public class XBPFeignFallbackTest {

  private static final int PAGE = 0;
  private static final int SIZE = 30;

  private XBPFeignFallback xbpFeignFallback = new XBPFeignFallback();

  @Test
  public void getAllActiveMerchantList() {
    GdnRestListResponse<ProfileResponse> response =
        xbpFeignFallback.getAllActiveMerchantList(new BusinessPartnerFilterRequest(), PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void filterByBusinessPartnerCode() {
    GdnRestSingleResponse<ProfileResponse> response = xbpFeignFallback.filterByBusinessPartnerCode(new String());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }
}
