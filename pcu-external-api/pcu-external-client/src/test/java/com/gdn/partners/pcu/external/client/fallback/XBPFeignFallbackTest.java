package com.gdn.partners.pcu.external.client.fallback;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.pcu.external.client.model.PickupPointOutboundResponse;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.x.businesspartner.dto.ProfileRequest;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.MarkPickupPointAsDefaultRequest;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class XBPFeignFallbackTest {
  private static final String PROFILE_ID = "profile_id";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String TYPE = "type";

  private XBPFeignFallback xbpFeignFallback = new XBPFeignFallback();

  @Test
  public void getProfileDetailByIdTest() {
    GdnRestSingleResponse<ProfileResponse> response =
        xbpFeignFallback.getProfileDetailById(PROFILE_ID);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void filterTest() {
    GdnRestListResponse<PickupPointOutboundResponse> response = xbpFeignFallback.filter(0, 1, new PickupPointFilterRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void updateDefaultPickupPointCodeTest() {
    BaseResponse response = xbpFeignFallback.updateDefaultPickupPointCode(new MarkPickupPointAsDefaultRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void updateDefaultConfigurationTest() {
    GdnRestSingleResponse<ProfileResponse> response = xbpFeignFallback.updateDefaultConfiguration(TYPE, new ProfileRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void filterByBusinessPartnerCodeTest() {
    GdnRestSingleResponse<ProfileResponse> response =
        xbpFeignFallback.filterByBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void filterByCodeTest() {
    GdnRestSingleResponse response = xbpFeignFallback.filterByCode(PROFILE_ID);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void getBusinessPartnerDetailsByListTest() {
    GdnRestListResponse<ProfileResponse> response =
        xbpFeignFallback.getBusinessPartnerDetailsByList(0, 1, new BusinessPartnerFilterRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }
  }
