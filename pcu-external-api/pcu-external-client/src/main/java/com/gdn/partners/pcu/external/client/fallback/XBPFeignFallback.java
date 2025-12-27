package com.gdn.partners.pcu.external.client.fallback;

import com.gdn.partners.pcu.external.client.model.PickupPointOutboundResponse;
import com.gdn.x.businesspartner.v2.dto.businesspartner.BusinessPartnerFilterRequest;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestParam;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.core.web.dto.BaseResponse;
import com.gdn.partners.pcu.external.client.feign.XBPFeign;
import com.gdn.partners.pcu.external.model.ErrorMessages;
import com.gdn.x.businesspartner.dto.PickupPointResponse;
import com.gdn.x.businesspartner.dto.ProfileRequest;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.MarkPickupPointAsDefaultRequest;
import com.gdn.x.businesspartner.v2.dto.pickuppoint.PickupPointFilterRequest;

/**
 * Created by govind on 13/12/2018 AD.
 */
@Component
public class XBPFeignFallback implements XBPFeign{

  @Override
  public GdnRestSingleResponse<ProfileResponse> getProfileDetailById(@PathVariable("id") String profileId){
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<PickupPointOutboundResponse> filter(int page, int size, PickupPointFilterRequest request) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }

  @Override
  public BaseResponse updateDefaultPickupPointCode(MarkPickupPointAsDefaultRequest request){
    return new BaseResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnRestSingleResponse<ProfileResponse> updateDefaultConfiguration(String type, ProfileRequest profileRequest) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<ProfileResponse> filterByBusinessPartnerCode(String businessPartnerCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<PickupPointOutboundResponse> filterByCode(String code) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<ProfileResponse> getBusinessPartnerDetailsByList(int page, int size,
      BusinessPartnerFilterRequest request) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }
}
