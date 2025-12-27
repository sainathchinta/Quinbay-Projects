package com.gdn.partners.pcu.internal.service;

import java.util.List;
import java.util.Map;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.mta.distributiontask.rest.model.response.DistributionProductDetailResponse;

public interface BPService {

  /**
   * Get profile response map bu business partner code
   *
   * @param businessPartnerCodes
   *
   * @return
   */
  Map<String, ProfileResponse> getProfileResponseMap(List<String> businessPartnerCodes);


  /**
   *
   * @param businessPartnerCode
   * @return
   */
  ProfileResponse getProfileResponseByBusinessPartnerCode(String businessPartnerCode);
}
