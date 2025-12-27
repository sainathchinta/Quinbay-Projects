package com.gdn.partners.pbp.outbound.pickuppoint;

import java.util.List;

import com.gdn.x.businesspartner.dto.ExternalPickupPointCodeResponseDetail;
import com.gdn.x.businesspartner.dto.PickupPointCodeResponse;
import com.gdn.x.businesspartner.dto.PickupPointResponse;


public interface PickupPointOutbound {


  /**
   * to check external pickup point availability
   *
   * @param requestId
   * @param username
   * @param merchantCode
   * @param externalPickupPointCodes
   * @return
   * @throws Exception
   */
  List<ExternalPickupPointCodeResponseDetail> checkExternalPickupPointCodeAvailability(
      String requestId, String username, String merchantCode, List<String> externalPickupPointCodes)
      throws Exception;

  /**
   * Get details by pickup point codes
   *
   * @param requestId
   * @param pickupPointCodes
   * @return
   * @throws Exception
   */
  List<PickupPointResponse> getByPickupPointCodes(String requestId,
    List<String> pickupPointCodes) throws Exception;
}
