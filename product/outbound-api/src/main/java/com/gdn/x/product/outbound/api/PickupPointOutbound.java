package com.gdn.x.product.outbound.api;

import com.gdn.x.businesspartner.dto.ExternalPickupPointCodeResponse;

import java.util.List;

public interface PickupPointOutbound {

  ExternalPickupPointCodeResponse filterPickupPointByExternalCodes(
      String requestId, String username, String businessPartnerCode, List<String> externalPickupPointCodes);
}

