package com.gdn.partners.pbp.outbound.sap;

import com.gda.mta.product.dto.response.CogsValueResponse;

public interface SapOutbound {

  /**
   * Get material code from SAP
   *
   * @param materialCode
   * @return
   */
  CogsValueResponse getCogsValueResponse(String materialCode);
}
