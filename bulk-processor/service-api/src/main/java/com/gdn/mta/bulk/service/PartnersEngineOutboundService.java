package com.gdn.mta.bulk.service;


import com.gdn.mta.bulk.dto.product.UserResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;

import java.util.Set;

public interface PartnersEngineOutboundService {

  /**
   * Retrieve users based on their respective role codes
   *
   * @param roleCode
   * @param sortedBy
   * @param sortDirection
   * @param page
   * @param roleCodes
   * @return
   */
  ListBaseResponse<UserResponse> userFilter(String roleCode, String sortedBy, String sortDirection,
      int page, Set<String> roleCodes);

}