package com.gdn.partners.pcu.external.service;

import com.gdn.x.bpservice.dto.request.BusinessPartnerHistoryRequest;

/**
 * Created by govind on 12/12/2018 AD.
 */
public interface BPService {

  /**
   * Save BusinessPartnerHistoryRequest
   * @param request
   */
  void saveBusinessPartnerHistory(BusinessPartnerHistoryRequest request);

}
