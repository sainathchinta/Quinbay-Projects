package com.gdn.partners.pcu.internal.service;

import java.util.List;

import com.gdn.partners.pcu.internal.web.model.response.LookupWebResponse;


public interface LookupService {

  /**
   * API to fetch dangerous goods level data
   *
   * @return
   */
  List<LookupWebResponse> getDangerousGoodsLevel();

}
