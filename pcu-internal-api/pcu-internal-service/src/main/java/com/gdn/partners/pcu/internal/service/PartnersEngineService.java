package com.gdn.partners.pcu.internal.service;

import java.util.List;
import java.util.Map;

public interface PartnersEngineService {

  /**
   * Retrieves the image and content reviewers.
   *
   * @return
   */
  Map<String, List<String>> getReviewers() throws Exception;

  /**
   * Retrieves the master sku reviewers
   *
   * @return Master sku reviewers
   */
  List<String> getMasterSkuReviewers() throws Exception;

  /**
   * get IPR reviewers by role code
   *
   * @param roleCode
   * @return
   * @throws Exception
   */
  List<String> getIPRReviewersByRoleCode(String roleCode) throws Exception;
}
