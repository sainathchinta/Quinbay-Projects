package com.gdn.partners.pcu.master.service;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.master.web.model.request.AutoQcConfigUpdateWebRequest;
import com.gdn.partners.pcu.master.web.model.response.AutoApprovalRulesListWebResponse;

public interface AutoQcConfigService {

  /**
   * to update autoApproval rules according to rule name
   *
   * @param ruleName
   * @param isNeedRevisionConfig
   * @param autoQcConfigUpdateWebRequest
   * @return
   * @throws Exception
   */
  GdnBaseRestResponse updateAutoQcConfigRule(String ruleName, boolean isNeedRevisionConfig,  AutoQcConfigUpdateWebRequest autoQcConfigUpdateWebRequest)
      throws Exception;

  /**
   * to get auto approval rules from prd_auto_approval_rules table
   *
   * @return
   */
  AutoApprovalRulesListWebResponse getAutoApprovalRules();
}
