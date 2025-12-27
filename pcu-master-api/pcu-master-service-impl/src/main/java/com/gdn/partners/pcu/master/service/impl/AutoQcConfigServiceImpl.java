package com.gdn.partners.pcu.master.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gda.mta.product.dto.response.AutoApprovalRulesListResponse;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.core.security.Credential;
import com.gdn.partners.pcu.master.client.feign.PBPFeign;
import com.gdn.partners.pcu.master.service.AutoQcConfigService;
import com.gdn.partners.pcu.master.service.impl.helper.RequestHelper;
import com.gdn.partners.pcu.master.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.master.web.model.request.AutoQcConfigUpdateWebRequest;
import com.gdn.partners.pcu.master.web.model.response.AutoApprovalRulesListWebResponse;

@Service
public class AutoQcConfigServiceImpl implements AutoQcConfigService {

  @Autowired
  private PBPFeign pbpFeign;

  @Override
  public GdnBaseRestResponse updateAutoQcConfigRule(String ruleName, boolean isNeedRevisionConfig,
      AutoQcConfigUpdateWebRequest autoQcConfigUpdateWebRequest) throws Exception {
    RequestHelper.checkAutoQcConfigUpdateAccessibility(Credential.getAccessibilities());
    GdnBaseRestResponse gdnBaseRestResponse = this.pbpFeign.update(isNeedRevisionConfig, ruleName,
        RequestHelper.convertToAutoQcConfigRequest(autoQcConfigUpdateWebRequest));
    ResponseHelper.validateResponse(gdnBaseRestResponse);
    return gdnBaseRestResponse;
  }

  @Override
  public AutoApprovalRulesListWebResponse getAutoApprovalRules() {
    GdnRestSimpleResponse<AutoApprovalRulesListResponse> response = pbpFeign.getAutoApprovalRules();
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toAutoApprovalRulesListWebResponse(response.getValue());
  }
}
