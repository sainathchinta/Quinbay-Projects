package com.gdn.partners.pcu.master.client.fallback;

import org.springframework.stereotype.Component;

import com.gda.mta.product.dto.AutoQcConfigRequest;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gda.mta.product.dto.response.AutoApprovalRulesListResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.master.client.feign.PBPFeign;
import com.gdn.partners.pcu.master.model.ErrorMessages;

@Component
public class PBPFeignFallback implements PBPFeign {

  @Override
  public GdnBaseRestResponse update(boolean isNeedRevisionConfig, String ruleName, AutoQcConfigRequest autoQcConfigRequest) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        com.gdn.common.enums.ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }

  @Override
  public GdnRestSimpleResponse<AutoApprovalRulesListResponse> getAutoApprovalRules() {
    return new GdnRestSimpleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }
}
