package com.gdn.partners.pcu.master.client.fallback;

import java.util.UUID;


import com.gda.mta.product.dto.AutoQcConfigRequest;
import com.gda.mta.product.dto.response.AutoApprovalRulesListResponse;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.master.client.feign.PBPFeign;
import com.gdn.partners.pcu.master.model.ErrorMessages;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

public class PBPFeignFallbackTest {

  private static final String DEFAULT_RULE_NAME = "OFFICIAL_SELLERS";

  private PBPFeignFallback pbpFeignFallback = new PBPFeignFallback();
  private PBPFeign pbpFeign = new PBPFeignFallback();

  private AutoQcConfigRequest autoQcConfigRequest;

  @Test
  void updateTest() throws Exception {
    GdnBaseRestResponse gdnBaseRestResponse = pbpFeignFallback.update(false, DEFAULT_RULE_NAME, autoQcConfigRequest);
    Assertions.assertFalse(gdnBaseRestResponse.isSuccess());
    Assertions.assertEquals(gdnBaseRestResponse.getErrorMessage(), ErrorMessages.FALLBACK_ERR_MESSAGE);
  }

  @Test
  void getAutoApprovalRulesTest() {
    GdnRestSimpleResponse<AutoApprovalRulesListResponse> response = pbpFeign.getAutoApprovalRules();
    Assertions.assertFalse(response.isSuccess());
    Assertions.assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    Assertions.assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());

  }
}
