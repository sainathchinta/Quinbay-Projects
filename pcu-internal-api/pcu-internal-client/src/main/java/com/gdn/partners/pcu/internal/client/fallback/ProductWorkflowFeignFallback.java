package com.gdn.partners.pcu.internal.client.fallback;

import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.RequestBody;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pbp.dto.workflow.product.ProductReturnForCorrectionRequest;
import com.gdn.partners.pcu.internal.client.feign.ProductWorkflowFeign;
import com.gdn.partners.pcu.internal.model.ErrorMessages;

/**
 * Created by govind on 11/01/2019 AD.
 */
@Component
public class ProductWorkflowFeignFallback implements ProductWorkflowFeign{

  @Override
  public GdnBaseRestResponse returnForCorrection(
      @RequestBody ProductReturnForCorrectionRequest request) {
    return new GdnBaseRestResponse(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null);
  }
}
