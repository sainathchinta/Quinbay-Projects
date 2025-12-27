package com.gdn.partners.pcu.internal.client.fallback;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pbp.dto.workflow.product.ProductReturnForCorrectionRequest;
import com.gdn.partners.pcu.internal.model.ErrorMessages;

/**
 * Created by govind on 14/01/2019 AD.
 */
public class ProductWorkflowFeignFallbackTest {

  private ProductWorkflowFeignFallback
      productWorkflowFeignFallback = new ProductWorkflowFeignFallback();

  @Test
  public void returnForCorrectionTest() {
    GdnBaseRestResponse
        response = productWorkflowFeignFallback.returnForCorrection(new ProductReturnForCorrectionRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }
}
