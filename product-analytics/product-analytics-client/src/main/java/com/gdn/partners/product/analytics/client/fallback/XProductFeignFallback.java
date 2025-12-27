package com.gdn.partners.product.analytics.client.fallback;

import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.product.analytics.client.XProduct.feign.XProductFeign;
import com.gdn.partners.product.analytics.client.response.SimpleBooleanResponse;
import com.gdn.partners.product.analytics.model.ErrorMessages;

@Component
public class XProductFeignFallback implements XProductFeign {

  @Override
  public GdnRestSingleResponse<SimpleBooleanResponse> checkIfProductIsShared(String sellerCode,
    String productCode) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }
}
