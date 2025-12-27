package com.gdn.partners.pcu.internal.client.fallback;

import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.internal.client.feign.XGPFeign;
import com.gdn.partners.pcu.internal.model.ErrorMessages;

@Component
public class XGPFeignFallback implements XGPFeign {

  @Override
  public GdnRestSimpleResponse<String> checkImageSizeByImageFilename(String imageFilename) {
    return new GdnRestSimpleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }
}
