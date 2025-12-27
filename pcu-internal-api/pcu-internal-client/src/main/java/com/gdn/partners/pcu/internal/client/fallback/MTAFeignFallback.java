package com.gdn.partners.pcu.internal.client.fallback;

import java.util.List;
import java.util.Map;

import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.internal.client.feign.MTAFeign;
import com.gdn.partners.pcu.internal.model.ErrorMessages;

@Component
public class MTAFeignFallback implements MTAFeign {

  @Override
  public GdnRestSimpleResponse<Map<String, List<String>>> getImageOrContentReviewers(String reviewerType) {
    return new GdnRestSimpleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }
}