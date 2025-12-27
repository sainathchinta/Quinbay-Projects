package com.gdn.partners.pcu.internal.client.fallback;

import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.pcu.internal.client.feign.PartnersEngineFeign;
import com.gdn.partners.pcu.internal.client.model.response.UserResponse;
import com.gdn.partners.pcu.internal.client.model.request.UserFilter;
import com.gdn.partners.pcu.internal.model.ErrorMessages;

@Component
public class PartnersEngineFeignFallback implements PartnersEngineFeign {

  @Override
  public ListBaseResponse<UserResponse> filter(int page, int size, UserFilter request) {
    return new ListBaseResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        false, null, null, new Metadata());
  }
}