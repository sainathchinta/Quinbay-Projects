package com.gdn.partners.pcu.internal.client.fallback;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.pcu.internal.client.model.request.UserFilter;
import com.gdn.partners.pcu.internal.client.model.response.UserResponse;
import com.gdn.partners.pcu.internal.model.ErrorMessages;

public class PartnersEngineFeignFallbackTest {

  private PartnersEngineFeignFallback partnersEngineFeignFallback = new PartnersEngineFeignFallback();

  @Test
  public void filter_Valid_Success() throws Exception {
    ListBaseResponse<UserResponse> response = partnersEngineFeignFallback.filter(0, 10, new UserFilter());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }
}
