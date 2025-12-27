package com.gdn.partners.pcu.internal.client.fallback;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import java.util.List;
import java.util.Map;

import com.mongodb.assertions.Assertions;
import org.junit.jupiter.api.Test;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.internal.model.ErrorMessages;

public class MTAFeignFallbackTest {

  private static final String TYPE = "type";

  private MTAFeignFallback mtaFeignFallback = new MTAFeignFallback();

  @Test
  public void getImageOrContentReviewers() {
    GdnRestSimpleResponse<Map<String, List<String>>> response =
        mtaFeignFallback.getImageOrContentReviewers(TYPE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }
}