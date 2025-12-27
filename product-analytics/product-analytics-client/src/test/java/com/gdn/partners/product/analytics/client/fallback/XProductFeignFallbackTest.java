package com.gdn.partners.product.analytics.client.fallback;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.product.analytics.model.ErrorMessages;

@ExtendWith(MockitoExtension.class)
public class XProductFeignFallbackTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String SELLER_CODE = "sellerCode";


  XProductFeignFallback xProductFeignFallback = new XProductFeignFallback();

  @Test
  public void checkIfProductIsShared() {
    GdnBaseRestResponse response =
      xProductFeignFallback.checkIfProductIsShared(PRODUCT_CODE, SELLER_CODE);
    Assertions.assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    Assertions.assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

}
