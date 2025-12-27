package com.gdn.partners.pcu.internal.client.fallback;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

import org.junit.jupiter.api.Test;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.mta.product.util.GdnRestSimpleResponse;
import com.gdn.partners.pcu.internal.model.ErrorMessages;

public class XGPFeignFallbackTest {

  private static final String FILE_NAME = "/MTA-10001.jpg";

  private XGPFeignFallback xgpFeignFallback= new XGPFeignFallback();

  @Test
  public void checkImageSizeByImageFilenameTest(){
    GdnRestSimpleResponse<String> response =
        xgpFeignFallback.checkImageSizeByImageFilename(FILE_NAME);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

}