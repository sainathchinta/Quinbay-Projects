package com.gdn.partners.pcu.internal.client.fallback;


import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.ext.catalog.rest.web.model.response.PristineCategoryMapResponse;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

public class ExtCatalogFeignFallbackTest {

  private static final String PRODUCT_CODE = "productCode";
  private static final String CATEGORY = "category";
  private static final int PAGE = 0;
  private static final int SIZE = 0;

  private ExtCatalogFeignFallback extCatalogFeignFallback= new ExtCatalogFeignFallback();

  @Test
  public void getSupportedBlibliCategoriesByPristineTest() {
    GdnRestSingleResponse<PristineCategoryMapResponse> response =
        extCatalogFeignFallback.getSupportedBlibliCategoriesByPristine();
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }

  @Test
  public void getPCBProductCodesTest() {
    GdnRestListResponse<ProductCodeResponse> response =
        extCatalogFeignFallback.getPCBProductCodes(PRODUCT_CODE, CATEGORY, PAGE, SIZE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertFalse(response.isSuccess());
  }
}