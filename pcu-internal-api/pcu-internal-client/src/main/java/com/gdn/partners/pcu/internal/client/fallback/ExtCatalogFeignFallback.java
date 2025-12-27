package com.gdn.partners.pcu.internal.client.fallback;

import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.ext.catalog.rest.web.model.response.PristineCategoryMapResponse;
import com.gdn.partners.pcu.internal.client.feign.ExtCatalogFeign;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.x.productcategorybase.dto.response.ProductCodeResponse;

@Component
public class ExtCatalogFeignFallback implements ExtCatalogFeign {

  @Override
  public GdnRestSingleResponse<PristineCategoryMapResponse> getSupportedBlibliCategoriesByPristine() {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestListResponse<ProductCodeResponse> getPCBProductCodes(String productCode, String category, int page, int size) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }
}
