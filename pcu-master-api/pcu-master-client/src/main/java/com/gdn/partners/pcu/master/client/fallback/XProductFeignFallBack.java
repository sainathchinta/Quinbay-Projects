package com.gdn.partners.pcu.master.client.fallback;

import com.gdn.x.product.model.vo.ProductSkuSizeChartResponse;
import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.master.client.feign.XProductFeign;
import com.gdn.partners.pcu.master.model.ErrorMessages;
import com.gdn.x.product.rest.web.model.response.ProductSummaryResponse;

@Component
public class XProductFeignFallBack implements XProductFeign {

  private static PageMetaData pageMetaData = new PageMetaData(0, 0, 0);

  @Override
  public GdnRestListResponse<ProductSummaryResponse> getListOfProductSummaryBySalesCatalog(String catalogCode,
      String categoryCode, int page, int size) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }

  @Override
  public GdnRestListResponse<ProductSkuSizeChartResponse> checkAnyProductsMappedToSizeChart(
      int page, int size, String sizeChartCode) {
    return new GdnRestListResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, pageMetaData, null);
  }
}
