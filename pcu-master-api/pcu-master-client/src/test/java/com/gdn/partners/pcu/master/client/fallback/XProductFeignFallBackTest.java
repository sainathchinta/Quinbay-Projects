package com.gdn.partners.pcu.master.client.fallback;


import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.partners.pcu.master.client.feign.XProductFeign;
import com.gdn.partners.pcu.master.model.Constants;
import com.gdn.partners.pcu.master.model.ErrorMessages;
import com.gdn.x.product.model.vo.ProductSkuSizeChartResponse;
import com.gdn.x.product.rest.web.model.response.ProductSummaryResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class XProductFeignFallBackTest {

  private static final String DEFAULT_CATEGORY_CODE = "CGC-001";
  private static final String DEFAULT_SALES_CATALOG_CODE = "defaultSalesCatalogCode";
  private static final String SIZE_CHART_CODE = "sizeChartCode";

  private XProductFeign xProductFeign = new XProductFeignFallBack();
  private XProductFeignFallBack feignFallback = new XProductFeignFallBack();

  @Test
  void getListOfProductSummaryBySalesCatalog() {
    GdnRestListResponse<ProductSummaryResponse> response = feignFallback.getListOfProductSummaryBySalesCatalog(
        DEFAULT_SALES_CATALOG_CODE, DEFAULT_CATEGORY_CODE, 0, 1 );
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

  @Test
  public void checkAnyProductsMappedToSizeChartTest() {
    GdnRestListResponse<ProductSkuSizeChartResponse> response =
        feignFallback.checkAnyProductsMappedToSizeChart(Constants.INT_ZERO, Constants.ONE,
            SIZE_CHART_CODE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    Assertions.assertFalse(response.isSuccess());
  }

}
