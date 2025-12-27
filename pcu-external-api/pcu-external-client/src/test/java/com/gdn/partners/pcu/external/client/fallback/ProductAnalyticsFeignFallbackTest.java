package com.gdn.partners.pcu.external.client.fallback;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationFeedbackRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationListRequest;
import com.gdn.partners.pcu.external.web.model.request.ProductOptimisationUpdateStatusRequest;
import com.gdn.partners.pcu.external.web.model.response.ProductCountsWebResponse;
import com.gdn.partners.pcu.external.web.model.response.ProductOptimisationListResponse;
import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

public class ProductAnalyticsFeignFallbackTest {

  private final ProductAnalyticsFeignFallback productAnalyticsFeignFallback =
      new ProductAnalyticsFeignFallback();
  private static final String REQUEST_ID = "requestId";
  private static final String STORE_ID = "requestId";
  private static final String SELLER_CODE = "sellerCode";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String PRODUCT_SKU = "PRODUCT_SKU";
  private static final String STATUS = "STATUS";

  @Test
  public void getProductCountTest() {
    GdnRestSingleResponse<ProductCountsWebResponse> response =
        productAnalyticsFeignFallback.getProductCount(STORE_ID, REQUEST_ID, SELLER_CODE);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void filterTest() {
    GdnRestListResponse<ProductOptimisationListResponse> response =
        productAnalyticsFeignFallback.filter(PAGE, SIZE, new ProductOptimisationListRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void feedbackTest() {
    GdnBaseRestResponse response = productAnalyticsFeignFallback.provideSuggestionFeedback(
        new ProductOptimisationFeedbackRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getSuggestionDetailsTest() {
    GdnBaseRestResponse response = productAnalyticsFeignFallback.getSuggestionDetail(PRODUCT_SKU);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void updateStatusTest() {
    GdnBaseRestResponse response =
        productAnalyticsFeignFallback.updateStatus(REQUEST_ID, new ProductOptimisationUpdateStatusRequest());
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }
}
