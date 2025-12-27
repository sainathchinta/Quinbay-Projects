package com.gdn.partners.pcu.internal.client.fallback;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedAssigneeRequest;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedRequest;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedSelectedDownloadRequest;
import com.gdn.partners.pcu.internal.client.model.request.AutoApprovedUserFeedbackRequest;
import com.gdn.partners.pcu.internal.client.model.request.ProductAttributeExtractionsRequest;
import com.gdn.partners.pcu.internal.client.model.response.AutoApprovedListWebResponse;
import com.gdn.partners.pcu.internal.client.model.response.AutoApprovedUserFeedbackResponse;
import com.gdn.partners.pcu.internal.client.model.response.ProductAssigneeChangeResponse;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import com.gdn.x.campaign.clientsdk.shade.com.gdn.common.enums.ErrorCategory;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

public class ProductAnalyticsFeignFallbackTest {
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String PRODUCT_CODE = "product-code";


  private final ProductAnalyticsFeignFallback feignFallback = new ProductAnalyticsFeignFallback();

  @Test
  public void fetchAutoApprovedProductsTest() {
    AutoApprovedRequest request = new AutoApprovedRequest();
    GdnRestListResponse<AutoApprovedListWebResponse> response =
      feignFallback.fetchAutoApprovedProductsList(PAGE, SIZE, request);

    assertNotNull(response);
    assertFalse(response.isSuccess());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertNull(response.getContent());
    assertNull(response.getRequestId());
  }

  @Test
  public void updateAssignedToTest() {
    AutoApprovedAssigneeRequest request = new AutoApprovedAssigneeRequest();
    GdnRestListResponse<ProductAssigneeChangeResponse> response = feignFallback.updateAssignedTo(request);
    assertNotNull(response);
    assertFalse(response.isSuccess());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertNull(response.getRequestId());
  }

  @Test
  public void updateUserFeedbackTest() {
    AutoApprovedUserFeedbackRequest request = new AutoApprovedUserFeedbackRequest();
    GdnBaseRestResponse response = feignFallback.updateUserFeedback(PRODUCT_CODE, request);
    assertNotNull(response);
    assertFalse(response.isSuccess());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertNull(response.getRequestId());
  }

  @Test
  public void fetchUserFeedbackTest() {
    GdnRestSingleResponse<AutoApprovedUserFeedbackResponse> response = feignFallback.fetchUserFeedback(PRODUCT_CODE);
    assertNotNull(response);
    assertFalse(response.isSuccess());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertNull(response.getRequestId());
  }

  @Test
  public void fetchAutoApprovedSelectedProductsList() {
    AutoApprovedSelectedDownloadRequest request = new AutoApprovedSelectedDownloadRequest();
    GdnRestListResponse<AutoApprovedListWebResponse> response =
        feignFallback.fetchAutoApprovedSelectedProductsList(request);
    assertNotNull(response);
    assertFalse(response.isSuccess());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertNull(response.getContent());
    assertNull(response.getRequestId());
  }

  @Test
  public void publishEventsByProductSkuTest() {
    String storeId = "store-id";
    ProductAttributeExtractionsRequest request = new ProductAttributeExtractionsRequest();
    GdnBaseRestResponse response = feignFallback.publishEventsByProductSku(storeId, request);
    assertNotNull(response);
    assertFalse(response.isSuccess());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, response.getErrorMessage());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
    assertNull(response.getRequestId());
  }
}
