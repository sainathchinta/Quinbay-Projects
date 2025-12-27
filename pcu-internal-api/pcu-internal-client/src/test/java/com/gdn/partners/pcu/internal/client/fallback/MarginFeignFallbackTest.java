package com.gdn.partners.pcu.internal.client.fallback;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.margin.webmodel.MarginCategoryResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.pcu.internal.client.model.request.FilterMarginsByOrderItemsRequest;
import com.gdn.partners.pcu.internal.client.model.response.OrderItemMarginsResponse;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

public class MarginFeignFallbackTest {

  private static final String CATEGORY_CODE = "CATEGORY_CODE";
  private static final String DATE = "2021-01-01 00:00:00";
  private MarginFeignFallback marginFeignFallback = new MarginFeignFallback();

  @Test
  public void filterMarginCategoryByCategoryCodeAndOrderDateTest() {
    GdnRestSingleResponse<MarginCategoryResponse> marginCategoryResponse =
        marginFeignFallback.filterMarginCategoryByCategoryCodeAndOrderDate(CATEGORY_CODE, DATE);
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(),
        marginCategoryResponse.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, marginCategoryResponse.getErrorMessage());
    assertFalse(marginCategoryResponse.isSuccess());
  }

  @Test
  public void marginFilterTest() {
    ListBaseResponse<OrderItemMarginsResponse> marginCategoryResponse =
        marginFeignFallback.filterMargin("storeid", "channalid", "clientid", "requestId", "username",
            new FilterMarginsByOrderItemsRequest());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), marginCategoryResponse.getErrorCode());
    assertEquals(ErrorMessages.FALLBACK_ERR_MESSAGE, marginCategoryResponse.getErrorMessage());
    assertFalse(marginCategoryResponse.isSuccess());
  }
}
