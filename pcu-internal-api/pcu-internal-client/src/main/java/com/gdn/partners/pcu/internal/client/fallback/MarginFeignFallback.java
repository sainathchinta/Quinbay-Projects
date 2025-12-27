package com.gdn.partners.pcu.internal.client.fallback;

import java.util.ArrayList;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.margin.webmodel.MarginCategoryResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.pcu.internal.client.feign.MarginFeign;
import com.gdn.partners.pcu.internal.client.model.request.FilterMarginsByOrderItemsRequest;
import com.gdn.partners.pcu.internal.client.model.response.OrderItemMarginsResponse;
import com.gdn.partners.pcu.internal.model.ErrorMessages;
import org.springframework.stereotype.Component;

@Component
public class MarginFeignFallback implements MarginFeign {

  @Override
  public GdnRestSingleResponse<MarginCategoryResponse> filterMarginCategoryByCategoryCodeAndOrderDate(
      String categoryCode, String orderDate) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public ListBaseResponse<OrderItemMarginsResponse> filterMargin(String storeId, String channelId, String clientId,
      String requestId, String username, FilterMarginsByOrderItemsRequest marginOrderItem) {
    return new ListBaseResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false,
        requestId, new ArrayList<>(), new Metadata());
  }
}
