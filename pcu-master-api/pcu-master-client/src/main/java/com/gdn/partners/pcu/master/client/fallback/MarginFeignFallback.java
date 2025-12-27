package com.gdn.partners.pcu.master.client.fallback;

import java.util.ArrayList;

import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.margin.webmodel.MarginCategoryResponse;
import com.gdn.mta.margin.webmodel.MarginOrderResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.core.web.dto.Metadata;
import com.gdn.partners.pcu.master.client.feign.MarginFeign;
import com.gdn.partners.pcu.master.client.model.FilterMarginsByOrderItemsRequest;
import com.gdn.partners.pcu.master.client.model.OrderItemMarginsResponse;
import com.gdn.partners.pcu.master.client.model.BaseMarginResponse;
import com.gdn.partners.pcu.master.client.model.CategoryCodesListRequest;
import com.gdn.partners.pcu.master.model.ErrorMessages;

@Service
public class MarginFeignFallback implements MarginFeign {
  @Override
  public GdnRestSingleResponse<MarginCategoryResponse> filterMarginCategoryByCategoryCodeAndOrderDate(String categoryCode,
      String orderDate) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public GdnRestSingleResponse<MarginOrderResponse> filterMarginBusinessPartnerByBusinessPartnerIdAndCategoryIdAndOrderDate(
      String businessPartnerId, String categoryCode, String orderDate, String gdnSku) {
    return new GdnRestSingleResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null);
  }

  @Override
  public ListBaseResponse<BaseMarginResponse> filterActiveBasicMargin(CategoryCodesListRequest request) {
    return new ListBaseResponse<> (ErrorMessages.FALLBACK_ERR_MESSAGE,
        ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false, null, null, null);
  }
  @Override
  public ListBaseResponse<OrderItemMarginsResponse> filterMargin(String storeId, String channelId, String clientId,
      String requestId, String username, FilterMarginsByOrderItemsRequest marginOrderItem) {
    return new ListBaseResponse<>(ErrorMessages.FALLBACK_ERR_MESSAGE, ErrorCategory.COMMUNICATION_FAILURE.getMessage(), false,
        requestId, new ArrayList<>(), new Metadata());
  }

}
