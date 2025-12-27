package com.gdn.partners.pcu.master.client.fallback;


import com.gdn.mta.margin.webmodel.MarginOrderResponse;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.margin.webmodel.MarginCategoryResponse;
import com.gdn.partners.core.web.dto.ListBaseResponse;
import com.gdn.partners.pcu.master.client.feign.MarginFeign;
import com.gdn.partners.pcu.master.client.model.BaseMarginResponse;
import com.gdn.partners.pcu.master.client.model.CategoryCodesListRequest;
import com.gdn.partners.pcu.master.client.model.FilterMarginsByOrderItemsRequest;
import com.gdn.partners.pcu.master.client.model.OrderItemMarginsResponse;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

public class MarginFeignFallbackTest {

  private MarginFeign marginFeign = new MarginFeignFallback();

  private static final String CATEGORY_ID = "category_id";
  private static final String DATE = "date";
  private static final String BUSINESS_PARTNER_ID = "businessPartnerId";
  private static final String GDN_SKU = "gdnSku";

  @Test
  void filterMarginCategoryByCategoryIdAndOrderDateTest() {
    GdnRestSingleResponse<MarginCategoryResponse> response =
        marginFeign.filterMarginCategoryByCategoryCodeAndOrderDate(CATEGORY_ID, DATE);

    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  void filterMarginBusinessPartnerByBusinessPartnerIdAndCategoryIdAndOrderDateTest() {
    GdnRestSingleResponse<MarginOrderResponse> response =
            marginFeign.filterMarginBusinessPartnerByBusinessPartnerIdAndCategoryIdAndOrderDate(BUSINESS_PARTNER_ID,CATEGORY_ID, DATE,GDN_SKU);

    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  void filterActiveBasicMarginTest() {
    ListBaseResponse<BaseMarginResponse> response =
        marginFeign.filterActiveBasicMargin(new CategoryCodesListRequest());

    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  void filterMarginTest() {
    ListBaseResponse<OrderItemMarginsResponse> response =
        marginFeign.filterMargin("STOREID","CHANNELID","CLIENTID","REQUESTID","USERNAME",new FilterMarginsByOrderItemsRequest());

    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

}