package com.gdn.x.product.outbound.impl;

import java.util.Objects;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.outbound.api.ProductAnalyticsOutbound;
import com.gdn.x.product.outbound.api.feign.ProductAnalyticsFeign;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProductAnalyticsOutboundImpl implements ProductAnalyticsOutbound {

  @Autowired
  private ProductAnalyticsFeign productAnalyticsFeign;

  @Override
  public SellerDetailResponse checkGoodSeller(String storeId, String requestId, String clientId, String channelId,
      String username, String sellerCode) {
    GdnRestSingleResponse<SellerDetailResponse> response =
        productAnalyticsFeign.findByMerchantAndCategory(storeId, channelId, clientId, requestId, username, sellerCode);
    if (!response.isSuccess() || Objects.isNull(response.getValue())) {
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, ErrorMessages.SELLER_DETAIL_FETCH_ERROR);
    }
    return response.getValue();
  }
}
