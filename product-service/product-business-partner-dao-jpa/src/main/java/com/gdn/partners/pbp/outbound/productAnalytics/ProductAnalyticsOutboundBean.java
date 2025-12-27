package com.gdn.partners.pbp.outbound.productAnalytics;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.productAnalytics.feign.ProductAnalyticsFeign;
import com.gdn.partners.product.analytics.web.model.AutoQCDetailResponse;
import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;
import com.gdn.x.product.rest.web.model.response.ProductResponse;
import lombok.extern.slf4j.Slf4j;

@Component
@Slf4j
public class ProductAnalyticsOutboundBean implements ProductAnalyticsOutbound{

  @Autowired
  private ProductAnalyticsFeign productAnalyticsFeign;

  @Override
  public AutoQCDetailResponse getAutoQCDetails(String merchantCode, String categoryCode)
      throws Exception {
    GdnRestSingleResponse<AutoQCDetailResponse> response =
        productAnalyticsFeign.getAutoQCDetails(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), merchantCode, categoryCode);
    if (!response.isSuccess()) {
      log.error("Failed to get response from Product Analytics  : {}", response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }

  @Override
  public SellerDetailResponse getSellerDetail(String merchantCode) {
    GdnRestSingleResponse<SellerDetailResponse> response = productAnalyticsFeign
        .getSellerDetail(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), merchantCode);
    if (!response.isSuccess()) {
      log.error("Failed to get response from Product Analytics for seller : {} and : {}", merchantCode,
          response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED, response.getErrorMessage());
    }
    return response.getValue();
  }
}
