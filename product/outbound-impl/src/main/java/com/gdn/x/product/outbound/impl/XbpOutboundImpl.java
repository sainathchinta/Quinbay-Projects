package com.gdn.x.product.outbound.impl;

import java.util.Objects;

import com.gdn.x.businesspartner.dto.ProductCounterResponse;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.outbound.api.XbpOutbound;
import com.gdn.x.product.outbound.api.feign.XbpFeign;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class XbpOutboundImpl implements XbpOutbound {

  @Autowired
  private XbpFeign xbpFeign;

  @Override
  public ProfileResponse getBusinessPartnerDetails(String storeId, String requestId, String username,
      String businessPartnerCode) {
    GdnRestSingleResponse<ProfileResponse> response =
        xbpFeign.getBusinessPartnerDetails(storeId, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID_X_PRODUCT, requestId, username, businessPartnerCode);
    if (!response.isSuccess() || Objects.isNull((response.getValue()))) {
      log.error("Error while fetching profile response from xbp : {}, error - {}",
          response, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
          ErrorMessages.INVALID_BUSINESS_PARTNER_CODE);
    }
    return response.getValue();
  }

  @Override
  public ProductCounterResponse productCounterIncrementAndGet(String storeId, String requestId, String username, String businessPartnerCode) {
    GdnRestSingleResponse<ProductCounterResponse> response =
            xbpFeign.productCounterIncrementAndGet(storeId, Constants.DEFAULT_CHANNEL_ID,
                    Constants.DEFAULT_CLIENT_ID_X_PRODUCT, requestId, username, businessPartnerCode);
    if (!response.isSuccess() || Objects.isNull((response.getValue()))) {
      log.error("Error while incrementing product counter from xbp : {}, error - {}",
              response, response.getErrorMessage());
      throw new ApplicationRuntimeException(ErrorCategory.COMMUNICATION_FAILURE,
              ErrorMessages.INVALID_BUSINESS_PARTNER_CODE);
    }
    return response.getValue();
  }
}
