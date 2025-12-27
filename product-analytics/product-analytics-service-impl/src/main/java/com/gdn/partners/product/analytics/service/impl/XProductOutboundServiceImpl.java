package com.gdn.partners.product.analytics.service.impl;

import java.util.Objects;

import org.springframework.stereotype.Service;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.product.analytics.client.XProduct.XProductOutbound;
import com.gdn.partners.product.analytics.client.XProduct.feign.XProductFeign;
import com.gdn.partners.product.analytics.client.response.SimpleBooleanResponse;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
@RequiredArgsConstructor
public class XProductOutboundServiceImpl implements XProductOutbound {

  private final XProductFeign xProductFeign;

  @Override
  public boolean isSharedProduct(String sellerCode, String productCode) {
    GdnRestSingleResponse<SimpleBooleanResponse> response =
      xProductFeign.checkIfProductIsShared(sellerCode, productCode);
    return validateResponse(response);
  }

  private static boolean validateResponse(GdnRestSingleResponse<SimpleBooleanResponse> response) {
    if (!response.isSuccess() || response.getValue() == null || Objects.isNull(
      response.getValue().getResult())) {
      log.error("Failed to validate response from XProduct service: {}", response);
      return true;
    }
    return response.getValue().getResult();
  }
}
