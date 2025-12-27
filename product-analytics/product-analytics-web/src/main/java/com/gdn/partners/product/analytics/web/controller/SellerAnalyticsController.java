package com.gdn.partners.product.analytics.web.controller;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.product.analytics.model.SellerAnalyticsApiPath;
import com.gdn.partners.product.analytics.service.SellerAnalyticsService;
import com.gdn.partners.product.analytics.web.model.SellerAnalyticsResponse;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@Slf4j
@RestController
@Tag(name = "Seller Analytics Controller", description = "Seller Analytics Controller")
@RequestMapping(value = SellerAnalyticsApiPath.BASE_PATH)
public class SellerAnalyticsController {

  @Autowired
  private SellerAnalyticsService sellerAnalyticsService;

  @Operation(summary = "Get Seller Analytics Detail")
  @GetMapping(value = SellerAnalyticsApiPath.GET_SELLER_ANALYTICS_DETAIL, produces =
      MediaType.APPLICATION_JSON_VALUE)
  public GdnRestSingleResponse<SellerAnalyticsResponse> getSellerAnalyticsDetails(
      @RequestParam String storeId, @RequestParam String channelId, @RequestParam String clientId,
      @RequestParam String requestId, @RequestParam(required = false) String username,
      @PathVariable("sellerCode") String sellerCode) {
    log.info("Get Seller Analytics Detail for sellerCode = {} ", sellerCode);
    try {
      return new GdnRestSingleResponse<>(
          sellerAnalyticsService.findSellerAnalyticsDetailByStoreIdAndSellerCode(storeId, sellerCode),
          requestId);
    } catch (ApplicationRuntimeException e) {
      return new GdnRestSingleResponse<>(e.getMessage(), ErrorCategory.UNSPECIFIED.getCode(), false,
          null, requestId);
    }
  }
}
