package com.gdn.partners.product.analytics.web.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.product.analytics.model.SellerApiPath;
import com.gdn.partners.product.analytics.model.enums.ErrorCode;
import com.gdn.partners.product.analytics.service.SellerDetailService;
import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;
import io.swagger.v3.oas.annotations.Operation;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name = "Seller Detail Controller", description = "Seller Detail Controller")
@RestController
@RequestMapping(value = SellerApiPath.BASE_PATH)
@Validated
public class SellerDetailController {

  @Autowired
  private SellerDetailService sellerDetailService;

  @Operation(summary = "Get Seller Detail by Seller code")
  @GetMapping(value = SellerApiPath.FIND_BY_SELLER_CODE, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnRestSingleResponse<SellerDetailResponse> findByMerchantAndCategory(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String merchantCode) throws Exception {
    log.info("Find seller detail by merchantCode {}", merchantCode);
    try {
      SellerDetailResponse sellerDetailResponse = sellerDetailService.findSellerDetailByMerchantCode(merchantCode);
      return new GdnRestSingleResponse<>(sellerDetailResponse, requestId);
    } catch (Exception ex) {
      log.error("Find seller detail error with merchantCode {} ", merchantCode, ex);
      return new GdnRestSingleResponse<>(ex.getMessage(), ErrorCode.FIND_SELLER_DETAIL.getCode(), false, null,
          requestId);
    }
  }
}
