package com.gdn.partners.product.analytics.web.controller;

import io.swagger.v3.oas.annotations.tags.Tag;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.product.analytics.model.AutoQCApiPath;
import com.gdn.partners.product.analytics.model.enums.ErrorCode;
import com.gdn.partners.product.analytics.service.AutoQCDetailService;
import com.gdn.partners.product.analytics.web.model.AutoQCDetailResponse;

import io.swagger.v3.oas.annotations.Operation;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name = "Auto QC API", description = "Auto QC API")
@RestController
@RequestMapping(value = AutoQCApiPath.BASE_PATH)
@Validated
public class AutoQCDetailController {

  @Autowired
  private AutoQCDetailService autoQCDetailService;

  private static final Logger LOG = LoggerFactory.getLogger(AutoQCDetailController.class);


  @Operation(summary = "Get AutoQC Detail by merchant and category")
  @GetMapping(value = AutoQCApiPath.FIND_BY_MERCHANT_CATEGORY, produces = MediaType.APPLICATION_JSON_VALUE)
  @ResponseBody
  public GdnRestSingleResponse<AutoQCDetailResponse> findByMerchantAndCategory(@RequestParam String storeId,
      @RequestParam String channelId, @RequestParam String clientId, @RequestParam String requestId,
      @RequestParam(required = false) String username, @RequestParam String merchantCode,
      @RequestParam String categoryCode) throws Exception {
    LOG.info("#findByMerchantAndCategory with merchant {} and category {}", merchantCode, categoryCode);
    try {
      AutoQCDetailResponse autoQCDetailResponse =
          autoQCDetailService.findByMerchantCodeAndCategoryCode(merchantCode, categoryCode);
      return new GdnRestSingleResponse<>(autoQCDetailResponse, requestId);
    } catch (Exception ex) {
      LOG.error("#findByMerchantAndCategory error with merchant {} and category {}", merchantCode, categoryCode, ex);
      return new GdnRestSingleResponse<>(ex.getMessage(), ErrorCode.FIND_AUTO_QC.getCode(), false, null, requestId);
    }
  }
}
