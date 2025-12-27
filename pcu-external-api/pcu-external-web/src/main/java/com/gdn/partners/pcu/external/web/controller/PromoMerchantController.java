package com.gdn.partners.pcu.external.web.controller;

import org.hibernate.validator.constraints.NotBlank;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.validation.annotation.Validated;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.gdn.partners.core.web.dto.SingleBaseResponse;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.PromoMerchantApiPath;
import com.gdn.partners.pcu.external.service.ProductPricingService;
import com.gdn.partners.pcu.external.web.model.response.PromoItemDetailWebResponse;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@Tag(name ="Promo Merchant API")
@RestController
@RequestMapping(value = PromoMerchantApiPath.BASE_PATH)
@Validated
public class PromoMerchantController {

  @Autowired
  private ProductPricingService productPricingService;

  @Autowired
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Operation(summary ="Promo discount Detail API")
  @GetMapping(value = PromoMerchantApiPath.SKU_DETAIL, produces = MediaType.APPLICATION_JSON_VALUE)
  public SingleBaseResponse<PromoItemDetailWebResponse> promoDiscountSkuDetail(
      @PathVariable("itemSku") @NotBlank String itemSku) {
    String requestId = mandatoryParameterHelper.getRequestId();
    String storeId = mandatoryParameterHelper.getStoreId();
    log.info("Method : Get promo discount sku detail : {}", itemSku);
    PromoItemDetailWebResponse response = productPricingService.getPromoDiscountSkuDetail(storeId, requestId, itemSku);
    return new SingleBaseResponse<>(null, null, true, requestId, response);
  }
}
