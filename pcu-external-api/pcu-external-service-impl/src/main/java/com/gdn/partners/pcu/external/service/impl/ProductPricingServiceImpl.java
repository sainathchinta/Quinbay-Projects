package com.gdn.partners.pcu.external.service.impl;

import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.client.feign.ProductPricingFeign;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.service.ProductPricingService;
import com.gdn.partners.pcu.external.service.impl.helper.ResponseHelper;
import com.gdn.partners.pcu.external.web.model.response.PromoItemDetailWebResponse;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceSkuDetailListRequest;
import com.gdn.partners.product.pricing.web.model.response.PromoSkuDetailResponse;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;

import lombok.extern.slf4j.Slf4j;

@Service
@Slf4j
public class ProductPricingServiceImpl implements ProductPricingService {

  @Value("${multipickuppoint.workflow.enabled:false}")
  private boolean multiPickupPointEnabled;

  @Value("${pricing.multipickuppoint.workflow.enabled}")
  private boolean pricingMultiPickupPointEnabled;

  @Autowired
  private ProductPricingFeign productPricingFeign;

  @Override
  public PromoItemDetailWebResponse getPromoDiscountSkuDetail(String storeId, String requestId, String itemSku) {
    log.info("Fetching promo discount sku detail for item sku : {}", itemSku);
    GdnRestSingleResponse<PromoSkuDetailResponse> response = null;
    if (multiPickupPointEnabled) {
      response = productPricingFeign.getPromoSkuDetailV2(storeId, requestId, itemSku);
    } else {
      response = productPricingFeign.getPromoSkuDetail(storeId, requestId, itemSku);
    }
    ResponseHelper.validateResponse(response);
    return ResponseHelper.toPromoItemDetailWebResponse(response.getValue());
  }

  @Override
  public Map<String, WholesalePriceSkuResponse> getItemWholesaleConfig(String storeId, String requestId,
      WholesalePriceSkuDetailListRequest itemSkus) {
    log.info("Fetching wholesale discount for item skus : {}", itemSkus);
    GdnRestListResponse<WholesalePriceSkuResponse> wholesaleMappingResponse = null;
    if (multiPickupPointEnabled || pricingMultiPickupPointEnabled) {
      wholesaleMappingResponse = productPricingFeign.getWholesalePriceSkuDetailV2(storeId, requestId, itemSkus);
    } else {
      wholesaleMappingResponse = productPricingFeign.getWholesalePriceSkuDetail(storeId, requestId, itemSkus);
    }
    ResponseHelper.validateResponse(wholesaleMappingResponse);
    return wholesaleMappingResponse.getContent().stream().collect(Collectors.toMap(
        wholesaleMapping -> wholesaleMapping.getItemSku() + Constants.DASH_SEPARATOR + wholesaleMapping
            .getPickUpPointCode(), Function.identity()));
  }
}
