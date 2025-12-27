package com.gdn.partners.pcu.external.service;

import java.util.Map;

import com.gdn.partners.pcu.external.web.model.response.PromoItemDetailWebResponse;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceSkuDetailListRequest;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;

public interface ProductPricingService {

  /**
   * Get Promo Discount detail by itemSku
   * @param itemSku
   * @return
   */
  PromoItemDetailWebResponse getPromoDiscountSkuDetail(String storeId, String requestId, String itemSku);

  /**
   * Get CategoryHierarchyWithProductCount from ProductPricing
   *
   * @param
   * @return
   */
  Map<String, WholesalePriceSkuResponse> getItemWholesaleConfig(String storeId, String requestId,
      WholesalePriceSkuDetailListRequest itemSkus);
}
