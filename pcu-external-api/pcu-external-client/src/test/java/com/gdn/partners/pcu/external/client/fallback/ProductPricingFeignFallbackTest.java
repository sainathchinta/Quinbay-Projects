package com.gdn.partners.pcu.external.client.fallback;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceSkuDetailListRequest;
import com.gdn.partners.product.pricing.web.model.response.PromoSkuDetailResponse;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import org.junit.jupiter.api.Test;

import java.util.HashSet;
import java.util.Set;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;

public class ProductPricingFeignFallbackTest {

  private ProductPricingFeignFallback productPricingFeignFallback = new ProductPricingFeignFallback();

  private static final String ITEM_SKU = "ITEM_SKU";
  private static final String REQUEST_ID = "REQUEST_ID";
  private static final String STORE_ID = "10001";
  private Set<String> itemSkus = new HashSet<>();
  private WholesalePriceSkuDetailListRequest wholesalePriceSkuDetailListRequest =
      new WholesalePriceSkuDetailListRequest();

  @Test
  public void getPromoSkuDetail() {
    GdnRestSingleResponse<PromoSkuDetailResponse> response =
        productPricingFeignFallback.getPromoSkuDetail(STORE_ID, REQUEST_ID, ITEM_SKU);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getWholesalePriceSkuDetail() {
    itemSkus.add(ITEM_SKU);
    wholesalePriceSkuDetailListRequest.setItemSkus(itemSkus);
    GdnRestListResponse<WholesalePriceSkuResponse> response = productPricingFeignFallback
        .getWholesalePriceSkuDetail(STORE_ID, REQUEST_ID, wholesalePriceSkuDetailListRequest);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getWholesalePriceDetail() {
    GdnRestSingleResponse<WholesalePriceSkuResponse> response =
        productPricingFeignFallback.getWholesalePriceDetail(ITEM_SKU);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getWholesalePriceSkuDetailV2Test() {
    itemSkus.add(ITEM_SKU);
    wholesalePriceSkuDetailListRequest.setItemSkus(itemSkus);
    GdnRestListResponse<WholesalePriceSkuResponse> response = productPricingFeignFallback
      .getWholesalePriceSkuDetailV2(STORE_ID, REQUEST_ID, wholesalePriceSkuDetailListRequest);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }

  @Test
  public void getPromoSkuDetailV2Test() {
    GdnRestSingleResponse<PromoSkuDetailResponse> response =
      productPricingFeignFallback.getPromoSkuDetailV2(STORE_ID, REQUEST_ID, ITEM_SKU);
    assertFalse(response.isSuccess());
    assertEquals(ErrorCategory.COMMUNICATION_FAILURE.getMessage(), response.getErrorCode());
  }
}
