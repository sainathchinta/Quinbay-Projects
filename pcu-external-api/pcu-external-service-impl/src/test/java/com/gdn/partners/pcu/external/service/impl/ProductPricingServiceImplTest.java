package com.gdn.partners.pcu.external.service.impl;

import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.external.client.feign.ProductPricingFeign;
import com.gdn.partners.pcu.external.web.model.response.PromoItemDetailWebResponse;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceSkuDetailListRequest;
import com.gdn.partners.product.pricing.web.model.response.PromoSkuDetailResponse;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

public class ProductPricingServiceImplTest {

  private static final String ITEM_SKU = "ITEM_SKU";
  private static final String REQUEST_ID = "requestId";
  private static final String STORE_ID = "storeId";
  private Set<String> itemSkus = new HashSet<>();
  private WholesalePriceSkuDetailListRequest wholesalePriceSkuRequest = new WholesalePriceSkuDetailListRequest();
  private WholesalePriceSkuResponse wholesalePriceSku = new WholesalePriceSkuResponse();

  @Mock
  private ProductPricingFeign productPricingFeign;

  @InjectMocks
  private ProductPricingServiceImpl productPricingService;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    ReflectionTestUtils.setField(productPricingService, "multiPickupPointEnabled", false);
  }

  @Test
  public void getPromoDiscountSkuDetailTest() {
    when(this.productPricingFeign.getPromoSkuDetail(STORE_ID, REQUEST_ID, ITEM_SKU))
        .thenReturn(new GdnRestSingleResponse<>(new PromoSkuDetailResponse(), REQUEST_ID));
    PromoItemDetailWebResponse response =
        this.productPricingService.getPromoDiscountSkuDetail(STORE_ID, REQUEST_ID, ITEM_SKU);
    verify(this.productPricingFeign).getPromoSkuDetail(STORE_ID, REQUEST_ID, ITEM_SKU);
    Assertions.assertNotNull(response);
  }

  @Test
  public void getPromoDiscountSkuDetailExceptionTest() {
    when(this.productPricingFeign.getPromoSkuDetail(STORE_ID, REQUEST_ID, ITEM_SKU))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, REQUEST_ID));
    try {
      this.productPricingService.getPromoDiscountSkuDetail(STORE_ID, REQUEST_ID, ITEM_SKU);
    } catch (Exception e) {
      verify(this.productPricingFeign).getPromoSkuDetail(STORE_ID, REQUEST_ID, ITEM_SKU);
    }
  }

  @Test
  public void getItemWholesaleConfigTest() {
    itemSkus.add(ITEM_SKU);
    wholesalePriceSkuRequest.setItemSkus(itemSkus);
    wholesalePriceSku =
        WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU).wholesaleRules(new HashMap<>()).skuStatus("ACTIVE")
            .build();
    when(this.productPricingFeign.getWholesalePriceSkuDetail(STORE_ID, REQUEST_ID, wholesalePriceSkuRequest))
        .thenReturn(new GdnRestListResponse<>(null, null, true,
            Arrays.asList(wholesalePriceSku), null, null));
    Map<String, WholesalePriceSkuResponse> responseMap =
        productPricingService.getItemWholesaleConfig(STORE_ID, REQUEST_ID, wholesalePriceSkuRequest);
    verify(this.productPricingFeign).getWholesalePriceSkuDetail(STORE_ID, REQUEST_ID, wholesalePriceSkuRequest);
    Assertions.assertNotNull(responseMap);
  }

  @Test
  public void getPromoDiscountSkuDetail_MppONTest() {
    ReflectionTestUtils.setField(productPricingService, "multiPickupPointEnabled", true);
    when(this.productPricingFeign.getPromoSkuDetailV2(STORE_ID, REQUEST_ID, ITEM_SKU))
      .thenReturn(new GdnRestSingleResponse<>(new PromoSkuDetailResponse(), REQUEST_ID));
    PromoItemDetailWebResponse response =
      this.productPricingService.getPromoDiscountSkuDetail(STORE_ID, REQUEST_ID, ITEM_SKU);
    verify(this.productPricingFeign).getPromoSkuDetailV2(STORE_ID, REQUEST_ID, ITEM_SKU);
    Assertions.assertNotNull(response);
  }

  @Test
  public void getItemWholesaleConfig_MppOnTest() {
    ReflectionTestUtils.setField(productPricingService, "multiPickupPointEnabled", true);
    itemSkus.add(ITEM_SKU);
    wholesalePriceSkuRequest.setItemSkus(itemSkus);
    wholesalePriceSku =
      WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU).wholesaleRules(new HashMap<>()).skuStatus("ACTIVE")
        .build();
    when(this.productPricingFeign.getWholesalePriceSkuDetailV2(STORE_ID, REQUEST_ID, wholesalePriceSkuRequest))
      .thenReturn(new GdnRestListResponse<>(null, null, true,
        Arrays.asList(wholesalePriceSku), null, null));
    Map<String, WholesalePriceSkuResponse> responseMap =
      productPricingService.getItemWholesaleConfig(STORE_ID, REQUEST_ID, wholesalePriceSkuRequest);
    verify(this.productPricingFeign).getWholesalePriceSkuDetailV2(STORE_ID, REQUEST_ID, wholesalePriceSkuRequest);
    Assertions.assertNotNull(responseMap);
  }

  @Test
  public void getItemWholesaleConfigPiricngMppOnTest() {
    ReflectionTestUtils.setField(productPricingService, "pricingMultiPickupPointEnabled", true);
    itemSkus.add(ITEM_SKU);
    wholesalePriceSkuRequest.setItemSkus(itemSkus);
    wholesalePriceSku =
        WholesalePriceSkuResponse.builder().itemSku(ITEM_SKU).wholesaleRules(new HashMap<>()).skuStatus("ACTIVE")
            .build();
    when(this.productPricingFeign.getWholesalePriceSkuDetailV2(STORE_ID, REQUEST_ID, wholesalePriceSkuRequest))
        .thenReturn(new GdnRestListResponse<>(null, null, true,
            Arrays.asList(wholesalePriceSku), null, null));
    Map<String, WholesalePriceSkuResponse> responseMap =
        productPricingService.getItemWholesaleConfig(STORE_ID, REQUEST_ID, wholesalePriceSkuRequest);
    verify(this.productPricingFeign).getWholesalePriceSkuDetailV2(STORE_ID, REQUEST_ID, wholesalePriceSkuRequest);
    Assertions.assertNotNull(responseMap);
  }
}
