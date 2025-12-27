package com.gdn.x.product.outbound.impl;


import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.collections.CollectionUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.product.pricing.web.model.promo.bundling.request.PromoBundlingRequest;
import com.gdn.partners.product.pricing.web.model.promo.bundling.response.PromoBundlingSkuByItemSkuAndItemCodesResponse;
import com.gdn.partners.product.pricing.web.model.promo.bundling.response.PromoBundlingSkuDetailResponse;
import com.gdn.x.product.exception.ProductPricingException;
import com.gdn.x.product.model.vo.ActivePromoBundlingResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingByItemSkuAndItemCodesResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingDetailResponseVO;
import com.gdn.x.product.outbound.api.feign.ProductPricingFeign;

/**
 * @author nitinmathew - created on 03/02/2020
 **/
public class ProductPricingOutboundImplTest {

  private static final String STORE_ID = "store-id";

  private static final String CHANNEL_ID = "channel-id";

  private static final String CLIENT_ID = "client-id";

  private static final String REQUEST_ID = "request-id";

  private static final String USERNAME = "username";

  private static final String ITEM_SKU = "item-sku";

  private static final int PAGE = 0;

  private static final int SIZE = 10;

  private static final String SORT_BY = "sort-by";

  private static final String SORT_TYPE = "sort-type";

  private static final String ITEM_CODE = "item-code";

  private static final String PROMO_BUNDLING_ID = "promo-bundling-id";

  private static final String PROMO_BUNDLING_TYPE = "promo-bundling-type";

  private static final String PROMO_BUNDLING_NAME = "promo-bundling-name";

  @InjectMocks
  private ProductPricingOutboundImpl productPricingOutbound;

  @Mock
  private ProductPricingFeign productPricingFeign;

  @Mock
  private GdnMapper gdnMapper;

  private MandatoryRequestParam mandatoryRequestParam;

  private PromoBundlingSkuDetailResponse promoBundlingSkuDetailResponse;

  private GdnRestListResponse<PromoBundlingSkuDetailResponse> promoBundlingSkuDetailGdnRestListResponse;

  private PromoBundlingSkuByItemSkuAndItemCodesResponse promoBundlingSkuByItemSkuAndItemCodesResponse;

  private GdnRestSingleResponse<PromoBundlingSkuByItemSkuAndItemCodesResponse>
      promoBundlingSkuByItemSkuAndItemCodesGdnRestSingleResponse;

  private Set<String> itemSkus;

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
            USERNAME);
    promoBundlingSkuDetailResponse  = PromoBundlingSkuDetailResponse.builder()
        .itemSku(ITEM_SKU)
        .promoBundlingId(PROMO_BUNDLING_ID)
        .promoBundlingName(PROMO_BUNDLING_NAME)
        .promoBundlingType(PROMO_BUNDLING_TYPE)
        .build();
    promoBundlingSkuDetailGdnRestListResponse =
        new GdnRestListResponse<>(Collections.singletonList(promoBundlingSkuDetailResponse), new PageMetaData(),
            REQUEST_ID);
    promoBundlingSkuByItemSkuAndItemCodesResponse = PromoBundlingSkuByItemSkuAndItemCodesResponse.builder()
        .promoBundlings(Collections.singleton(promoBundlingSkuDetailResponse)).totalComboRule(0).totalWholesaleRule(1)
        .build();
    promoBundlingSkuByItemSkuAndItemCodesGdnRestSingleResponse =
        new GdnRestSingleResponse<>(promoBundlingSkuByItemSkuAndItemCodesResponse, REQUEST_ID);
    itemSkus = new HashSet<String>();
    itemSkus.add(ITEM_SKU);
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(productPricingFeign, gdnMapper);
  }

  @Test
  public void findActiveByPromoBundlingTypeAndItemSkusTest() throws Exception {
    PromoBundlingRequest promoBundlingRequest =
        PromoBundlingRequest.builder().promoBundlingType(PROMO_BUNDLING_TYPE).itemSkus(Collections.singleton(ITEM_SKU))
            .build();
    when(productPricingFeign
        .getActiveBundlingPromos(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, promoBundlingRequest))
        .thenReturn(promoBundlingSkuDetailGdnRestListResponse);
    when(gdnMapper.deepCopy(eq(promoBundlingSkuDetailResponse), eq(PromoBundlingDetailResponseVO.class)))
        .thenReturn(new PromoBundlingDetailResponseVO());
    ActivePromoBundlingResponseVO responseVO = productPricingOutbound
        .findActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PROMO_BUNDLING_TYPE,
            Collections.singleton(ITEM_SKU));
    assertEquals(0, responseVO.getTotalRecords());
    verify(productPricingFeign)
        .getActiveBundlingPromos(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, promoBundlingRequest);
    verify(gdnMapper).deepCopy(eq(promoBundlingSkuDetailResponse), eq(PromoBundlingDetailResponseVO.class));
  }

  @Test
  public void findActiveByPromoBundlingTypeAndItemSkus_whenResultNullTest() throws Exception {
    GdnRestListResponse<PromoBundlingSkuDetailResponse> response =
        new GdnRestListResponse<>(null, new PageMetaData(),
            REQUEST_ID);
    PromoBundlingRequest promoBundlingRequest =
        PromoBundlingRequest.builder().promoBundlingType(PROMO_BUNDLING_TYPE).itemSkus(Collections.singleton(ITEM_SKU))
            .build();
    when(productPricingFeign
        .getActiveBundlingPromos(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, promoBundlingRequest))
        .thenReturn(response);
    try {
      Assertions.assertThrows(ProductPricingException.class, () -> productPricingOutbound.findActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PROMO_BUNDLING_TYPE,
          Collections.singleton(ITEM_SKU)));
    } finally {
      verify(productPricingFeign)
          .getActiveBundlingPromos(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, promoBundlingRequest);
    }
  }

  @Test
  public void findActiveByPromoBundlingTypeAndItemSkus_whenGdnResultNullTest() throws Exception {
    PromoBundlingRequest promoBundlingRequest =
        PromoBundlingRequest.builder().promoBundlingType(PROMO_BUNDLING_TYPE).itemSkus(Collections.singleton(ITEM_SKU))
            .build();
    when(productPricingFeign
        .getActiveBundlingPromos(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, promoBundlingRequest))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ProductPricingException.class, () -> productPricingOutbound.findActiveByPromoBundlingTypeAndItemSkus(mandatoryRequestParam, PROMO_BUNDLING_TYPE,
          Collections.singleton(ITEM_SKU)));
    } finally {
      verify(productPricingFeign)
          .getActiveBundlingPromos(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, promoBundlingRequest);
    }
  }

  @Test
  public void findActiveByPromoBundlingTypeAndItemCodesTest() throws Exception {
    PromoBundlingRequest promoBundlingRequest = PromoBundlingRequest.builder().promoBundlingType(PROMO_BUNDLING_TYPE)
        .itemCodes(Collections.singleton(ITEM_CODE)).build();
    when(productPricingFeign
        .getActiveBundlingPromosByTypeAndItemCodes(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PAGE, SIZE,
            SORT_BY, SORT_TYPE, promoBundlingRequest)).thenReturn(promoBundlingSkuDetailGdnRestListResponse);
    when(gdnMapper.deepCopy(eq(promoBundlingSkuDetailResponse), eq(PromoBundlingDetailResponseVO.class)))
        .thenReturn(new PromoBundlingDetailResponseVO());
    ActivePromoBundlingResponseVO responseVO = productPricingOutbound
        .findActiveByPromoBundlingTypeAndItemCodes(mandatoryRequestParam, PAGE, SIZE, SORT_BY, SORT_TYPE,
            PROMO_BUNDLING_TYPE, Collections.singleton(ITEM_CODE));
    assertEquals(0, responseVO.getTotalRecords());
    verify(productPricingFeign)
        .getActiveBundlingPromosByTypeAndItemCodes(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PAGE, SIZE,
            SORT_BY, SORT_TYPE, promoBundlingRequest);
    verify(gdnMapper).deepCopy(eq(promoBundlingSkuDetailResponse), eq(PromoBundlingDetailResponseVO.class));
  }

  @Test
  public void findActiveByPromoBundlingTypeAndItemCodes_whenResultNullTest() throws Exception {
    GdnRestListResponse<PromoBundlingSkuDetailResponse> response =
        new GdnRestListResponse<>(null, new PageMetaData(), REQUEST_ID);
    PromoBundlingRequest promoBundlingRequest = PromoBundlingRequest.builder().promoBundlingType(PROMO_BUNDLING_TYPE)
        .itemCodes(Collections.singleton(ITEM_CODE)).build();
    when(productPricingFeign
        .getActiveBundlingPromosByTypeAndItemCodes(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PAGE, SIZE,
            SORT_BY, SORT_TYPE, promoBundlingRequest)).thenReturn(response);
    try {
      Assertions.assertThrows(ProductPricingException.class, () -> productPricingOutbound
          .findActiveByPromoBundlingTypeAndItemCodes(mandatoryRequestParam, PAGE, SIZE, SORT_BY, SORT_TYPE,
              PROMO_BUNDLING_TYPE, Collections.singleton(ITEM_CODE)));
    } finally {
      verify(productPricingFeign)
          .getActiveBundlingPromosByTypeAndItemCodes(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PAGE, SIZE,
              SORT_BY, SORT_TYPE, promoBundlingRequest);
    }
  }

  @Test
  public void findActiveByPromoBundlingTypeAndItemCodes_whenGdnResultNullTest() throws Exception {
    PromoBundlingRequest promoBundlingRequest = PromoBundlingRequest.builder().promoBundlingType(PROMO_BUNDLING_TYPE)
        .itemCodes(Collections.singleton(ITEM_CODE)).build();
    when(productPricingFeign
        .getActiveBundlingPromosByTypeAndItemCodes(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PAGE, SIZE,
            SORT_BY, SORT_TYPE, promoBundlingRequest)).thenReturn(null);
    try {
      Assertions.assertThrows(ProductPricingException.class, () -> productPricingOutbound
          .findActiveByPromoBundlingTypeAndItemCodes(mandatoryRequestParam, PAGE, SIZE, SORT_BY, SORT_TYPE,
              PROMO_BUNDLING_TYPE, Collections.singleton(ITEM_CODE)));
    } finally {
      verify(productPricingFeign)
          .getActiveBundlingPromosByTypeAndItemCodes(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PAGE, SIZE,
              SORT_BY, SORT_TYPE, promoBundlingRequest);
    }
  }

  @Test
  public void findActivePromoBundlingByIdsTest() throws Exception {
    PromoBundlingRequest promoBundlingRequest =
        PromoBundlingRequest.builder().promoBundlingIds(Collections.singleton(PROMO_BUNDLING_ID)).itemSkus(itemSkus)
            .build();
    when(productPricingFeign
        .getBundlingPromosByIds(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, promoBundlingRequest))
        .thenReturn(promoBundlingSkuDetailGdnRestListResponse);
    when(gdnMapper.deepCopy(eq(promoBundlingSkuDetailResponse), eq(PromoBundlingDetailResponseVO.class)))
        .thenReturn(new PromoBundlingDetailResponseVO());
    List<PromoBundlingDetailResponseVO> responseVOS = productPricingOutbound
        .findActivePromoBundlingByIds(mandatoryRequestParam, Collections.singleton(PROMO_BUNDLING_ID), itemSkus);
    Assertions.assertTrue(CollectionUtils.isNotEmpty(responseVOS));
    assertEquals(1, responseVOS.size());
    verify(productPricingFeign)
        .getBundlingPromosByIds(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, promoBundlingRequest);
    verify(gdnMapper).deepCopy(eq(promoBundlingSkuDetailResponse), eq(PromoBundlingDetailResponseVO.class));
  }

  @Test
  public void findActivePromoBundlingByIds_whenResultNullTest() throws Exception {
    GdnRestListResponse<PromoBundlingSkuDetailResponse> response =
        new GdnRestListResponse<>(null, new PageMetaData(),
            REQUEST_ID);
    PromoBundlingRequest promoBundlingRequest =
        PromoBundlingRequest.builder().promoBundlingIds(Collections.singleton(PROMO_BUNDLING_ID)).itemSkus(itemSkus)
            .build();
    when(productPricingFeign
        .getBundlingPromosByIds(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, promoBundlingRequest))
        .thenReturn(response);
    try {
      Assertions.assertThrows(ProductPricingException.class, () -> productPricingOutbound
          .findActivePromoBundlingByIds(mandatoryRequestParam, Collections.singleton(PROMO_BUNDLING_ID), itemSkus));
    } finally {
      verify(productPricingFeign)
          .getBundlingPromosByIds(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, promoBundlingRequest);
    }
  }

  @Test
  public void findActivePromoBundlingByIds_whenGdnResultNullTest() throws Exception {
    PromoBundlingRequest promoBundlingRequest =
        PromoBundlingRequest.builder().promoBundlingIds(Collections.singleton(PROMO_BUNDLING_ID)).itemSkus(itemSkus)
            .build();
    when(productPricingFeign
        .getBundlingPromosByIds(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, promoBundlingRequest))
        .thenReturn(null);
    try {
      Assertions.assertThrows(ProductPricingException.class, () -> productPricingOutbound
          .findActivePromoBundlingByIds(mandatoryRequestParam, Collections.singleton(PROMO_BUNDLING_ID), itemSkus));
    } finally {
      verify(productPricingFeign)
          .getBundlingPromosByIds(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, promoBundlingRequest);
    }
  }

  @Test
  public void getActiveAndPromoBundlingTotalByPromoBundlingTypeTest() throws Exception {
    PromoBundlingRequest promoBundlingRequest =
        PromoBundlingRequest.builder().promoBundlingType(PROMO_BUNDLING_TYPE)
            .itemCodes(Collections.singleton(ITEM_CODE)).build();
    when(productPricingFeign
        .getActiveAndPromoBundlingTotalByItemCodes(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, ITEM_SKU,
            promoBundlingRequest)).thenReturn(promoBundlingSkuByItemSkuAndItemCodesGdnRestSingleResponse);
    when(gdnMapper.deepCopy(eq(promoBundlingSkuByItemSkuAndItemCodesResponse),
        eq(PromoBundlingByItemSkuAndItemCodesResponseVO.class)))
        .thenReturn(new PromoBundlingByItemSkuAndItemCodesResponseVO());
    when(gdnMapper.deepCopy(eq(promoBundlingSkuDetailResponse), eq(PromoBundlingDetailResponseVO.class)))
        .thenReturn(new PromoBundlingDetailResponseVO());
    PromoBundlingByItemSkuAndItemCodesResponseVO responseVO = productPricingOutbound
        .getActiveAndPromoBundlingTotalByPromoBundlingType(mandatoryRequestParam, PROMO_BUNDLING_TYPE, ITEM_SKU,
            Collections.singleton(ITEM_CODE), false);
    Assertions.assertNotNull(responseVO);
    verify(productPricingFeign)
        .getActiveAndPromoBundlingTotalByItemCodes(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, ITEM_SKU,
            promoBundlingRequest);
    verify(gdnMapper).deepCopy(eq(promoBundlingSkuByItemSkuAndItemCodesResponse),
        eq(PromoBundlingByItemSkuAndItemCodesResponseVO.class));
    verify(gdnMapper).deepCopy(eq(promoBundlingSkuDetailResponse), eq(PromoBundlingDetailResponseVO.class));
  }

  @Test
  public void getActiveAndPromoBundlingTotalByPromoBundlingType_whenResultNullTest() throws Exception {
    GdnRestSingleResponse<PromoBundlingSkuByItemSkuAndItemCodesResponse> response =
        new GdnRestSingleResponse<>(null, REQUEST_ID);
    PromoBundlingRequest promoBundlingRequest = PromoBundlingRequest.builder().promoBundlingType(PROMO_BUNDLING_TYPE)
        .itemCodes(Collections.singleton(ITEM_CODE)).build();
    when(productPricingFeign
        .getActiveAndPromoBundlingTotalByItemCodes(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, ITEM_SKU,
            promoBundlingRequest)).thenReturn(response);
    try {
      Assertions.assertThrows(ProductPricingException.class, () -> productPricingOutbound
          .getActiveAndPromoBundlingTotalByPromoBundlingType(mandatoryRequestParam, PROMO_BUNDLING_TYPE, ITEM_SKU,
              Collections.singleton(ITEM_CODE), false));
    } finally {
      verify(productPricingFeign)
          .getActiveAndPromoBundlingTotalByItemCodes(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, ITEM_SKU,
              promoBundlingRequest);
    }
  }

  @Test
  public void getActiveAndPromoBundlingTotalByPromoBundlingType_whenGdnResultNullTest() throws Exception {
    PromoBundlingRequest promoBundlingRequest = PromoBundlingRequest.builder().promoBundlingType(PROMO_BUNDLING_TYPE)
        .itemCodes(Collections.singleton(ITEM_CODE)).build();
    when(productPricingFeign
        .getActiveAndPromoBundlingTotalByItemCodes(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, ITEM_SKU,
            promoBundlingRequest)).thenReturn(null);
    try {
      Assertions.assertThrows(ProductPricingException.class, () -> productPricingOutbound
          .getActiveAndPromoBundlingTotalByPromoBundlingType(mandatoryRequestParam, PROMO_BUNDLING_TYPE, ITEM_SKU,
              Collections.singleton(ITEM_CODE), false));
    } finally {
      verify(productPricingFeign)
          .getActiveAndPromoBundlingTotalByItemCodes(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, ITEM_SKU,
              promoBundlingRequest);
    }
  }

  @Test
  public void findActiveByPromoBundlingTypeAndItemSkusAndPickupPointCodeTest() {
    when(productPricingFeign.getActiveBundlingPromos(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID),
        eq(USERNAME), Mockito.any(PromoBundlingRequest.class))).thenReturn(
        new GdnRestListResponse<>(null, null, true, new ArrayList<>(), new PageMetaData(0, 0, 0), REQUEST_ID));

    productPricingOutbound.findActiveByPromoBundlingTypeAndItemSkusAndPickupPointCode(mandatoryRequestParam,
        PROMO_BUNDLING_TYPE, new HashSet<>());

    verify(productPricingFeign).getActiveBundlingPromos(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID),
        eq(USERNAME), Mockito.any(PromoBundlingRequest.class));
  }

  @Test
  public void findActiveByPromoBundlingTypeAndItemSkusAndPickupPointCodeSuccessFalseTest() {
    when(productPricingFeign.getActiveBundlingPromos(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID),
        eq(USERNAME), Mockito.any(PromoBundlingRequest.class))).thenReturn(null);

    try {
      Assertions.assertThrows(ProductPricingException.class, () -> productPricingOutbound.findActiveByPromoBundlingTypeAndItemSkusAndPickupPointCode(mandatoryRequestParam,
          PROMO_BUNDLING_TYPE, new HashSet<>()));
    }
    finally {
      verify(productPricingFeign).getActiveBundlingPromos(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID),
          eq(USERNAME), Mockito.any(PromoBundlingRequest.class));
    }
  }

  @Test
  public void findActiveByPromoBundlingTypeAndItemSkusAndPickupPointCodeContentEmptyTest() {
    when(productPricingFeign.getActiveBundlingPromos(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID),
        eq(USERNAME), Mockito.any(PromoBundlingRequest.class))).thenReturn(
        new GdnRestListResponse<>(null, null, true, null, new PageMetaData(0, 0, 0), REQUEST_ID));

    try {
      Assertions.assertThrows(ProductPricingException.class, () -> productPricingOutbound.findActiveByPromoBundlingTypeAndItemSkusAndPickupPointCode(mandatoryRequestParam,
          PROMO_BUNDLING_TYPE, new HashSet<>()));
    }
    finally {
      verify(productPricingFeign).getActiveBundlingPromos(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID),
          eq(USERNAME), Mockito.any(PromoBundlingRequest.class));
    }
  }
}