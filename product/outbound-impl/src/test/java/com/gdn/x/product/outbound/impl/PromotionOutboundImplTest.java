package com.gdn.x.product.outbound.impl;

import static java.util.stream.Collectors.toSet;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.anyList;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import org.apache.commons.collections.CollectionUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.util.ReflectionTestUtils;

import com.gdn.common.base.mapper.GdnMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.exception.ProductPricingException;
import com.gdn.x.product.model.response.AdjustmentProductChangeResponseVO;
import com.gdn.x.product.model.response.AdjustmentProductResponse;
import com.gdn.x.product.model.vo.ActivePromoBundlingResponseVO;
import com.gdn.x.product.model.vo.ProductAndItemsVO;
import com.gdn.x.product.model.vo.PromoBundlingByItemSkuAndItemCodesResponseVO;
import com.gdn.x.product.model.vo.PromoBundlingDetailResponseVO;
import com.gdn.x.product.outbound.api.ProductPricingOutbound;
import com.gdn.x.product.outbound.api.feign.PromotionFeign;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.promotion.enums.PromoBundlingType;
import com.gdn.x.promotion.rest.web.model.dto.request.AdjustmentProductSkuRequest;
import com.gdn.x.promotion.rest.web.model.dto.request.SimpleSetStringRequest;
import com.gdn.x.promotion.rest.web.model.promo.bundling.WholesaleRule;
import com.gdn.x.promotion.rest.web.model.promo.bundling.request.PromoBundlingByItemSkuAndItemCodesRequest;
import com.gdn.x.promotion.rest.web.model.promo.bundling.response.PromoBundlingByItemSkuAndItemCodesResponse;
import com.gdn.x.promotion.rest.web.model.promo.bundling.response.PromoBundlingDetailResponse;

;

public class PromotionOutboundImplTest {

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
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String CAMPAIGN_CODE = "campaignCode";

  @InjectMocks
  private PromotionOutboundImpl promotionOutboundImpl;

  @Mock
  private PromotionFeign promotionFeign;

  @Mock
  private ProductPricingOutbound productPricingOutbound;

  @Mock
  private GdnMapper gdnMapper;

  private AdjustmentProductSkuRequest promotionRequest;

  private GdnRestListResponse<PromoBundlingDetailResponse> promoBundlingDetailResponseGdnRestListResponse;

  private PromoBundlingDetailResponse promoBundlingDetailResponse;

  private List<PromoBundlingDetailResponse> promoBundlingDetailResponses;

  private SimpleSetStringRequest request;

  private Set<String> itemCodes;

  private MandatoryRequestParam param;

  private List<PromoBundlingByItemSkuAndItemCodesResponse> promoBundlingByItemSkuAndItemCodesResponses;

  private PromoBundlingByItemSkuAndItemCodesResponse promoBundlingByItemSkuAndItemCodesResponse;

  private GdnRestSingleResponse<PromoBundlingByItemSkuAndItemCodesResponse> promoBundlingByItemSkuAndItemCodesResponseGdnRestListResponse;

  private PromoBundlingByItemSkuAndItemCodesRequest promoBundlingByItemSkuAndItemCodesRequest;

  private MandatoryRequestParam mandatoryRequestParam;
  private Set<String> itemSkus = Stream.of(ITEM_SKU).collect(toSet());
  private SimpleSetStringRequest simpleSetStringRequest;
  private ProductAndItemsVO productAndItemsVO = new ProductAndItemsVO();
  private List<ProductAndItemsVO> productAndItemsList = Collections.singletonList(productAndItemsVO);
  private GdnRestListResponse<AdjustmentProductChangeResponseVO> adjustmentProductChangeResponse =
    new GdnRestListResponse<>();

  @Test
  public void getAdjustmentProductExceptionThrown() throws Exception {
    when(this.promotionFeign.getPromosBySku(anyString(), anyString(), anyString(),
        anyString(), anyString(), Mockito.any())).thenThrow(RuntimeException.class);

    this.promotionOutboundImpl.getAdjustmentProduct(PromotionOutboundImplTest.REQUEST_ID,
        PromotionOutboundImplTest.USERNAME, Arrays.asList(PromotionOutboundImplTest.ITEM_SKU));
    verify(this.promotionFeign).getPromosBySku(anyString(), anyString(), anyString(),
        anyString(), anyString(), Mockito.any());
  }

  @Test
  public void getAdjustmentProductTest() throws Exception {
    when(this.promotionFeign.getPromosBySku(anyString(), anyString(), anyString(),
        anyString(), anyString(), Mockito.any())).thenReturn(new GdnRestListResponse<>());
    this.promotionOutboundImpl.getAdjustmentProduct(PromotionOutboundImplTest.REQUEST_ID,
        PromotionOutboundImplTest.USERNAME, Arrays.asList(PromotionOutboundImplTest.ITEM_SKU));
    verify(this.promotionFeign).getPromosBySku(anyString(), anyString(), anyString(),
        anyString(), anyString(), Mockito.any());
  }


  @Test
  public void getAdjustmentProductWithEmptyItemSkus() throws Exception {

    List<AdjustmentProductResponse> responses =
        this.promotionOutboundImpl.getAdjustmentProduct(PromotionOutboundImplTest.REQUEST_ID,
            PromotionOutboundImplTest.USERNAME, new ArrayList<String>());
    assertEquals(responses.size(), (0));
  }

  @Test
  public void getAdjustmentProductWithNullItemSkus() throws Exception {

    List<AdjustmentProductResponse> responses =
        this.promotionOutboundImpl.getAdjustmentProduct(PromotionOutboundImplTest.REQUEST_ID,
            PromotionOutboundImplTest.USERNAME, null);
    assertEquals(responses.size(), (0));
  }

  @Test
  public void getActiveByPromoBundlingTypeAndItemSkus_whenPricingRouteTrueTest() throws Exception {
    ReflectionTestUtils.setField(this.promotionOutboundImpl, "promoBundlingProductPricingRoute", true);
    when(productPricingOutbound
        .findActiveByPromoBundlingTypeAndItemSkus(param, PromoBundlingType.WHOLESALE.name(), itemSkus))
        .thenReturn(new ActivePromoBundlingResponseVO());
    ActivePromoBundlingResponseVO responseVO =
        promotionOutboundImpl.getActiveByPromoBundlingTypeAndItemSkus(param, PromoBundlingType.WHOLESALE, itemSkus);
    assertEquals(responseVO, new ActivePromoBundlingResponseVO());
    verify(productPricingOutbound)
        .findActiveByPromoBundlingTypeAndItemSkus(param, PromoBundlingType.WHOLESALE.name(), itemSkus);
  }

  @Test
  public void getActiveByPromoBundlingTypeAndItemSkus_whenPricingRouteTrueAndExceptionTest() throws Exception {
    ReflectionTestUtils.setField(this.promotionOutboundImpl, "promoBundlingProductPricingRoute", true);
    when(productPricingOutbound
        .findActiveByPromoBundlingTypeAndItemSkus(param, PromoBundlingType.WHOLESALE.name(), itemSkus))
        .thenThrow(new ProductPricingException());
    ActivePromoBundlingResponseVO responseVO =
        promotionOutboundImpl.getActiveByPromoBundlingTypeAndItemSkus(param, PromoBundlingType.WHOLESALE, itemSkus);
    assertEquals(responseVO, new ActivePromoBundlingResponseVO());
    verify(productPricingOutbound)
        .findActiveByPromoBundlingTypeAndItemSkus(param, PromoBundlingType.WHOLESALE.name(), itemSkus);
  }

  @BeforeEach
  public void init() throws Exception {
    openMocks(this);
    this.promotionRequest =
        new AdjustmentProductSkuRequest(Arrays.asList(PromotionOutboundImplTest.ITEM_SKU), null);

    this.param = MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, null);

    this.promoBundlingDetailResponse = new PromoBundlingDetailResponse();
    this.promoBundlingDetailResponse.setItemSku(ITEM_SKU);
    this.promoBundlingDetailResponse.setPromoBundlingId(PROMO_BUNDLING_ID);
    this.promoBundlingDetailResponse.setPromoBundlingType(PROMO_BUNDLING_TYPE);
    this.promoBundlingDetailResponse.setPromoBundlingName(PROMO_BUNDLING_NAME);

    this.promoBundlingDetailResponses = new ArrayList<>();
    this.promoBundlingDetailResponses.add(promoBundlingDetailResponse);

    this.promoBundlingDetailResponseGdnRestListResponse = new GdnRestListResponse<>();
    this.promoBundlingDetailResponseGdnRestListResponse.setContent(this.promoBundlingDetailResponses);
    this.promoBundlingDetailResponseGdnRestListResponse.setSuccess(true);

    this.request = new SimpleSetStringRequest();
    this.itemCodes = new HashSet<>();
    this.itemCodes.add(ITEM_CODE);

    this.mandatoryRequestParam = MandatoryRequestParam
        .generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
            USERNAME);
    this.simpleSetStringRequest = new SimpleSetStringRequest();
    this.simpleSetStringRequest.setValue(itemSkus);

    promoBundlingByItemSkuAndItemCodesResponses = new ArrayList<>();

    promoBundlingByItemSkuAndItemCodesResponse = new PromoBundlingByItemSkuAndItemCodesResponse();
    promoBundlingByItemSkuAndItemCodesResponse.setId("id");
    promoBundlingByItemSkuAndItemCodesResponse.setStoreId(STORE_ID);

    this.promoBundlingByItemSkuAndItemCodesResponseGdnRestListResponse =
        new GdnRestSingleResponse<>();
    this.promoBundlingByItemSkuAndItemCodesResponseGdnRestListResponse
        .setValue(promoBundlingByItemSkuAndItemCodesResponse);
    this.promoBundlingByItemSkuAndItemCodesResponseGdnRestListResponse.setSuccess(true);
    promoBundlingByItemSkuAndItemCodesRequest = new PromoBundlingByItemSkuAndItemCodesRequest();
    promoBundlingByItemSkuAndItemCodesRequest.setItemSku(ITEM_SKU);
    promoBundlingByItemSkuAndItemCodesRequest.setItemCodes(itemCodes);
    this.adjustmentProductChangeResponse.setContent(Collections.singletonList(
      AdjustmentProductChangeResponseVO.builder().productSku(ITEM_SKU)
        .pickupPointCode(PICKUP_POINT_CODE).campaignCode(CAMPAIGN_CODE).build()));
    this.adjustmentProductChangeResponse.setSuccess(Boolean.TRUE);
    this.adjustmentProductChangeResponse.setErrorCode(null);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.productPricingOutbound);
  }

  @Test
  public void getActiveCombosByItemCodes_whenPricingRouteTrueTest() throws Exception {
    ReflectionTestUtils.setField(this.promotionOutboundImpl, "promoBundlingProductPricingRoute", true);
    when(productPricingOutbound.findActiveByPromoBundlingTypeAndItemCodes(param, PAGE, SIZE, SORT_BY, SORT_TYPE,
        PromoBundlingType.COMBO.name(), itemCodes)).thenReturn(new ActivePromoBundlingResponseVO());
    ActivePromoBundlingResponseVO responseVO =
        promotionOutboundImpl.getActiveCombosByItemCodes(param, PAGE, SIZE, SORT_BY, SORT_TYPE, itemCodes);
    assertEquals(responseVO, new ActivePromoBundlingResponseVO());
    verify(productPricingOutbound).findActiveByPromoBundlingTypeAndItemCodes(param, PAGE, SIZE, SORT_BY, SORT_TYPE,
        PromoBundlingType.COMBO.name(), itemCodes);
  }

  @Test
  public void getActiveCombosByItemCodes_whenPricingRouteTrueAndExceptionTest() throws Exception {
    ReflectionTestUtils.setField(this.promotionOutboundImpl, "promoBundlingProductPricingRoute", true);
    when(productPricingOutbound.findActiveByPromoBundlingTypeAndItemCodes(param, PAGE, SIZE, SORT_BY, SORT_TYPE,
        PromoBundlingType.COMBO.name(), itemCodes)).thenThrow(new ProductPricingException());
    ActivePromoBundlingResponseVO responseVO =
        promotionOutboundImpl.getActiveCombosByItemCodes(param, PAGE, SIZE, SORT_BY, SORT_TYPE, itemCodes);
    assertEquals(responseVO, new ActivePromoBundlingResponseVO());
    verify(productPricingOutbound).findActiveByPromoBundlingTypeAndItemCodes(param, PAGE, SIZE, SORT_BY, SORT_TYPE,
        PromoBundlingType.COMBO.name(), itemCodes);
  }

  @Test
  public void getActiveAndPromoBundlingTotalByPromoBundlingType_whenPricingRouteTrueTest() throws Exception {
    ReflectionTestUtils.setField(this.promotionOutboundImpl, "promoBundlingProductPricingRoute", true);
    when(productPricingOutbound
        .getActiveAndPromoBundlingTotalByPromoBundlingType(param, PROMO_BUNDLING_TYPE, ITEM_SKU, itemCodes, false))
        .thenReturn(new PromoBundlingByItemSkuAndItemCodesResponseVO());
    PromoBundlingByItemSkuAndItemCodesResponseVO responseVO = promotionOutboundImpl
        .getActiveAndPromoBundlingTotalByPromoBundlingType(param, PROMO_BUNDLING_TYPE, ITEM_SKU, itemCodes, false);
    assertNotNull(responseVO);
    verify(productPricingOutbound)
        .getActiveAndPromoBundlingTotalByPromoBundlingType(param, PROMO_BUNDLING_TYPE, ITEM_SKU, itemCodes, false);
  }

  @Test
  public void getActiveAndPromoBundlingTotalByPromoBundlingType_whenPricingRouteTrueAndNewVersionTest() throws Exception {
    ReflectionTestUtils.setField(this.promotionOutboundImpl, "promoBundlingProductPricingRoute", true);
    when(productPricingOutbound
        .getActiveAndPromoBundlingTotalByPromoBundlingType(param, PROMO_BUNDLING_TYPE, ITEM_SKU, itemCodes, true))
        .thenReturn(new PromoBundlingByItemSkuAndItemCodesResponseVO());
    PromoBundlingByItemSkuAndItemCodesResponseVO responseVO = promotionOutboundImpl
        .getActiveAndPromoBundlingTotalByPromoBundlingType(param, PROMO_BUNDLING_TYPE, ITEM_SKU, itemCodes, true);
    assertNotNull(responseVO);
    verify(productPricingOutbound)
        .getActiveAndPromoBundlingTotalByPromoBundlingType(param, PROMO_BUNDLING_TYPE, ITEM_SKU, itemCodes, true);
  }

  @Test
  public void getActiveAndPromoBundlingTotalByPromoBundlingType_whenPricingRouteTrueAndExceptionTest()
      throws Exception {
    ReflectionTestUtils.setField(this.promotionOutboundImpl, "promoBundlingProductPricingRoute", true);
    when(productPricingOutbound
        .getActiveAndPromoBundlingTotalByPromoBundlingType(param, PROMO_BUNDLING_TYPE, ITEM_SKU, itemCodes, false))
        .thenThrow(new ProductPricingException());
    PromoBundlingByItemSkuAndItemCodesResponseVO responseVO = promotionOutboundImpl
        .getActiveAndPromoBundlingTotalByPromoBundlingType(param, PROMO_BUNDLING_TYPE, ITEM_SKU, itemCodes, false);
    assertNotNull(responseVO);
    verify(productPricingOutbound)
        .getActiveAndPromoBundlingTotalByPromoBundlingType(param, PROMO_BUNDLING_TYPE, ITEM_SKU, itemCodes, false);
  }

  @Test
  public void getPromoBundlingDetailByPromoBundlingIds_whenPricingRouteTrueTest() throws Exception {
    ReflectionTestUtils.setField(this.promotionOutboundImpl, "promoBundlingProductPricingRoute", true);
    when(productPricingOutbound.findActivePromoBundlingByIds(param, Collections.singleton(PROMO_BUNDLING_ID), itemSkus))
        .thenReturn(Collections.singletonList(new PromoBundlingDetailResponseVO()));
    List<PromoBundlingDetailResponseVO> responseVOS =
        promotionOutboundImpl.getPromoBundlingDetailByPromoBundlingIds(param, Collections.singleton(PROMO_BUNDLING_ID),
            itemSkus);
    assertTrue(CollectionUtils.isNotEmpty(responseVOS));
    assertEquals(1, responseVOS.size());
    verify(productPricingOutbound).findActivePromoBundlingByIds(param, Collections.singleton(PROMO_BUNDLING_ID),
        itemSkus);
  }

  @Test
  public void getPromoBundlingDetailByPromoBundlingIds_whenPricingRouteTrueAndExceptionTest() throws Exception {
    ReflectionTestUtils.setField(this.promotionOutboundImpl, "promoBundlingProductPricingRoute", true);
    when(productPricingOutbound.findActivePromoBundlingByIds(param, Collections.singleton(PROMO_BUNDLING_ID), itemSkus))
        .thenThrow(new ProductPricingException());
    List<PromoBundlingDetailResponseVO> responseVOS =
        promotionOutboundImpl.getPromoBundlingDetailByPromoBundlingIds(param, Collections.singleton(PROMO_BUNDLING_ID),
            itemSkus);
    assertTrue(CollectionUtils.isEmpty(responseVOS));
    verify(productPricingOutbound).findActivePromoBundlingByIds(param, Collections.singleton(PROMO_BUNDLING_ID),
        itemSkus);
  }

  @Test
  public void getAdjustmentProductBySkuAndPickupPointCodeTest() {
    when(this.promotionFeign.getAdjustmentProductBySkuAndPickupPointCodeList(anyString(), anyString(),
      anyString(), anyString(), anyString(), eq(Collections.singletonList(
        ItemPickupPointRequest.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
          .build())))).thenReturn(adjustmentProductChangeResponse);
    this.promotionOutboundImpl.getAdjustmentProductBySkuAndPickupPointCode(STORE_ID,
      Collections.singletonList(
        ItemPickupPointRequest.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
          .build()));
    verify(this.promotionFeign).getAdjustmentProductBySkuAndPickupPointCodeList(anyString(),
      anyString(), anyString(), anyString(), anyString(), anyList());
  }

  @Test
  public void getAdjustmentProductBySkuAndPickupPointCodeExceptionTest() {
    this.adjustmentProductChangeResponse.setSuccess(Boolean.FALSE);
    Assertions.assertThrows(Exception.class,
        () -> this.promotionOutboundImpl.getAdjustmentProductBySkuAndPickupPointCode(STORE_ID,
            Collections.singletonList(
                ItemPickupPointRequest.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).build())));
  }
}
