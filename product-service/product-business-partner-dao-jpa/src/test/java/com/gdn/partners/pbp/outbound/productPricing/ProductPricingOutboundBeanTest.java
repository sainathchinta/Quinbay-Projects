package com.gdn.partners.pbp.outbound.productPricing;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pbp.commons.util.CommonUtils;
import com.gdn.partners.pbp.dto.promo.request.FreeSampleParticipationResponse;
import com.gdn.partners.pbp.model.SortOrder;
import com.gdn.partners.pbp.outbound.productPricing.feign.ProductPricingFeign;
import com.gdn.partners.product.pricing.web.model.dto.FailedItemReasonDto;
import com.gdn.partners.product.pricing.web.model.dto.ItemInfoDto;
import com.gdn.partners.product.pricing.web.model.dto.WholeSalePriceSkuStatusDto;
import com.gdn.partners.product.pricing.web.model.promo.bundling.request.PromoBundlingPricingDetailRequest;
import com.gdn.partners.pbp.dto.promo.request.PromoBundlingPricingSummaryRequest;
import com.gdn.partners.product.pricing.web.model.promo.bundling.response.ComboRule;
import com.gdn.partners.product.pricing.web.model.promo.bundling.response.PromoBundlingPricingDetailResponse;
import com.gdn.partners.product.pricing.web.model.promo.bundling.response.PromoBundlingSkuResponse;
import com.gdn.partners.product.pricing.web.model.promo.bundling.response.PromoBundlingSummaryResponse;
import com.gdn.partners.product.pricing.web.model.request.BulkActivateDeactivateRequest;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceBulkUpdateRequest;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceRequest;
import com.gdn.partners.product.pricing.web.model.request.WholesalePriceSkuDetailListRequest;
import com.gdn.partners.product.pricing.web.model.response.BulkActivateDeactivateResponse;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceBulkUpdateResponse;
import com.gdn.partners.product.pricing.web.model.response.WholesalePriceSkuResponse;
import com.gdn.x.product.enums.SolrConstants;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.slf4j.MDC;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.util.ReflectionTestUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.initMocks;

/**
 * @author nitinmathew - created on 06/02/2020
 **/
public class ProductPricingOutboundBeanTest {
  private static final String STORE_ID = "storeId";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String PRODUCT_SKU = "productSku";
  private static final String AUTHENTICATOR = "authenticator";
  private static final int PAGE_NUMBER = 0;
  private static final int PAGE_SIZE = 1;
  private static final int PAGE_TOTAL_RECORDS = 100;
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String PROMO_BUNDLING_CODE = "promoBundlingCode";
  private static final String PROMO_BUNDLING_STATUS = "PENDING";
  private static final String MAIN_ITEM_SKU = "mainItemSku";
  private static final String COMPLEMENTARY_ITEM_SKU = "complementaryItemSku";
  private static final String ACTIVE_STATUS = "ACTIVE";
  private static final String ITEM_SKU = "item-sku";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";

  private static  final String START_DATE = "startDate";
  @InjectMocks
  private ProductPricingOutboundBean productPricingOutboundBean;

  @Mock
  private ProductPricingFeign productPricingFeign;

  private PageMetaData pageMetaData;
  private PromoBundlingPricingDetailResponse promoBundlingPricingDetailResponse;
  private WholesalePriceSkuDetailListRequest wholesalePriceSkuDetailListRequest =
      new WholesalePriceSkuDetailListRequest();
  private ItemInfoDto itemInfoDto = new ItemInfoDto();
  private WholesalePriceRequest wholesalePriceRequest = new WholesalePriceRequest();
  private WholesalePriceSkuResponse wholesalePriceSkuResponse = new WholesalePriceSkuResponse();
  private BulkActivateDeactivateRequest bulkActivateDeactivateRequest = new BulkActivateDeactivateRequest();
  private GdnRestSingleResponse<BulkActivateDeactivateResponse> bulkActivateDeactivateResponse;
  PromoBundlingSummaryResponse promoBundlingSummaryResponse;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    MDC.put("storeId", STORE_ID);
    MDC.put("channelId", CHANNEL_ID);
    MDC.put("clientId", CLIENT_ID);
    MDC.put("requestId", REQUEST_ID);
    MDC.put("username", USERNAME);
    MDC.put("authenticatorId", AUTHENTICATOR);
    pageMetaData = new PageMetaData(PAGE_SIZE, PAGE_NUMBER, PAGE_TOTAL_RECORDS);
    LinkedHashMap<Integer, Double> wholesaleRules = new LinkedHashMap<>();
    wholesaleRules.put(2, 20D);
    wholesaleRules.put(5, 50D);
    PromoBundlingSkuResponse promoBundlingSkuResponse = PromoBundlingSkuResponse.builder()
        .itemSku(MAIN_ITEM_SKU)
        .comboRule(ComboRule.builder().itemSku(MAIN_ITEM_SKU).mainSku(true).availableQuantity(true).build())
        .wholesaleRules(wholesaleRules)
        .build();
    PromoBundlingSkuResponse promoBundlingSkuResponseComplementary = PromoBundlingSkuResponse.builder()
        .itemSku(COMPLEMENTARY_ITEM_SKU)
        .comboRule(ComboRule.builder().itemSku(COMPLEMENTARY_ITEM_SKU).mainSku(false).availableQuantity(true).build())
        .build();
    promoBundlingPricingDetailResponse = PromoBundlingPricingDetailResponse.builder()
        .promoBundlingCode(PROMO_BUNDLING_CODE)
        .promoBundlingStatus(PROMO_BUNDLING_STATUS)
        .promoBundlingSkuList(Collections.singletonList(promoBundlingSkuResponse))
        .complementaryPromoBundlingSkuList(Collections.singletonList(promoBundlingSkuResponseComplementary))
        .build();


    wholesalePriceSkuDetailListRequest.setItemSkus(Collections.singleton(MAIN_ITEM_SKU));
    itemInfoDto.setItemSku(ITEM_SKU);
    itemInfoDto.setPickupPointCode(PICKUP_POINT_CODE);

    wholesalePriceRequest.setMerchantCode(MERCHANT_CODE);

    bulkActivateDeactivateRequest.setItemSku(MAIN_ITEM_SKU);
    bulkActivateDeactivateRequest.setPickUpPointCode(PICKUP_POINT_CODE);
    bulkActivateDeactivateRequest.setUpdateSkuStatus(ACTIVE_STATUS);
    bulkActivateDeactivateResponse =
        new GdnRestSingleResponse<>(null, null, true, new BulkActivateDeactivateResponse(), REQUEST_ID);

    this.promoBundlingSummaryResponse =
        PromoBundlingSummaryResponse.builder().promoBundlingCode(PROMO_BUNDLING_CODE).promoBundlingName("promoName")
            .promoBundlingType(com.gdn.partners.product.pricing.model.enums.PromoBundlingType.FREE_SAMPLE.name())
            .build();
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(productPricingFeign);
  }

  @Test
  public void getWholesalePriceMPPOn() throws Exception {
    itemInfoDto.setItemSku(MAIN_ITEM_SKU);
    itemInfoDto.setItemPickupPointId(CommonUtils.getItemSkuAndPickupPointKey(MAIN_ITEM_SKU, PICKUP_POINT_CODE));
    wholesalePriceSkuDetailListRequest.setItemInfo(Collections.singletonList(itemInfoDto));
    when(productPricingFeign.getWholesalePriceListDetailV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        wholesalePriceSkuDetailListRequest)).thenReturn(
        new GdnRestListResponse<>(null, null, true, Collections.singletonList(wholesalePriceSkuResponse), null,
            REQUEST_ID));
    WholesalePriceSkuResponse response = productPricingOutboundBean.getWholesalePrice(MAIN_ITEM_SKU,
        PICKUP_POINT_CODE);
    verify(productPricingFeign).getWholesalePriceListDetailV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        wholesalePriceSkuDetailListRequest);
    assertNotNull(response);
  }

  @Test
  public void getWholesalePricePricingMPPOnSuccessFalse() throws Exception {
    itemInfoDto.setItemSku(MAIN_ITEM_SKU);
    itemInfoDto.setItemPickupPointId(CommonUtils.getItemSkuAndPickupPointKey(MAIN_ITEM_SKU, PICKUP_POINT_CODE));
    wholesalePriceSkuDetailListRequest.setItemInfo(Collections.singletonList(itemInfoDto));
    when(productPricingFeign.getWholesalePriceListDetailV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        wholesalePriceSkuDetailListRequest)).thenReturn(
        new GdnRestListResponse<>(null, null, false, Collections.singletonList(wholesalePriceSkuResponse), null,
            REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        WholesalePriceSkuResponse response =
            productPricingOutboundBean.getWholesalePrice(MAIN_ITEM_SKU, PICKUP_POINT_CODE);
      });
    } finally {
      verify(productPricingFeign).getWholesalePriceListDetailV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
          wholesalePriceSkuDetailListRequest);
    }
  }

  @Test
  public void getWholesalePricePricingMPPOnEmptyData() throws Exception {
    itemInfoDto.setItemSku(MAIN_ITEM_SKU);
    itemInfoDto.setItemPickupPointId(CommonUtils.getItemSkuAndPickupPointKey(MAIN_ITEM_SKU, PICKUP_POINT_CODE));
    wholesalePriceSkuDetailListRequest.setItemInfo(Collections.singletonList(itemInfoDto));
    when(productPricingFeign.getWholesalePriceListDetailV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        wholesalePriceSkuDetailListRequest)).thenReturn(
        new GdnRestListResponse<>(null, null, true, new ArrayList<>(), null,
            REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        WholesalePriceSkuResponse response =
            productPricingOutboundBean.getWholesalePrice(MAIN_ITEM_SKU, PICKUP_POINT_CODE);
      });
    } finally {
      verify(productPricingFeign).getWholesalePriceListDetailV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
          wholesalePriceSkuDetailListRequest);
    }
  }

  @Test
  public void upsertWholesalePriceExceptionTest() throws Exception {
    when(productPricingFeign.bulkUpdateV2(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID), eq(MERCHANT_CODE), eq(USERNAME),
        Mockito.anyList())).thenReturn(new GdnRestSingleResponse(StringUtils.EMPTY, StringUtils.EMPTY, false,
        new WholesalePriceBulkUpdateResponse(), REQUEST_ID));
    try {
      productPricingOutboundBean.upsertWholesalePrice(wholesalePriceRequest);
    } catch (Exception e) {
    } finally {
      verify(productPricingFeign).bulkUpdateV2(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID), eq(USERNAME),
          eq(MERCHANT_CODE), Mockito.anyList());
    }
  }

  @Test
  public void setWholesaleActivateStatusMppOnTest() throws Exception {
    Mockito.when(productPricingFeign
        .bulkActivateOrDeactivateSkuV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
            Collections.singletonList(bulkActivateDeactivateRequest))).thenReturn(bulkActivateDeactivateResponse);
    boolean response =
        productPricingOutboundBean.setWholesaleActivated(MAIN_ITEM_SKU, ACTIVE_STATUS, PICKUP_POINT_CODE);
    verify(productPricingFeign).bulkActivateOrDeactivateSkuV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        Collections.singletonList(bulkActivateDeactivateRequest));
    assertTrue(response);
  }

  @Test
  public void setWholesaleActivateStatusPricingMppOnFalseTest() throws Exception {
    bulkActivateDeactivateResponse.setSuccess(false);
    Mockito.when(productPricingFeign
        .bulkActivateOrDeactivateSkuV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
            Collections.singletonList(bulkActivateDeactivateRequest))).thenReturn(bulkActivateDeactivateResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        boolean response =
            productPricingOutboundBean.setWholesaleActivated(MAIN_ITEM_SKU, ACTIVE_STATUS, PICKUP_POINT_CODE);
      });
    } finally {
      verify(productPricingFeign).bulkActivateOrDeactivateSkuV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
          Collections.singletonList(bulkActivateDeactivateRequest));
    }
  }

  @Test
  public void setWholesaleActivateStatusPricingMppOnObjectNullTest() throws Exception {
    bulkActivateDeactivateResponse.setValue(null);
    Mockito.when(productPricingFeign
        .bulkActivateOrDeactivateSkuV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
            Collections.singletonList(bulkActivateDeactivateRequest))).thenReturn(bulkActivateDeactivateResponse);
    boolean response =
        productPricingOutboundBean.setWholesaleActivated(MAIN_ITEM_SKU, ACTIVE_STATUS, PICKUP_POINT_CODE);
    verify(productPricingFeign).bulkActivateOrDeactivateSkuV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        Collections.singletonList(bulkActivateDeactivateRequest));
    assertTrue(response);
  }

  @Test
  public void setWholesaleActivateStatusPricingMppOnFailedTest() throws Exception {
    BulkActivateDeactivateResponse bulkActivateDeactivate = new BulkActivateDeactivateResponse();
    bulkActivateDeactivate
        .setFailedItemReason(Collections.singletonList(new FailedItemReasonDto(ITEM_SKU, null, null)));
    bulkActivateDeactivateResponse.setValue(bulkActivateDeactivate);
    Mockito.when(productPricingFeign
        .bulkActivateOrDeactivateSkuV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
            Collections.singletonList(bulkActivateDeactivateRequest))).thenReturn(bulkActivateDeactivateResponse);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        boolean response =
            productPricingOutboundBean.setWholesaleActivated(MAIN_ITEM_SKU, ACTIVE_STATUS, PICKUP_POINT_CODE);
      });
    } finally {
      verify(productPricingFeign).bulkActivateOrDeactivateSkuV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
          Collections.singletonList(bulkActivateDeactivateRequest));
    }
  }

  @Test
  public void getWholesalePriceListDetailMppOnTest() throws Exception {
    wholesalePriceSkuDetailListRequest.setItemSkus(Collections.singleton(MAIN_ITEM_SKU));
    wholesalePriceSkuDetailListRequest.setItemInfo(Collections.singletonList(
        new ItemInfoDto(MAIN_ITEM_SKU, PICKUP_POINT_CODE,
            CommonUtils.getItemSkuAndPickupPointKey(MAIN_ITEM_SKU, PICKUP_POINT_CODE))));
    Map<String, String> itemSkuAndPPCodeMap = new HashMap<>();
    itemSkuAndPPCodeMap.putIfAbsent(MAIN_ITEM_SKU, PICKUP_POINT_CODE);
    Mockito.when(this.productPricingFeign
        .getWholesalePriceListDetailV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
            wholesalePriceSkuDetailListRequest))
        .thenReturn(new GdnRestListResponse<>(null, null, true, new ArrayList<>(), null, REQUEST_ID));
    List<WholesalePriceSkuResponse> response =
        productPricingOutboundBean.getWholesalePriceList(Collections.singleton(MAIN_ITEM_SKU), itemSkuAndPPCodeMap);
    verify(productPricingFeign).getWholesalePriceListDetailV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        wholesalePriceSkuDetailListRequest);
  }

  @Test
  public void getWholesalePriceListDetailPricingMppOnTest() throws Exception {
    wholesalePriceSkuDetailListRequest.setItemSkus(Collections.singleton(MAIN_ITEM_SKU));
    wholesalePriceSkuDetailListRequest.setItemInfo(Collections.singletonList(
        new ItemInfoDto(MAIN_ITEM_SKU, PICKUP_POINT_CODE,
            CommonUtils.getItemSkuAndPickupPointKey(MAIN_ITEM_SKU, PICKUP_POINT_CODE))));
    Map<String, String> itemSkuAndPPCodeMap = new HashMap<>();
    itemSkuAndPPCodeMap.putIfAbsent(MAIN_ITEM_SKU, PICKUP_POINT_CODE);
    Mockito.when(this.productPricingFeign
        .getWholesalePriceListDetailV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
            wholesalePriceSkuDetailListRequest))
        .thenReturn(new GdnRestListResponse<>(null, null, true, new ArrayList<>(), null, REQUEST_ID));
    List<WholesalePriceSkuResponse> response =
        productPricingOutboundBean.getWholesalePriceList(Collections.singleton(MAIN_ITEM_SKU), itemSkuAndPPCodeMap);
    verify(productPricingFeign).getWholesalePriceListDetailV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        wholesalePriceSkuDetailListRequest);
  }

  @Test
  public void bulkActivateOrDeactivateSkuSwicthOnTest() throws Exception {
    BulkActivateDeactivateRequest bulkActivateDeactivateRequest =
        BulkActivateDeactivateRequest.builder().updateSkuStatus(ACTIVE_STATUS).itemSku(ITEM_SKU).build();
    GdnRestSingleResponse<BulkActivateDeactivateResponse> response =
        new GdnRestSingleResponse<>(null, null, true, new BulkActivateDeactivateResponse(), REQUEST_ID);
    Mockito.when(productPricingFeign.bulkActivateOrDeactivateSkuV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        Arrays.asList(bulkActivateDeactivateRequest))).thenReturn(response);
    BulkActivateDeactivateResponse bulkActivateDeactivateResponse =
        productPricingOutboundBean.bulkActivateOrDeactivateSku(Arrays.asList(bulkActivateDeactivateRequest));
    verify(productPricingFeign).bulkActivateOrDeactivateSkuV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        Arrays.asList(bulkActivateDeactivateRequest));
    Assertions.assertTrue(response.isSuccess());
  }

  @Test
  public void bulkActivateOrDeactivateSkuSwitchOnValueNullTest() throws Exception {
    BulkActivateDeactivateRequest bulkActivateDeactivateRequest =
        BulkActivateDeactivateRequest.builder().updateSkuStatus(ACTIVE_STATUS).itemSku(ITEM_SKU).build();
    GdnRestSingleResponse<BulkActivateDeactivateResponse> response =
        new GdnRestSingleResponse<>(null, null, true, null, REQUEST_ID);
    Mockito.when(productPricingFeign.bulkActivateOrDeactivateSkuV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        Arrays.asList(bulkActivateDeactivateRequest))).thenReturn(response);
    productPricingOutboundBean.bulkActivateOrDeactivateSku(Arrays.asList(bulkActivateDeactivateRequest));
    verify(productPricingFeign).bulkActivateOrDeactivateSkuV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        Arrays.asList(bulkActivateDeactivateRequest));
    Assertions.assertTrue(response.isSuccess());
  }

  @Test
  public void bulkUpdateWholesalePriceNullTest() throws Exception {
    WholesalePriceBulkUpdateRequest wholesalePriceBulkUpdateRequest = new WholesalePriceBulkUpdateRequest();
    WholesalePriceBulkUpdateResponse wholesalePriceBulkUpdateResponse = new WholesalePriceBulkUpdateResponse();
    WholeSalePriceSkuStatusDto wholeSalePriceSkuStatusDto = new WholeSalePriceSkuStatusDto();
    wholeSalePriceSkuStatusDto.setItemSku(ITEM_SKU);
    wholeSalePriceSkuStatusDto.setSkuStatus(ACTIVE_STATUS);
    wholesalePriceBulkUpdateResponse.setWholesalePriceSkuStatus(Collections.singletonList(wholeSalePriceSkuStatusDto));
    GdnRestSingleResponse<WholesalePriceBulkUpdateResponse> response =
        new GdnRestSingleResponse<>(null, null, true, null, REQUEST_ID);
    Mockito.when(productPricingFeign.bulkUpdateV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
        Collections.singletonList(wholesalePriceBulkUpdateRequest))).thenReturn(response);
    WholesalePriceBulkUpdateResponse priceBulkUpdateResponse = productPricingOutboundBean
        .bulkUpdateWholesalePrice(Collections.singletonList(wholesalePriceBulkUpdateRequest), MERCHANT_CODE);
    verify(productPricingFeign).bulkUpdateV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
        Collections.singletonList(wholesalePriceBulkUpdateRequest));
    Assertions.assertTrue(response.isSuccess());
  }

  @Test
  public void bulkUpdateWholesalePriceSwitchOnListNotEmptyTest() throws Exception {
    WholesalePriceBulkUpdateRequest wholesalePriceBulkUpdateRequest = new WholesalePriceBulkUpdateRequest();
    WholesalePriceBulkUpdateResponse wholesalePriceBulkUpdateResponse = new WholesalePriceBulkUpdateResponse();
    WholeSalePriceSkuStatusDto wholeSalePriceSkuStatusDto = new WholeSalePriceSkuStatusDto();
    wholeSalePriceSkuStatusDto.setItemSku(ITEM_SKU);
    wholeSalePriceSkuStatusDto.setSkuStatus(ACTIVE_STATUS);
    wholesalePriceBulkUpdateResponse.setWholesalePriceSkuStatus(Collections.singletonList(wholeSalePriceSkuStatusDto));
    GdnRestSingleResponse<WholesalePriceBulkUpdateResponse> response =
        new GdnRestSingleResponse<>(null, null, true, wholesalePriceBulkUpdateResponse, REQUEST_ID);
    Mockito.when(productPricingFeign.bulkUpdateV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
        Collections.singletonList(wholesalePriceBulkUpdateRequest))).thenReturn(response);
    WholesalePriceBulkUpdateResponse priceBulkUpdateResponse = productPricingOutboundBean
        .bulkUpdateWholesalePrice(Collections.singletonList(wholesalePriceBulkUpdateRequest), MERCHANT_CODE);
    verify(productPricingFeign).bulkUpdateV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
        Collections.singletonList(wholesalePriceBulkUpdateRequest));
    Assertions.assertTrue(response.isSuccess());
    assertEquals(ACTIVE_STATUS, priceBulkUpdateResponse.getWholesalePriceSkuStatusMap().get(ITEM_SKU));
  }

  @Test
  public void bulkUpdateWholesalePriceSwitchOnMapNotEmptyTest() throws Exception {
    WholesalePriceBulkUpdateRequest wholesalePriceBulkUpdateRequest = new WholesalePriceBulkUpdateRequest();
    WholesalePriceBulkUpdateResponse wholesalePriceBulkUpdateResponse = new WholesalePriceBulkUpdateResponse();
    WholeSalePriceSkuStatusDto wholeSalePriceSkuStatusDto = new WholeSalePriceSkuStatusDto();
    wholeSalePriceSkuStatusDto.setItemSku(ITEM_SKU);
    wholeSalePriceSkuStatusDto.setSkuStatus(ACTIVE_STATUS);
    wholesalePriceBulkUpdateResponse.setWholesalePriceSkuStatus(Collections.singletonList(wholeSalePriceSkuStatusDto));
    Map<String, String> itemSkuAndPPCodeMap = new HashMap<>();
    itemSkuAndPPCodeMap.putIfAbsent(MAIN_ITEM_SKU, PICKUP_POINT_CODE);
    wholesalePriceBulkUpdateResponse.setWholesalePriceSkuStatusMap(itemSkuAndPPCodeMap);
    GdnRestSingleResponse<WholesalePriceBulkUpdateResponse> response =
        new GdnRestSingleResponse<>(null, null, true, wholesalePriceBulkUpdateResponse, REQUEST_ID);
    Mockito.when(productPricingFeign.bulkUpdateV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
        Collections.singletonList(wholesalePriceBulkUpdateRequest))).thenReturn(response);
    productPricingOutboundBean
        .bulkUpdateWholesalePrice(Collections.singletonList(wholesalePriceBulkUpdateRequest), MERCHANT_CODE);
    verify(productPricingFeign).bulkUpdateV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
        Collections.singletonList(wholesalePriceBulkUpdateRequest));
    Assertions.assertTrue(response.isSuccess());
  }

  @Test
  public void bulkUpdateWholesalePriceSwitchOnFailedListNotEmptyTest() throws Exception {
    WholesalePriceBulkUpdateRequest wholesalePriceBulkUpdateRequest = new WholesalePriceBulkUpdateRequest();
    WholesalePriceBulkUpdateResponse wholesalePriceBulkUpdateResponse = new WholesalePriceBulkUpdateResponse();
    FailedItemReasonDto failedItemReasonDto = new FailedItemReasonDto();
    failedItemReasonDto.setItemSku(ITEM_SKU);
    failedItemReasonDto.setFailedReason(ACTIVE_STATUS);
    wholesalePriceBulkUpdateResponse.setFailedItemReason(Collections.singletonList(failedItemReasonDto));
    GdnRestSingleResponse<WholesalePriceBulkUpdateResponse> response =
        new GdnRestSingleResponse<>(null, null, true, wholesalePriceBulkUpdateResponse, REQUEST_ID);
    Mockito.when(productPricingFeign.bulkUpdateV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
        Collections.singletonList(wholesalePriceBulkUpdateRequest))).thenReturn(response);
    WholesalePriceBulkUpdateResponse priceBulkUpdateResponse = productPricingOutboundBean
        .bulkUpdateWholesalePrice(Collections.singletonList(wholesalePriceBulkUpdateRequest), MERCHANT_CODE);
    verify(productPricingFeign).bulkUpdateV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
        Collections.singletonList(wholesalePriceBulkUpdateRequest));
    Assertions.assertTrue(response.isSuccess());
    assertEquals(ACTIVE_STATUS, priceBulkUpdateResponse.getFailedItemSkuToFailedReasonMap().get(ITEM_SKU));
  }

  @Test
  public void bulkUpdateWholesalePriceSwitchOnFailedMapNotEmptyTest() throws Exception {
    WholesalePriceBulkUpdateRequest wholesalePriceBulkUpdateRequest = new WholesalePriceBulkUpdateRequest();
    WholesalePriceBulkUpdateResponse wholesalePriceBulkUpdateResponse = new WholesalePriceBulkUpdateResponse();
    FailedItemReasonDto failedItemReasonDto = new FailedItemReasonDto();
    failedItemReasonDto.setItemSku(ITEM_SKU);
    failedItemReasonDto.setFailedReason(ACTIVE_STATUS);
    wholesalePriceBulkUpdateResponse.setFailedItemReason(Collections.singletonList(failedItemReasonDto));
    Map<String, String> itemSkuAndPPCodeMap = new HashMap<>();
    itemSkuAndPPCodeMap.putIfAbsent(MAIN_ITEM_SKU, PICKUP_POINT_CODE);
    wholesalePriceBulkUpdateResponse.setFailedItemSkuToFailedReasonMap(itemSkuAndPPCodeMap);
    GdnRestSingleResponse<WholesalePriceBulkUpdateResponse> response =
        new GdnRestSingleResponse<>(null, null, true, wholesalePriceBulkUpdateResponse, REQUEST_ID);
    Mockito.when(productPricingFeign.bulkUpdateV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
        Collections.singletonList(wholesalePriceBulkUpdateRequest))).thenReturn(response);
    productPricingOutboundBean
        .bulkUpdateWholesalePrice(Collections.singletonList(wholesalePriceBulkUpdateRequest), MERCHANT_CODE);
    verify(productPricingFeign).bulkUpdateV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
        Collections.singletonList(wholesalePriceBulkUpdateRequest));
    Assertions.assertTrue(response.isSuccess());
  }

  @Test
  public void bulkUpdateWholesalePriceSwitchOnTest() throws Exception {
    WholesalePriceBulkUpdateRequest wholesalePriceBulkUpdateRequest = new WholesalePriceBulkUpdateRequest();
    GdnRestSingleResponse<WholesalePriceBulkUpdateResponse> response =
        new GdnRestSingleResponse<>(null, null, true, new WholesalePriceBulkUpdateResponse(), REQUEST_ID);
    Mockito.when(productPricingFeign.bulkUpdateV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
        Arrays.asList(wholesalePriceBulkUpdateRequest))).thenReturn(response);
    WholesalePriceBulkUpdateResponse wholesalePriceBulkUpdateResponse = productPricingOutboundBean
        .bulkUpdateWholesalePrice(Arrays.asList(wholesalePriceBulkUpdateRequest), MERCHANT_CODE);
    verify(productPricingFeign).bulkUpdateV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
        Arrays.asList(wholesalePriceBulkUpdateRequest));
    Assertions.assertTrue(response.isSuccess());
  }

  @Test
  public void bulkUpdateWholesalePriceV2Test() throws Exception {
    WholesalePriceBulkUpdateRequest wholesalePriceBulkUpdateRequest = new WholesalePriceBulkUpdateRequest();
    GdnRestSingleResponse<WholesalePriceBulkUpdateResponse> response =
        new GdnRestSingleResponse<>(null, null, true, new WholesalePriceBulkUpdateResponse(), REQUEST_ID);
    Mockito.when(productPricingFeign.bulkUpdateV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
        Arrays.asList(wholesalePriceBulkUpdateRequest))).thenReturn(response);
    WholesalePriceBulkUpdateResponse wholesalePriceBulkUpdateResponse = productPricingOutboundBean.bulkUpdateWholesalePriceV2(
        Arrays.asList(wholesalePriceBulkUpdateRequest), MERCHANT_CODE, USERNAME);
    verify(productPricingFeign).bulkUpdateV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
        Arrays.asList(wholesalePriceBulkUpdateRequest));
    Assertions.assertTrue(response.isSuccess());
  }

  @Test
  public void bulkUpdateWholesalePriceV2ExceptionTest() throws Exception {
    WholesalePriceBulkUpdateRequest wholesalePriceBulkUpdateRequest = new WholesalePriceBulkUpdateRequest();
    GdnRestSingleResponse<WholesalePriceBulkUpdateResponse> response =
        new GdnRestSingleResponse<>(null, null, false, new WholesalePriceBulkUpdateResponse(), REQUEST_ID);
    Mockito.when(productPricingFeign.bulkUpdateV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
        Arrays.asList(wholesalePriceBulkUpdateRequest))).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        WholesalePriceBulkUpdateResponse wholesalePriceBulkUpdateResponse = productPricingOutboundBean.bulkUpdateWholesalePriceV2(
            Arrays.asList(wholesalePriceBulkUpdateRequest), MERCHANT_CODE, USERNAME);
      });
    } finally {
      verify(productPricingFeign).bulkUpdateV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, MERCHANT_CODE,
          Arrays.asList(wholesalePriceBulkUpdateRequest));
      Assertions.assertFalse(response.isSuccess());
    }
  }

  @Test
  public void testGetPromoBundlingSummaryPricingMppResponse () throws Exception {
    GdnRestListResponse<PromoBundlingSummaryResponse> response = null;
    PromoBundlingPricingSummaryRequest promoBundlingPricingSummaryRequest = new PromoBundlingPricingSummaryRequest();
    response =
        new GdnRestListResponse<>(Collections.singletonList(promoBundlingSummaryResponse), new PageMetaData(1, 0, 1), "requestId");
    when(productPricingFeign
        .filterPromoBundlingV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PAGE_NUMBER, PAGE_SIZE, START_DATE,
            SolrConstants.ASC, promoBundlingPricingSummaryRequest)).thenReturn(response);
    productPricingOutboundBean.getPromoBundlingSummaryResponse(promoBundlingPricingSummaryRequest);
    verify(productPricingFeign)
        .filterPromoBundlingV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PAGE_NUMBER, PAGE_SIZE, START_DATE,
            SolrConstants.ASC, promoBundlingPricingSummaryRequest);
    Assertions.assertTrue(response.isSuccess());
    Assertions.assertFalse(response.getContent().isEmpty());
  }

  @Test
  public void testGetPromoBundlingSummaryMppResponse () throws Exception {
    GdnRestListResponse<PromoBundlingSummaryResponse> response = null;
    PromoBundlingPricingSummaryRequest promoBundlingPricingSummaryRequest = new PromoBundlingPricingSummaryRequest();
    response =
        new GdnRestListResponse<>(Collections.singletonList(promoBundlingSummaryResponse), new PageMetaData(1, 0, 1), "requestId");
    when(productPricingFeign
        .filterPromoBundlingV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PAGE_NUMBER, PAGE_SIZE, START_DATE,
            SolrConstants.ASC, promoBundlingPricingSummaryRequest)).thenReturn(response);
      productPricingOutboundBean.getPromoBundlingSummaryResponse(promoBundlingPricingSummaryRequest);
      verify(productPricingFeign)
          .filterPromoBundlingV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, PAGE_NUMBER, PAGE_SIZE,
              START_DATE, SolrConstants.ASC, promoBundlingPricingSummaryRequest);
      Assertions.assertTrue(response.isSuccess());
      Assertions.assertFalse(response.getContent().isEmpty());
  }

  @Test
  public void testGetPromoBundlingSummaryFreeSampleCheckNewFlowResponse() throws Exception {
    ReflectionTestUtils.setField(productPricingOutboundBean, "freeSampleCheckNewFlow", true);
    PromoBundlingPricingSummaryRequest promoBundlingPricingSummaryRequest = new PromoBundlingPricingSummaryRequest();
    promoBundlingPricingSummaryRequest.setProductSku(PRODUCT_SKU);
    Mockito.when(productPricingFeign.freeSampleParticipationCheck(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        PRODUCT_SKU)).thenReturn(new GdnRestSingleResponse<>());
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productPricingOutboundBean.getPromoBundlingSummaryResponse(promoBundlingPricingSummaryRequest);
      });
    } finally {
      verify(productPricingFeign).freeSampleParticipationCheck(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
          PRODUCT_SKU);
    }
  }

  @Test
  public void testGetPromoBundlingSummaryFreeSampleCheckNewFlowResponseNullResponse() throws Exception {
    ReflectionTestUtils.setField(productPricingOutboundBean, "freeSampleCheckNewFlow", true);
    PromoBundlingPricingSummaryRequest promoBundlingPricingSummaryRequest = new PromoBundlingPricingSummaryRequest();
    promoBundlingPricingSummaryRequest.setProductSku(PRODUCT_SKU);
    Mockito.when(productPricingFeign.freeSampleParticipationCheck(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        PRODUCT_SKU)).thenReturn(new GdnRestSingleResponse<>(null, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productPricingOutboundBean.getPromoBundlingSummaryResponse(promoBundlingPricingSummaryRequest);
      });
    } finally {
      verify(productPricingFeign).freeSampleParticipationCheck(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
          PRODUCT_SKU);
    }
  }

  @Test
  public void testGetPromoBundlingSummaryFreeSampleCheckNewFlowResponseTrueResponse() throws Exception {
    ReflectionTestUtils.setField(productPricingOutboundBean, "freeSampleCheckNewFlow", true);
    PromoBundlingPricingSummaryRequest promoBundlingPricingSummaryRequest = new PromoBundlingPricingSummaryRequest();
    promoBundlingPricingSummaryRequest.setProductSku(PRODUCT_SKU);
    FreeSampleParticipationResponse freeSampleParticipationResponse = new FreeSampleParticipationResponse();
    freeSampleParticipationResponse.setFreeSampleAvailable(true);
    Mockito.when(productPricingFeign.freeSampleParticipationCheck(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        PRODUCT_SKU)).thenReturn(new GdnRestSingleResponse<>(freeSampleParticipationResponse, REQUEST_ID));
    List<PromoBundlingSummaryResponse> response =
        productPricingOutboundBean.getPromoBundlingSummaryResponse(promoBundlingPricingSummaryRequest);
    verify(productPricingFeign).freeSampleParticipationCheck(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        PRODUCT_SKU);
    Assertions.assertTrue(response.stream().anyMatch(
        promoBundlingSummaryResponse -> com.gdn.partners.product.pricing.model.enums.PromoBundlingType.FREE_SAMPLE.name()
            .equals(promoBundlingSummaryResponse.getPromoBundlingType())));
  }

  @Test
  public void testGetPromoBundlingSummaryFreeSampleCheckNewFlowResponseFalseResponse() throws Exception {
    ReflectionTestUtils.setField(productPricingOutboundBean, "freeSampleCheckNewFlow", true);
    PromoBundlingPricingSummaryRequest promoBundlingPricingSummaryRequest = new PromoBundlingPricingSummaryRequest();
    promoBundlingPricingSummaryRequest.setProductSku(PRODUCT_SKU);
    FreeSampleParticipationResponse freeSampleParticipationResponse = new FreeSampleParticipationResponse();
    freeSampleParticipationResponse.setFreeSampleAvailable(false);
    Mockito.when(productPricingFeign.freeSampleParticipationCheck(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        PRODUCT_SKU)).thenReturn(new GdnRestSingleResponse<>(freeSampleParticipationResponse, REQUEST_ID));
    List<PromoBundlingSummaryResponse> response =
        productPricingOutboundBean.getPromoBundlingSummaryResponse(promoBundlingPricingSummaryRequest);
    verify(productPricingFeign).freeSampleParticipationCheck(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        PRODUCT_SKU);
    Assertions.assertTrue(response.stream().noneMatch(
        promoBundlingSummaryResponse -> com.gdn.partners.product.pricing.model.enums.PromoBundlingType.FREE_SAMPLE.name()
            .equals(promoBundlingSummaryResponse.getPromoBundlingType())));
  }

  @Test
  public void getWholesalePriceByItemSkuAndPickupPointCodeSwitchOnTest() throws Exception {
    Mockito.when(this.productPricingFeign.getWholesalePriceListDetailV2(eq(STORE_ID), eq(CHANNEL_ID),
        eq(CLIENT_ID), eq(REQUEST_ID), eq(USERNAME),
        any(WholesalePriceSkuDetailListRequest.class)))
        .thenReturn(new GdnRestListResponse<>(null, null, true, new ArrayList<>(), null, REQUEST_ID));
    productPricingOutboundBean.getWholesalePriceByItemSkuAndPickupPointCode(
        Collections.singletonList(itemInfoDto));
    Mockito.verify(
        this.productPricingFeign).getWholesalePriceListDetailV2(eq(STORE_ID), eq(CHANNEL_ID),
        eq(CLIENT_ID), eq(REQUEST_ID), eq(USERNAME), any(WholesalePriceSkuDetailListRequest.class));
  }

  @Test
  public void getWholesalePriceByItemSkuAndPickupPointCode_exceptionTest() throws Exception {
    Mockito.when(this.productPricingFeign.getWholesalePriceListDetailV2(eq(STORE_ID), eq(CHANNEL_ID),
        eq(CLIENT_ID), eq(REQUEST_ID), eq(USERNAME),
        any(WholesalePriceSkuDetailListRequest.class)))
      .thenReturn(new GdnRestListResponse<>(null, null, false, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productPricingOutboundBean.getWholesalePriceByItemSkuAndPickupPointCode(Collections.singletonList(itemInfoDto));
      });
    } finally {
      Mockito.verify(
        this.productPricingFeign).getWholesalePriceListDetailV2(eq(STORE_ID), eq(CHANNEL_ID),
        eq(CLIENT_ID), eq(REQUEST_ID), eq(USERNAME), any(WholesalePriceSkuDetailListRequest.class));
    }
  }

  @Test
  public void getWholesalePriceListDetailPricingEnableV2TrueTest() throws Exception {
    WholesalePriceSkuDetailListRequest wholesalePriceSkuDetailListRequest = new WholesalePriceSkuDetailListRequest();
    when(productPricingFeign.getWholesalePriceListDetailV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        wholesalePriceSkuDetailListRequest))
        .thenReturn(new GdnRestListResponse<>(null, null, true, null, null, REQUEST_ID));
    List<WholesalePriceSkuResponse> response = productPricingOutboundBean.getWholesalePriceListV2(wholesalePriceSkuDetailListRequest);
    verify(productPricingFeign).getWholesalePriceListDetailV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        wholesalePriceSkuDetailListRequest);
  }

  @Test
  public void setWholesaleActivatedFlagMppTrueTest() throws Exception {
    GdnRestSingleResponse<BulkActivateDeactivateResponse> response =
        new GdnRestSingleResponse<>(null, null, true, new BulkActivateDeactivateResponse(), REQUEST_ID);
    Mockito.when(productPricingFeign.bulkActivateOrDeactivateSkuV2(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID), eq(USERNAME),
        any())).thenReturn(response);
    boolean response1 = productPricingOutboundBean.setWholesaleActivatedFlag(ITEM_SKU, PICKUP_POINT_CODE, ACTIVE_STATUS);
    verify(productPricingFeign).bulkActivateOrDeactivateSkuV2(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID), eq(USERNAME),
        any());
    Assertions.assertTrue(response1);
  }

  @Test
  public void setWholesaleActivatedFlagMppTrueExceptionTest() throws Exception {
    BulkActivateDeactivateResponse bulkActivateDeactivateResponse = new BulkActivateDeactivateResponse();
    FailedItemReasonDto failedItemReasonDto = new FailedItemReasonDto();
    failedItemReasonDto.setItemSku(ITEM_SKU);
    failedItemReasonDto.setFailedReason(REQUEST_ID);
    bulkActivateDeactivateResponse.setFailedItemReason(Collections.singletonList(failedItemReasonDto));
    GdnRestSingleResponse<BulkActivateDeactivateResponse> response =
        new GdnRestSingleResponse<>(null, null, true, bulkActivateDeactivateResponse, REQUEST_ID);
    Mockito.when(productPricingFeign.bulkActivateOrDeactivateSkuV2(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID), eq(USERNAME),
        any())).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productPricingOutboundBean.setWholesaleActivatedFlag(ITEM_SKU, PICKUP_POINT_CODE, ACTIVE_STATUS);
      });
    } finally {
      verify(productPricingFeign).bulkActivateOrDeactivateSkuV2(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID), eq(USERNAME),
          any());
    }
  }

  @Test
  public void setWholesaleActivatedFlagMppTrueExceptionTest1() throws Exception {
    BulkActivateDeactivateResponse bulkActivateDeactivateResponse = new BulkActivateDeactivateResponse();
    FailedItemReasonDto failedItemReasonDto = new FailedItemReasonDto();
    failedItemReasonDto.setItemSku(ITEM_SKU);
    failedItemReasonDto.setFailedReason(REQUEST_ID);
    bulkActivateDeactivateResponse.setFailedItemReason(Collections.singletonList(failedItemReasonDto));
    Map<String, String> itemSkuAndPPCodeMap = new HashMap<>();
    itemSkuAndPPCodeMap.putIfAbsent(MAIN_ITEM_SKU, PICKUP_POINT_CODE);
    bulkActivateDeactivateResponse.setFailedItemSkuToFailedReasonMap(itemSkuAndPPCodeMap);
    GdnRestSingleResponse<BulkActivateDeactivateResponse> response =
        new GdnRestSingleResponse<>(null, null, true, bulkActivateDeactivateResponse, REQUEST_ID);
    Mockito.when(productPricingFeign.bulkActivateOrDeactivateSkuV2(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID), eq(USERNAME),
        any())).thenReturn(response);
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productPricingOutboundBean.setWholesaleActivatedFlag(ITEM_SKU, PICKUP_POINT_CODE, ACTIVE_STATUS);
      });
    } finally {
      verify(productPricingFeign).bulkActivateOrDeactivateSkuV2(eq(STORE_ID), eq(CHANNEL_ID), eq(CLIENT_ID), eq(REQUEST_ID), eq(USERNAME),
          any());
    }
  }

}