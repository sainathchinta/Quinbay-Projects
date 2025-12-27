package com.gdn.x.product.outbound.impl;


import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.openMocks;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.campaign.dto.ItemInfoStatusDto;
import com.gdn.x.campaign.request.CampaignPriceRequest;
import com.gdn.x.campaign.request.CampaignPriceSkuRequest;
import com.gdn.x.campaign.request.CampaignUpdateDiscountRequest;
import com.gdn.x.campaign.response.CampaignPriceResponse;
import com.gdn.x.campaign.response.CampaignPriceSkuResponse;
import com.gdn.x.campaign.response.CampaignUpdateDiscountResponse;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.outbound.api.feign.XCampaignFeign;

public class XCampaignOutboundImpTest {

  @InjectMocks
  private XCampaignOutboundImpl xCampaignOutbound;

  @Mock
  private XCampaignFeign xCampaignFeign;

  private static final String STORE_ID = "storeId";
  private static final String ITEM_SKU = "ITEM-0001-0000-1111";
  private static final String DEFAULT_CATEGORY_CODE = "CAT-10001";
  public static final double CAMPAIGN_PRICE = 10001.0;
  public static final double MAX_ALLOWED_PRICE = 10001.0;
  public static final double MIN_ALLOWED_PRICE = 2000.0;
  private CampaignPriceRequest campaignPriceRequest = new CampaignPriceRequest();
  private CampaignPriceResponse campaignPriceResponse = new CampaignPriceResponse();


  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    campaignPriceRequest.setCampaignPriceSkuRequestList(
        Arrays.asList(CampaignPriceSkuRequest.builder().itemSku(ITEM_SKU).categoryCode(DEFAULT_CATEGORY_CODE).build()));

    Map<String, CampaignPriceSkuResponse> itemSkuToPriceResponseMap = new HashMap<>();
    CampaignPriceSkuResponse campaignPriceSkuResponse =
        CampaignPriceSkuResponse.builder().campaignPrice(CAMPAIGN_PRICE).maxAllowedPrice(MAX_ALLOWED_PRICE)
            .minAllowedPrice(MIN_ALLOWED_PRICE).live(true).registered(true).build();
    itemSkuToPriceResponseMap.put(ITEM_SKU, campaignPriceSkuResponse);
    campaignPriceResponse.setItemSkuToPriceResponse(itemSkuToPriceResponseMap);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(xCampaignFeign);
  }

  @Test
  public void getCampaignPriceInfoMppOnTest() {
    Mockito.when(
        xCampaignFeign.getCampaignPriceInfoV2(STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, campaignPriceRequest))
        .thenReturn(new GdnRestSingleResponse<>(campaignPriceResponse, Constants.DEFAULT_REQUEST_ID));
    CampaignPriceResponse response = xCampaignOutbound.getCampaignPriceInfo(STORE_ID, campaignPriceRequest);
    Mockito.verify(xCampaignFeign)
        .getCampaignPriceInfoV2(STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, campaignPriceRequest);
    Assertions.assertEquals(CAMPAIGN_PRICE, response.getItemSkuToPriceResponse().get(ITEM_SKU).getCampaignPrice(), 0);
    Assertions.assertEquals(MIN_ALLOWED_PRICE, response.getItemSkuToPriceResponse().get(ITEM_SKU).getMinAllowedPrice(), 0);
    Assertions.assertEquals(MAX_ALLOWED_PRICE, response.getItemSkuToPriceResponse().get(ITEM_SKU).getMaxAllowedPrice(), 0);
    Assertions.assertTrue(response.getItemSkuToPriceResponse().get(ITEM_SKU).isRegistered());
    Assertions.assertTrue(response.getItemSkuToPriceResponse().get(ITEM_SKU).isLive());
  }

  @Test
  public void getCampaignPriceInfoMppOnNullTest() {
    Mockito.when(
        xCampaignFeign.getCampaignPriceInfoV2(STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, campaignPriceRequest))
        .thenReturn(new GdnRestSingleResponse<>(null, Constants.DEFAULT_REQUEST_ID));
    CampaignPriceResponse response = xCampaignOutbound.getCampaignPriceInfo(STORE_ID, campaignPriceRequest);
    Mockito.verify(xCampaignFeign)
        .getCampaignPriceInfoV2(STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, campaignPriceRequest);
  }

  @Test
  public void getCampaignPriceInfoMppOnNonNullTest() {
    campaignPriceResponse.setItemSkuToPriceResponse(new HashMap<>());
    CampaignPriceSkuResponse campaignPriceSkuResponse =
        CampaignPriceSkuResponse.builder().campaignPrice(CAMPAIGN_PRICE).maxAllowedPrice(MAX_ALLOWED_PRICE)
            .minAllowedPrice(MIN_ALLOWED_PRICE).live(true).itemSku(ITEM_SKU).registered(true).build();
    campaignPriceResponse.setItemInfoToPriceResponse(Arrays.asList(campaignPriceSkuResponse,campaignPriceSkuResponse));
    Mockito.when(
        xCampaignFeign.getCampaignPriceInfoV2(STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, campaignPriceRequest))
        .thenReturn(new GdnRestSingleResponse<>(campaignPriceResponse, Constants.DEFAULT_REQUEST_ID));
    CampaignPriceResponse response = xCampaignOutbound.getCampaignPriceInfo(STORE_ID, campaignPriceRequest);
    Mockito.verify(xCampaignFeign)
        .getCampaignPriceInfoV2(STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, campaignPriceRequest);
    Assertions.assertEquals(CAMPAIGN_PRICE, response.getItemSkuToPriceResponse().get(ITEM_SKU).getCampaignPrice(), 0);
    Assertions.assertEquals(MIN_ALLOWED_PRICE, response.getItemSkuToPriceResponse().get(ITEM_SKU).getMinAllowedPrice(), 0);
    Assertions.assertEquals(MAX_ALLOWED_PRICE, response.getItemSkuToPriceResponse().get(ITEM_SKU).getMaxAllowedPrice(), 0);
    Assertions.assertTrue(response.getItemSkuToPriceResponse().get(ITEM_SKU).isRegistered());
    Assertions.assertTrue(response.getItemSkuToPriceResponse().get(ITEM_SKU).isLive());
  }

  @Test
  public void getCampaignPriceInfoMppOnNonNull2Test() {
    CampaignPriceSkuResponse campaignPriceSkuResponse =
        CampaignPriceSkuResponse.builder().campaignPrice(CAMPAIGN_PRICE).maxAllowedPrice(MAX_ALLOWED_PRICE)
            .minAllowedPrice(MIN_ALLOWED_PRICE).itemSku(ITEM_SKU).live(true).registered(true).build();
    campaignPriceResponse.setItemInfoToPriceResponse(Collections.singletonList(campaignPriceSkuResponse));
    Mockito.when(
        xCampaignFeign.getCampaignPriceInfoV2(STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, campaignPriceRequest))
        .thenReturn(new GdnRestSingleResponse<>(campaignPriceResponse, Constants.DEFAULT_REQUEST_ID));
    CampaignPriceResponse response = xCampaignOutbound.getCampaignPriceInfo(STORE_ID, campaignPriceRequest);
    Mockito.verify(xCampaignFeign)
        .getCampaignPriceInfoV2(STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, campaignPriceRequest);
    Assertions.assertEquals(CAMPAIGN_PRICE, response.getItemSkuToPriceResponse().get(ITEM_SKU).getCampaignPrice(), 0);
    Assertions.assertEquals(MIN_ALLOWED_PRICE, response.getItemSkuToPriceResponse().get(ITEM_SKU).getMinAllowedPrice(), 0);
    Assertions.assertEquals(MAX_ALLOWED_PRICE, response.getItemSkuToPriceResponse().get(ITEM_SKU).getMaxAllowedPrice(), 0);
    Assertions.assertTrue(response.getItemSkuToPriceResponse().get(ITEM_SKU).isRegistered());
    Assertions.assertTrue(response.getItemSkuToPriceResponse().get(ITEM_SKU).isLive());
  }

  @Test
  public void getCampaignPriceInfoPricingMppOnTest() {
    Mockito.when(
        xCampaignFeign.getCampaignPriceInfoV2(STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, campaignPriceRequest))
        .thenReturn(new GdnRestSingleResponse<>(campaignPriceResponse, Constants.DEFAULT_REQUEST_ID));
    CampaignPriceResponse response = xCampaignOutbound.getCampaignPriceInfo(STORE_ID, campaignPriceRequest);
    Mockito.verify(xCampaignFeign)
        .getCampaignPriceInfoV2(STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
            Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, campaignPriceRequest);
    Assertions.assertEquals(CAMPAIGN_PRICE, response.getItemSkuToPriceResponse().get(ITEM_SKU).getCampaignPrice(), 0);
    Assertions.assertEquals(MIN_ALLOWED_PRICE, response.getItemSkuToPriceResponse().get(ITEM_SKU).getMinAllowedPrice(), 0);
    Assertions.assertEquals(MAX_ALLOWED_PRICE, response.getItemSkuToPriceResponse().get(ITEM_SKU).getMaxAllowedPrice(), 0);
    Assertions.assertTrue(response.getItemSkuToPriceResponse().get(ITEM_SKU).isRegistered());
    Assertions.assertTrue(response.getItemSkuToPriceResponse().get(ITEM_SKU).isLive());
  }

  @Test
  public void validateAndUpdateDiscountPriceMppOn() {
    CampaignUpdateDiscountRequest campaignUpdateDiscountRequest = new CampaignUpdateDiscountRequest();
    Mockito.when(xCampaignFeign.updateCampaignDiscountV2(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, Boolean.TRUE, campaignUpdateDiscountRequest))
        .thenReturn(
            new GdnRestSingleResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, new CampaignUpdateDiscountResponse(),
                Constants.DEFAULT_REQUEST_ID));
    xCampaignOutbound.validateAndUpdateDiscountPrice(Boolean.TRUE, campaignUpdateDiscountRequest);
    Mockito.verify(xCampaignFeign).updateCampaignDiscountV2(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, Boolean.TRUE, campaignUpdateDiscountRequest);
  }

  @Test
  public void validateAndUpdateDiscountPriceMppNullOn() {
    CampaignUpdateDiscountRequest campaignUpdateDiscountRequest = new CampaignUpdateDiscountRequest();
    Mockito.when(xCampaignFeign.updateCampaignDiscountV2(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, Boolean.TRUE, campaignUpdateDiscountRequest))
        .thenReturn(
            new GdnRestSingleResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, null,
                Constants.DEFAULT_REQUEST_ID));
    xCampaignOutbound.validateAndUpdateDiscountPrice(Boolean.TRUE, campaignUpdateDiscountRequest);
    Mockito.verify(xCampaignFeign).updateCampaignDiscountV2(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, Boolean.TRUE, campaignUpdateDiscountRequest);
  }

  @Test
  public void validateAndUpdateDiscountPriceMppNotNullOn() {
    CampaignUpdateDiscountResponse campaignUpdateDiscountResponse = new CampaignUpdateDiscountResponse();
    ItemInfoStatusDto itemInfoStatusDto = new ItemInfoStatusDto();
    itemInfoStatusDto.setItemSku(ITEM_SKU);
    itemInfoStatusDto.setStatus(ITEM_SKU);
    campaignUpdateDiscountResponse.setItemInfoStatus(Arrays.asList(itemInfoStatusDto, itemInfoStatusDto));
    CampaignUpdateDiscountRequest campaignUpdateDiscountRequest = new CampaignUpdateDiscountRequest();
    Mockito.when(xCampaignFeign.updateCampaignDiscountV2(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, Boolean.TRUE, campaignUpdateDiscountRequest))
        .thenReturn(
            new GdnRestSingleResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, campaignUpdateDiscountResponse,
                Constants.DEFAULT_REQUEST_ID));
    xCampaignOutbound.validateAndUpdateDiscountPrice(Boolean.TRUE, campaignUpdateDiscountRequest);
    Mockito.verify(xCampaignFeign).updateCampaignDiscountV2(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, Boolean.TRUE, campaignUpdateDiscountRequest);
  }

  @Test
  public void validateAndUpdateDiscountPriceMppNotNul2lOn() {
    CampaignUpdateDiscountResponse campaignUpdateDiscountResponse = new CampaignUpdateDiscountResponse();
    Map<String, String> itemSkuStatusMap = new HashMap<>();
    ItemInfoStatusDto itemInfoStatusDto = new ItemInfoStatusDto();
    itemInfoStatusDto.setItemSku(ITEM_SKU);
    itemSkuStatusMap.putIfAbsent(ITEM_SKU, ITEM_SKU);
    campaignUpdateDiscountResponse.setItemSkuStatusMap(itemSkuStatusMap);
    campaignUpdateDiscountResponse.setItemInfoStatus(Collections.singletonList(itemInfoStatusDto));
    CampaignUpdateDiscountRequest campaignUpdateDiscountRequest = new CampaignUpdateDiscountRequest();
    Mockito.when(xCampaignFeign.updateCampaignDiscountV2(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, Boolean.TRUE, campaignUpdateDiscountRequest))
        .thenReturn(
            new GdnRestSingleResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, campaignUpdateDiscountResponse,
                Constants.DEFAULT_REQUEST_ID));
    xCampaignOutbound.validateAndUpdateDiscountPrice(Boolean.TRUE, campaignUpdateDiscountRequest);
    Mockito.verify(xCampaignFeign).updateCampaignDiscountV2(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, Boolean.TRUE, campaignUpdateDiscountRequest);
  }

  @Test
  public void validateAndUpdateDiscountPricePricingMppOn() {
    CampaignUpdateDiscountRequest campaignUpdateDiscountRequest = new CampaignUpdateDiscountRequest();
    Mockito.when(xCampaignFeign.updateCampaignDiscountV2(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, Boolean.TRUE, campaignUpdateDiscountRequest))
        .thenReturn(
            new GdnRestSingleResponse(StringUtils.EMPTY, StringUtils.EMPTY, true, new CampaignUpdateDiscountResponse(),
                Constants.DEFAULT_REQUEST_ID));
    xCampaignOutbound.validateAndUpdateDiscountPrice(Boolean.TRUE, campaignUpdateDiscountRequest);
    Mockito.verify(xCampaignFeign).updateCampaignDiscountV2(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, Boolean.TRUE, campaignUpdateDiscountRequest);
  }

  @Test
  public void getCampaignPriceInfoSuccessFalse() throws Exception {
    Mockito.when(xCampaignFeign.getCampaignPriceInfoV2(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, campaignPriceRequest))
        .thenReturn(new GdnRestSingleResponse(StringUtils.EMPTY, StringUtils.EMPTY, false, campaignPriceResponse,
            Constants.DEFAULT_REQUEST_ID));
    try {
      xCampaignOutbound.getCampaignPriceInfo(Constants.DEFAULT_STORE_ID, campaignPriceRequest);
    } finally {
      Mockito.verify(xCampaignFeign)
          .getCampaignPriceInfoV2(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, campaignPriceRequest);
    }
  }

  @Test
  public void validateAndUpdateDiscountPriceSuccessFalse() throws Exception {
    CampaignUpdateDiscountRequest campaignUpdateDiscountRequest = new CampaignUpdateDiscountRequest();
    Mockito.when(xCampaignFeign.updateCampaignDiscountV2(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, Boolean.TRUE, campaignUpdateDiscountRequest))
        .thenReturn(
            new GdnRestSingleResponse(StringUtils.EMPTY, StringUtils.EMPTY, false, new CampaignUpdateDiscountResponse(),
                Constants.DEFAULT_REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> xCampaignOutbound.validateAndUpdateDiscountPrice(Boolean.TRUE, campaignUpdateDiscountRequest));
    } finally {
      Mockito.verify(xCampaignFeign)
          .updateCampaignDiscountV2(Constants.DEFAULT_STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID,
              Constants.DEFAULT_REQUEST_ID, Constants.DEFAULT_USERNAME, Boolean.TRUE, campaignUpdateDiscountRequest);
    }
  }
}