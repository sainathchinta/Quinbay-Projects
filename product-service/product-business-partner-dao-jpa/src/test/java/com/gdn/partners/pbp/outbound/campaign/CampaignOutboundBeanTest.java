package com.gdn.partners.pbp.outbound.campaign;


import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.MockitoAnnotations.initMocks;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.slf4j.MDC;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.x.campaign.rest.web.model.dto.ItemInfoStatusDto;
import com.gdn.x.campaign.rest.web.model.request.CampaignPriceRequest;
import com.gdn.x.campaign.rest.web.model.request.CampaignPriceSkuRequest;
import com.gdn.x.campaign.rest.web.model.request.CampaignUpdateDiscountRequest;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceResponse;
import com.gdn.x.campaign.rest.web.model.response.CampaignPriceSkuResponse;
import com.gdn.x.campaign.rest.web.model.response.CampaignUpdateDiscountResponse;

public class CampaignOutboundBeanTest  {
  private static final String STORE_ID = "storeId";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String ITEM_SKU = "ITEM-0001-0000-1111";
  private static final String DEFAULT_CATEGORY_CODE = "CAT-10001";
  public static final double CAMPAIGN_PRICE = 10001.0;
  public static final double MAX_ALLOWED_PRICE = 10001.0;
  public static final double MIN_ALLOWED_PRICE = 2000.0;
  private CampaignPriceRequest campaignPriceRequest = new CampaignPriceRequest();
  private CampaignPriceResponse campaignPriceResponse = new CampaignPriceResponse();

  @InjectMocks
  private CampaignOutboundBean campaignOutboundBean;

  @Mock
  private XCampaignFeign xCampaignFeign;

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    MDC.put("storeId", STORE_ID);
    MDC.put("channelId", CHANNEL_ID);
    MDC.put("clientId", CLIENT_ID);
    MDC.put("requestId", REQUEST_ID);
    MDC.put("username", USERNAME);

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
  public void getCampaignPriceInfoV2Test() throws Exception {
    Mockito.when(xCampaignFeign
            .getCampaignPriceInfoV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, campaignPriceRequest))
        .thenReturn(new GdnRestSingleResponse(campaignPriceResponse, REQUEST_ID));
    CampaignPriceResponse response = campaignOutboundBean.getCampaignPriceInfoV2(campaignPriceRequest);
    Mockito.verify(xCampaignFeign)
        .getCampaignPriceInfoV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, campaignPriceRequest);
    Assertions.assertEquals(CAMPAIGN_PRICE, response.getItemSkuToPriceResponse().get(ITEM_SKU).getCampaignPrice(), 0);
    Assertions.assertEquals(MIN_ALLOWED_PRICE, response.getItemSkuToPriceResponse().get(ITEM_SKU).getMinAllowedPrice(), 0);
    Assertions.assertEquals(MAX_ALLOWED_PRICE, response.getItemSkuToPriceResponse().get(ITEM_SKU).getMaxAllowedPrice(), 0);
    Assertions.assertTrue(response.getItemSkuToPriceResponse().get(ITEM_SKU).isRegistered());
    Assertions.assertTrue(response.getItemSkuToPriceResponse().get(ITEM_SKU).isLive());
  }

  @Test
  public void getCampaignPriceInfoV2ExceptionTest() throws Exception {
    Mockito.when(xCampaignFeign
            .getCampaignPriceInfoV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, campaignPriceRequest))
        .thenReturn(new GdnRestSingleResponse(StringUtils.EMPTY, StringUtils.EMPTY, false,null,
            REQUEST_ID));
    try {
      CampaignPriceResponse response = campaignOutboundBean.getCampaignPriceInfoV2(campaignPriceRequest);
    } catch (Exception e) {
    } finally {
      Mockito.verify(xCampaignFeign)
          .getCampaignPriceInfoV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, campaignPriceRequest);
    }
  }

  @Test
  public void validateAndUpdateDiscountPriceMPPTrueTest() throws Exception {
    Mockito.when(xCampaignFeign.updateCampaignDiscountV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, true,
            new CampaignUpdateDiscountRequest()))
        .thenReturn(new GdnRestSingleResponse(new CampaignUpdateDiscountResponse(), REQUEST_ID));
    campaignOutboundBean.validateAndUpdateDiscountPrice(true, new CampaignUpdateDiscountRequest());
    Mockito.verify(xCampaignFeign).updateCampaignDiscountV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, true,
        new CampaignUpdateDiscountRequest());
  }

  @Test
  public void validateAndUpdateDiscountPricePricingMPPTrueTest() throws Exception {
    Mockito.when(xCampaignFeign.updateCampaignDiscountV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, true,
        new CampaignUpdateDiscountRequest()))
        .thenReturn(new GdnRestSingleResponse(new CampaignUpdateDiscountResponse(), REQUEST_ID));
    campaignOutboundBean.validateAndUpdateDiscountPrice(true, new CampaignUpdateDiscountRequest());
    Mockito.verify(xCampaignFeign).updateCampaignDiscountV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, true,
        new CampaignUpdateDiscountRequest());
  }

  @Test
  public void validateAndUpdateDiscountPriceExceptionTest() throws Exception {
    Mockito.when(xCampaignFeign.updateCampaignDiscountV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, true,
        new CampaignUpdateDiscountRequest()))
        .thenReturn(new GdnRestSingleResponse(null, null, false, new CampaignUpdateDiscountResponse(), REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        campaignOutboundBean.validateAndUpdateDiscountPrice(true, new CampaignUpdateDiscountRequest());
      });
    } finally {
      Mockito.verify(xCampaignFeign).updateCampaignDiscountV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, true,
          new CampaignUpdateDiscountRequest());
    }
  }

  @Test
  public void validateDiscountPricePricingMppTest() throws Exception {
    Mockito.when(xCampaignFeign.validateDiscountPriceV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, true,
        new CampaignUpdateDiscountRequest()))
        .thenReturn(new GdnRestSingleResponse(new CampaignUpdateDiscountResponse(), REQUEST_ID));
    campaignOutboundBean.validateDiscountPrice(true, new CampaignUpdateDiscountRequest());
    Mockito.verify(xCampaignFeign).validateDiscountPriceV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, true,
        new CampaignUpdateDiscountRequest());
  }

  @Test
  public void validateDiscountPricePricingMppNullTest() throws Exception {
    Mockito.when(xCampaignFeign.validateDiscountPriceV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, true,
        new CampaignUpdateDiscountRequest()))
        .thenReturn(new GdnRestSingleResponse(null, REQUEST_ID));
    campaignOutboundBean.validateDiscountPrice(true, new CampaignUpdateDiscountRequest());
    Mockito.verify(xCampaignFeign).validateDiscountPriceV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, true,
        new CampaignUpdateDiscountRequest());
  }

  @Test
  public void validateDiscountPricePricingMppListNotEmptyTest() throws Exception {
    CampaignUpdateDiscountResponse campaignUpdateDiscountResponse = new CampaignUpdateDiscountResponse();
    ItemInfoStatusDto itemInfoStatusDto = new ItemInfoStatusDto();
    itemInfoStatusDto.setItemSku(ITEM_SKU);
    itemInfoStatusDto.setStatus(ITEM_SKU);
    campaignUpdateDiscountResponse.setItemInfoStatus(Arrays.asList(itemInfoStatusDto,itemInfoStatusDto));
    Map<String, String> itemSkuStatusMap = new HashMap<>();
    itemSkuStatusMap.putIfAbsent(ITEM_SKU, ITEM_SKU);
    campaignUpdateDiscountResponse.setItemSkuStatusMap(itemSkuStatusMap);
    Mockito.when(xCampaignFeign.validateDiscountPriceV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, true,
        new CampaignUpdateDiscountRequest()))
        .thenReturn(new GdnRestSingleResponse(campaignUpdateDiscountResponse, REQUEST_ID));
    campaignOutboundBean.validateDiscountPrice(true, new CampaignUpdateDiscountRequest());
    Mockito.verify(xCampaignFeign).validateDiscountPriceV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, true,
        new CampaignUpdateDiscountRequest());
  }

  @Test
  public void validateDiscountPricePricingMppMapEmptylTest() throws Exception {
    CampaignUpdateDiscountResponse campaignUpdateDiscountResponse = new CampaignUpdateDiscountResponse();
    ItemInfoStatusDto itemInfoStatusDto = new ItemInfoStatusDto();
    itemInfoStatusDto.setItemSku(ITEM_SKU);
    itemInfoStatusDto.setStatus(ITEM_SKU);
    campaignUpdateDiscountResponse.setItemInfoStatus(Arrays.asList(itemInfoStatusDto,itemInfoStatusDto));
    Mockito.when(xCampaignFeign.validateDiscountPriceV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, true,
        new CampaignUpdateDiscountRequest()))
        .thenReturn(new GdnRestSingleResponse(campaignUpdateDiscountResponse, REQUEST_ID));
    campaignOutboundBean.validateDiscountPrice(true, new CampaignUpdateDiscountRequest());
    Mockito.verify(xCampaignFeign).validateDiscountPriceV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, true,
        new CampaignUpdateDiscountRequest());
  }

  @Test
  public void validateDiscountPriceMppTest() throws Exception {
    Mockito.when(xCampaignFeign.validateDiscountPriceV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, true,
        new CampaignUpdateDiscountRequest()))
        .thenReturn(new GdnRestSingleResponse(new CampaignUpdateDiscountResponse(), REQUEST_ID));
    campaignOutboundBean.validateDiscountPrice(true, new CampaignUpdateDiscountRequest());
    Mockito.verify(xCampaignFeign).validateDiscountPriceV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, true,
        new CampaignUpdateDiscountRequest());
  }

  @Test
  public void validateDiscountPriceExceptionTest() throws Exception {
    Mockito.when(xCampaignFeign.validateDiscountPriceV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, true,
        new CampaignUpdateDiscountRequest()))
      .thenReturn(new GdnRestSingleResponse(null, null, false, new CampaignUpdateDiscountResponse(), REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        campaignOutboundBean.validateDiscountPrice(true, new CampaignUpdateDiscountRequest());
      });
    } finally {
      Mockito.verify(xCampaignFeign).validateDiscountPriceV2(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
        true,
        new CampaignUpdateDiscountRequest());
    }
  }
}