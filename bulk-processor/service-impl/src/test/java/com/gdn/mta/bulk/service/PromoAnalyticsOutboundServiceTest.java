package com.gdn.mta.bulk.service;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;

import java.util.ArrayList;
import java.util.Arrays;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.mta.bulk.feignConfig.PromoAnalyticsFeign;
import com.gdn.partners.bulk.util.Constant;
import com.gdn.partners.promo.analytics.web.model.request.LowestPriceRecommendationRequest;
import com.gdn.partners.promo.analytics.web.model.response.LowestPriceRecommendationL5Response;
import com.gdn.partners.promo.analytics.web.model.response.LowestPriceRecommendationResponse;

public class PromoAnalyticsOutboundServiceTest {

  @Mock
  private PromoAnalyticsFeign promoAnalyticsFeign;

  @InjectMocks
  private PromoAnalyticsOutboundServiceImpl promoAnalyticsOutboundService;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(promoAnalyticsFeign);
  }

  @Test
  public void getLowestPriceRecommendation() {
    GdnRestSingleResponse<LowestPriceRecommendationResponse> response =
        new GdnRestSingleResponse(new LowestPriceRecommendationResponse(), Constant.REQUEST_ID);
    when(promoAnalyticsFeign
        .getLowestPriceRecommendation(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, new LowestPriceRecommendationRequest())).thenReturn(response);
    promoAnalyticsOutboundService.getLowestPriceRecommendation(new LowestPriceRecommendationRequest());
    verify(promoAnalyticsFeign)
        .getLowestPriceRecommendation(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, new LowestPriceRecommendationRequest());
  }

  @Test
  public void getLowestPriceRecommendation_FalseSuccess() {
    GdnRestSingleResponse<LowestPriceRecommendationResponse> response =
        new GdnRestSingleResponse(new LowestPriceRecommendationResponse(), Constant.REQUEST_ID);
    response.setSuccess(false);
    when(promoAnalyticsFeign
        .getLowestPriceRecommendation(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, new LowestPriceRecommendationRequest())).thenReturn(response);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> promoAnalyticsOutboundService.getLowestPriceRecommendation(
              new LowestPriceRecommendationRequest()));
    } finally {
      verify(promoAnalyticsFeign)
          .getLowestPriceRecommendation(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
              Constant.USER_NAME, new LowestPriceRecommendationRequest());
    }
  }

  @Test
  public void getLowestPriceRecommendationV2() {
        GdnRestListResponse<LowestPriceRecommendationL5Response> response = new GdnRestListResponse();
    response.setSuccess(true);
    when(promoAnalyticsFeign
        .getLowestPriceRecommendationV2(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, new LowestPriceRecommendationRequest())).thenReturn(response);
    promoAnalyticsOutboundService.getLowestPriceRecommendationV2(new LowestPriceRecommendationRequest());
    verify(promoAnalyticsFeign)
        .getLowestPriceRecommendationV2(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, new LowestPriceRecommendationRequest());
  }

  @Test
  public void getLowestPriceRecommendationV2WithResponse() {
    GdnRestListResponse<LowestPriceRecommendationL5Response> response = new GdnRestListResponse();
    response.setSuccess(true);
    response.setContent(Arrays.asList(new LowestPriceRecommendationL5Response()));
    when(promoAnalyticsFeign
        .getLowestPriceRecommendationV2(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, new LowestPriceRecommendationRequest())).thenReturn(response);
    promoAnalyticsOutboundService.getLowestPriceRecommendationV2(new LowestPriceRecommendationRequest());
    verify(promoAnalyticsFeign)
        .getLowestPriceRecommendationV2(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, new LowestPriceRecommendationRequest());
  }

  @Test
  public void getLowestPriceRecommendationV2_FalseSuccess() {
    GdnRestListResponse<LowestPriceRecommendationL5Response> response = new GdnRestListResponse();
    response.setSuccess(false);
    when(promoAnalyticsFeign
        .getLowestPriceRecommendationV2(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
            Constant.USER_NAME, new LowestPriceRecommendationRequest())).thenReturn(response);
    try {
      Assertions.assertThrows(RuntimeException.class,
          () -> promoAnalyticsOutboundService.getLowestPriceRecommendationV2(
              new LowestPriceRecommendationRequest()));
    } finally {
      verify(promoAnalyticsFeign)
          .getLowestPriceRecommendationV2(Constant.STORE_ID, Constant.CHANNEL_ID, Constant.CLIENT_ID, Constant.REQUEST_ID,
              Constant.USER_NAME, new LowestPriceRecommendationRequest());
    }
  }

}
