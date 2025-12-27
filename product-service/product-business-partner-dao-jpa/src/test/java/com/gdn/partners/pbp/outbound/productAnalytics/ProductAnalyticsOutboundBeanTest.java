package com.gdn.partners.pbp.outbound.productAnalytics;

import static org.mockito.MockitoAnnotations.initMocks;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.product.util.GdnMandatoryRequestParameterUtil;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.outbound.productAnalytics.feign.ProductAnalyticsFeign;
import com.gdn.partners.product.analytics.web.model.AutoQCDetailResponse;
import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;

public class ProductAnalyticsOutboundBeanTest {

  private static final String DEFAULT_CATEGORY_CODE = "categoryCode";
  private static final String DEFAULT_MERCHANT_CODE = "merchantCode";

  @Mock
  private ProductAnalyticsFeign productAnalyticsFeign;

  @InjectMocks
  private ProductAnalyticsOutboundBean productAnalyticsOutbound;

  @BeforeEach
  public void init() throws Exception {
    initMocks(this);
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(productAnalyticsFeign);
  }

  @Test
  public void activateOnNeedCorrectionTest() throws Exception {
    Mockito.when(productAnalyticsFeign.getAutoQCDetails(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), DEFAULT_MERCHANT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true,
            new AutoQCDetailResponse(), GdnMandatoryRequestParameterUtil.getRequestId()));
    productAnalyticsOutbound.getAutoQCDetails(DEFAULT_MERCHANT_CODE, DEFAULT_CATEGORY_CODE);
    Mockito.verify(productAnalyticsFeign).getAutoQCDetails(GdnMandatoryRequestParameterUtil.getStoreId(),
            Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), DEFAULT_MERCHANT_CODE, DEFAULT_CATEGORY_CODE);
  }

  @Test
  public void activateOnNeedCorrectionExceptionTest() throws Exception {
    Mockito.when(productAnalyticsFeign.getAutoQCDetails(GdnMandatoryRequestParameterUtil.getStoreId(),
        Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
        GdnMandatoryRequestParameterUtil.getUsername(), DEFAULT_MERCHANT_CODE, DEFAULT_CATEGORY_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, null));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productAnalyticsOutbound.getAutoQCDetails(DEFAULT_MERCHANT_CODE, DEFAULT_CATEGORY_CODE);
      });
    } finally {
      Mockito.verify(productAnalyticsFeign).getAutoQCDetails(GdnMandatoryRequestParameterUtil.getStoreId(),
          Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
          GdnMandatoryRequestParameterUtil.getUsername(), DEFAULT_MERCHANT_CODE, DEFAULT_CATEGORY_CODE);
    }
  }

  @Test
  public void getSellerDetailTest() {
    Mockito.when(productAnalyticsFeign
        .getSellerDetail(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), DEFAULT_MERCHANT_CODE)).thenReturn(
        new GdnRestSingleResponse<>(null, null, true, new SellerDetailResponse(),
            GdnMandatoryRequestParameterUtil.getRequestId()));
    productAnalyticsOutbound.getSellerDetail(DEFAULT_MERCHANT_CODE);
    Mockito.verify(productAnalyticsFeign)
        .getSellerDetail(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), DEFAULT_MERCHANT_CODE);
  }

  @Test
  public void getSellerDetailExceptionTest() {
    Mockito.when(productAnalyticsFeign
        .getSellerDetail(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
            GdnMandatoryRequestParameterUtil.getUsername(), DEFAULT_MERCHANT_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, null, null));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> {
        productAnalyticsOutbound.getSellerDetail(DEFAULT_MERCHANT_CODE);
      });
    } finally {
      Mockito.verify(productAnalyticsFeign)
          .getSellerDetail(GdnMandatoryRequestParameterUtil.getStoreId(), Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID, GdnMandatoryRequestParameterUtil.getRequestId(),
              GdnMandatoryRequestParameterUtil.getUsername(), DEFAULT_MERCHANT_CODE);
    }
  }

}
