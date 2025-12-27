package com.gdn.x.product.outbound.impl;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.product.analytics.web.model.SellerDetailResponse;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.outbound.api.feign.ProductAnalyticsFeign;

public class ProductAnalyticsOutboundTest {

  private static final String STORE_ID = "storeId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String SELLER_CODE = "sellerCode";

  @InjectMocks
  private ProductAnalyticsOutboundImpl productAnalyticsOutbound;

  @Mock
  private ProductAnalyticsFeign productAnalyticsFeign;

  private SellerDetailResponse sellerDetailResponse;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);

    sellerDetailResponse = new SellerDetailResponse();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(productAnalyticsFeign);
  }

  @Test
  public void checkGoodSellerTest() {
    Mockito.when(productAnalyticsFeign.findByMerchantAndCategory(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID_X_PRODUCT, REQUEST_ID, USERNAME, SELLER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, sellerDetailResponse, REQUEST_ID));

    productAnalyticsOutbound.checkGoodSeller(STORE_ID, REQUEST_ID, Constants.DEFAULT_CLIENT_ID_X_PRODUCT,
        Constants.DEFAULT_CHANNEL_ID, USERNAME, SELLER_CODE);

    Mockito.verify(productAnalyticsFeign)
        .findByMerchantAndCategory(STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID_X_PRODUCT,
            REQUEST_ID, USERNAME, SELLER_CODE);
  }

  @Test
  public void checkGoodSellerSuccessFalseTest() {
    Mockito.when(productAnalyticsFeign.findByMerchantAndCategory(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID_X_PRODUCT, REQUEST_ID, USERNAME, SELLER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, sellerDetailResponse, REQUEST_ID));

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->  productAnalyticsOutbound.checkGoodSeller(STORE_ID, REQUEST_ID, Constants.DEFAULT_CLIENT_ID_X_PRODUCT, Constants.DEFAULT_CHANNEL_ID, USERNAME, SELLER_CODE));
    }
    finally {
      Mockito.verify(productAnalyticsFeign)
          .findByMerchantAndCategory(STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID_X_PRODUCT,
              REQUEST_ID, USERNAME, SELLER_CODE);
    }
  }

  @Test
  public void checkGoodSellerValueNullTest() {
    Mockito.when(productAnalyticsFeign.findByMerchantAndCategory(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID_X_PRODUCT, REQUEST_ID, USERNAME, SELLER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, true, null, REQUEST_ID));

    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () ->  productAnalyticsOutbound.checkGoodSeller(STORE_ID, REQUEST_ID, Constants.DEFAULT_CLIENT_ID_X_PRODUCT, Constants.DEFAULT_CHANNEL_ID, USERNAME, SELLER_CODE));
    }
    finally {
      Mockito.verify(productAnalyticsFeign)
          .findByMerchantAndCategory(STORE_ID, Constants.DEFAULT_CHANNEL_ID, Constants.DEFAULT_CLIENT_ID_X_PRODUCT,
              REQUEST_ID, USERNAME, SELLER_CODE);
    }
  }

}
