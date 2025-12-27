package com.gdn.x.product.outbound.impl;

import com.gdn.x.businesspartner.dto.ProductCounterResponse;

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
import com.gdn.x.businesspartner.dto.ProfileResponse;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.outbound.api.feign.XbpFeign;

public class XbpOutboundImplTest {

  private static final String STORE_ID = "storeId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";

  @Mock
  private XbpFeign xbpFeign;

  @InjectMocks
  private XbpOutboundImpl xbpOutbound;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(xbpFeign);
  }

  @Test
  public void getBusinessPartnerDetailsTest() {
    Mockito.when(xbpFeign.getBusinessPartnerDetails(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID_X_PRODUCT, REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(new ProfileResponse(), REQUEST_ID));
    xbpOutbound.getBusinessPartnerDetails(STORE_ID, REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE);
    Mockito.verify(this.xbpFeign).getBusinessPartnerDetails(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID_X_PRODUCT, REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE);
  }

  @Test
  public void getBusinessPartnerDetailsExceptionTest() {
    Mockito.when(xbpFeign.getBusinessPartnerDetails(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
        Constants.DEFAULT_CLIENT_ID_X_PRODUCT, REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE))
        .thenReturn(new GdnRestSingleResponse<>(null, null, false, new ProfileResponse(), REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> xbpOutbound.getBusinessPartnerDetails(STORE_ID, REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE));
    } finally {
      Mockito.verify(this.xbpFeign).getBusinessPartnerDetails(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
          Constants.DEFAULT_CLIENT_ID_X_PRODUCT, REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  public void verifyBusinessPartnerCodeSucessFalseValueNullTest() {
    Mockito.when(xbpFeign.getBusinessPartnerDetails(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
                    Constants.DEFAULT_CLIENT_ID_X_PRODUCT, REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE))
            .thenReturn(new GdnRestSingleResponse<>(null, null, true, null, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> xbpOutbound.getBusinessPartnerDetails(STORE_ID, REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE));
    } finally {
      Mockito.verify(this.xbpFeign).getBusinessPartnerDetails(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID_X_PRODUCT, REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  public void productCounterIncrementAndGetTest() {
    Mockito.when(xbpFeign.productCounterIncrementAndGet(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID_X_PRODUCT, REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE)).thenReturn(new GdnRestSingleResponse<>(null, null, true, new ProductCounterResponse(), REQUEST_ID));
    xbpOutbound.productCounterIncrementAndGet(STORE_ID, REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE);
    Mockito.verify(xbpFeign).productCounterIncrementAndGet(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID_X_PRODUCT, REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE);
  }

  @Test
  public void productCounterIncrementAndGetSuccessFalseTest() {
    Mockito.when(xbpFeign.productCounterIncrementAndGet(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID_X_PRODUCT, REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE)).thenReturn(new GdnRestSingleResponse<>(null, null, false, new ProductCounterResponse(), REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> xbpOutbound.productCounterIncrementAndGet(STORE_ID, REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE));
    } finally {
      Mockito.verify(xbpFeign).productCounterIncrementAndGet(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID_X_PRODUCT, REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE);
    }
  }

  @Test
  public void productCounterIncrementAndGetValueNullTest() {
    Mockito.when(xbpFeign.productCounterIncrementAndGet(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
            Constants.DEFAULT_CLIENT_ID_X_PRODUCT, REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE)).thenReturn(new GdnRestSingleResponse<>(null, null, true, null, REQUEST_ID));
    try {
      Assertions.assertThrows(ApplicationRuntimeException.class, () -> xbpOutbound.productCounterIncrementAndGet(STORE_ID, REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE));
    } finally {
      Mockito.verify(xbpFeign).productCounterIncrementAndGet(STORE_ID, Constants.DEFAULT_CHANNEL_ID,
              Constants.DEFAULT_CLIENT_ID_X_PRODUCT, REQUEST_ID, USERNAME, BUSINESS_PARTNER_CODE);
    }
  }
}
