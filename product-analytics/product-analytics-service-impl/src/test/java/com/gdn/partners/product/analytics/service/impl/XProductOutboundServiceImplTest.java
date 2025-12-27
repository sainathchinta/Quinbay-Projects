package com.gdn.partners.product.analytics.service.impl;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.product.analytics.client.XProduct.feign.XProductFeign;
import com.gdn.partners.product.analytics.client.response.SimpleBooleanResponse;

@ExtendWith(MockitoExtension.class)
class XProductOutboundServiceImplTest {

  private static final String PRODUCT_CODE = "product-code";
  private static final String REQUEST_ID = "request-id";
  private static final String SELLER_CODE = "seller-code";

  @InjectMocks
  XProductOutboundServiceImpl xProductOutboundService;

  @Mock
  XProductFeign xProductFeign;

  private SimpleBooleanResponse simpleBooleanResponse;
  private GdnRestSingleResponse<SimpleBooleanResponse> response;

  @BeforeEach
  public void setup() {
  }

  @AfterEach
  void teardown() {
    Mockito.verifyNoMoreInteractions(xProductFeign);
  }

  @Test
  void isSharedProductTest() {
    simpleBooleanResponse = new SimpleBooleanResponse(true);
    response = new GdnRestSingleResponse<>(simpleBooleanResponse, REQUEST_ID);
    Mockito.when(xProductFeign.checkIfProductIsShared(PRODUCT_CODE, SELLER_CODE)).thenReturn(response);
    boolean result = xProductOutboundService.isSharedProduct(PRODUCT_CODE, SELLER_CODE);
    Mockito.verify(xProductFeign).checkIfProductIsShared(PRODUCT_CODE, SELLER_CODE);
    Assertions.assertTrue(result);
  }

  @Test
  void isSharedProductSuccessFalseTest() {
    response = new GdnRestSingleResponse<>(null, null, false, null, REQUEST_ID);
    Mockito.when(xProductFeign.checkIfProductIsShared(PRODUCT_CODE, SELLER_CODE)).thenReturn(response);
    Assertions.assertTrue(xProductOutboundService.isSharedProduct(PRODUCT_CODE, SELLER_CODE));

  }

  @Test
  void isSharedProductValueNullTest() {
    response = new GdnRestSingleResponse<>(null, REQUEST_ID);
    Mockito.when(xProductFeign.checkIfProductIsShared(PRODUCT_CODE, SELLER_CODE)).thenReturn(response);
    Assertions.assertTrue(xProductOutboundService.isSharedProduct(PRODUCT_CODE, SELLER_CODE));

  }

  @Test
  void isSharedProductBooleanResultNullTest() {
    simpleBooleanResponse = new SimpleBooleanResponse(null);
    response = new GdnRestSingleResponse<>(simpleBooleanResponse, REQUEST_ID);
    Mockito.when(xProductFeign.checkIfProductIsShared(PRODUCT_CODE, SELLER_CODE)).thenReturn(response);
    Assertions.assertTrue(xProductOutboundService.isSharedProduct(PRODUCT_CODE, SELLER_CODE));
  }

}
