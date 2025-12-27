package com.gdn.partners.product.analytics.service.impl;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.product.analytics.entity.SellerAnalytics;
import com.gdn.partners.product.analytics.repository.SellerAnalyticsRepository;
import com.gdn.partners.product.analytics.web.model.SellerAnalyticsResponse;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;

@ExtendWith(MockitoExtension.class)
class SellerAnalyticsServiceImplTest {

  private static final String SELLER_CODE = "sellerCode";
  private static final String STORE_ID = "10001";
  public static final String ERROR_MESSAGE =
      "Can not process invalid input data :seller data analytics not found";
  public static final String SELLER_CODE_MUST_NOT_BE_BLANK =
      "Can not process invalid input data :seller code must not be blank";

  @InjectMocks
  private SellerAnalyticsServiceImpl sellerAnalyticsService;

  @Mock
  private SellerAnalyticsRepository sellerAnalyticsRepository;
  private SellerAnalytics sellerAnalytics;

  @BeforeEach
  void setup() {
    sellerAnalytics = new SellerAnalytics();
    sellerAnalytics.setSellerCode(SELLER_CODE);
    sellerAnalytics.setStoreId(STORE_ID);
  }

  @AfterEach
  void teardown() {
    Mockito.verifyNoMoreInteractions(sellerAnalyticsRepository);
  }

  @Test
  void fetchSellerAnalyticsTest() {
    Mockito.when(sellerAnalyticsRepository.findByStoreIdAndSellerCode(STORE_ID, SELLER_CODE))
        .thenReturn(sellerAnalytics);
    SellerAnalyticsResponse response =
        sellerAnalyticsService.findSellerAnalyticsDetailByStoreIdAndSellerCode(STORE_ID,
            SELLER_CODE);
    Assertions.assertEquals(SELLER_CODE, response.getSellerCode());
  }

  @Test
  void fetchSellerAnalyticsNullTest() {
    Mockito.when(sellerAnalyticsRepository.findByStoreIdAndSellerCode(STORE_ID, SELLER_CODE))
        .thenReturn(null);
    try {
      sellerAnalyticsService.findSellerAnalyticsDetailByStoreIdAndSellerCode(STORE_ID, SELLER_CODE);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals(ERROR_MESSAGE, e.getErrorMessage());
    }
  }

  @Test
  void fetchSellerAnalyticsSellerCodeNullTest() {
    try {
      sellerAnalyticsService.findSellerAnalyticsDetailByStoreIdAndSellerCode(STORE_ID, null);
    } catch (ApplicationRuntimeException e) {
      Assertions.assertEquals(SELLER_CODE_MUST_NOT_BE_BLANK, e.getErrorMessage());
    }
  }
}
