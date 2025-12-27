package com.gdn.partners.product.analytics.web.controller;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.partners.product.analytics.model.SellerAnalyticsApiPath;
import com.gdn.partners.product.analytics.service.SellerAnalyticsService;
import com.gdn.partners.product.analytics.web.model.SellerAnalyticsResponse;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.web.servlet.MockMvc;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.verify;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

@ExtendWith(MockitoExtension.class)
class SellerAnalyticsControllerTest {

  private static final String STORE_ID = "storeId";
  private static final String CLIENT_ID = "clientId";
  private static final String CHANNEL_ID = "channelId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String SELLER_CODE = "sellerCode";

  private MockMvc mockMvc;

  @InjectMocks
  private SellerAnalyticsController sellerAnalyticsController;

  @Mock
  private SellerAnalyticsService sellerAnalyticsService;

  @BeforeEach
  public void setUp() {
    this.mockMvc = standaloneSetup(this.sellerAnalyticsController).build();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(sellerAnalyticsService);
  }

  @Test
  void fetchSellerAnalyticsDetailsTest() throws Exception {
    Mockito.when(sellerAnalyticsService.findSellerAnalyticsDetailByStoreIdAndSellerCode(STORE_ID,
        SELLER_CODE)).thenReturn(new SellerAnalyticsResponse());
    this.mockMvc.perform(
            get(SellerAnalyticsApiPath.BASE_PATH + SellerAnalyticsApiPath.GET_SELLER_ANALYTICS_DETAIL,
                SELLER_CODE).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
                .param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(jsonPath("$.errorCode", nullValue()))
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(sellerAnalyticsService).findSellerAnalyticsDetailByStoreIdAndSellerCode(STORE_ID,
        SELLER_CODE);
  }

  @Test
  void fetchSellerAnalyticsDetailsExceptionTest() throws Exception {
    Mockito.doThrow(ApplicationRuntimeException.class).when(sellerAnalyticsService)
        .findSellerAnalyticsDetailByStoreIdAndSellerCode(STORE_ID, SELLER_CODE);
    this.mockMvc.perform(
            get(SellerAnalyticsApiPath.BASE_PATH + SellerAnalyticsApiPath.GET_SELLER_ANALYTICS_DETAIL,
                SELLER_CODE).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
                .param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(sellerAnalyticsService).findSellerAnalyticsDetailByStoreIdAndSellerCode(STORE_ID,
        SELLER_CODE);
  }
}
