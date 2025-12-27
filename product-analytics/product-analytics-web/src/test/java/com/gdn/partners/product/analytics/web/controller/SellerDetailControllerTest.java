package com.gdn.partners.product.analytics.web.controller;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.test.web.servlet.MockMvc;

import com.gdn.partners.product.analytics.model.SellerApiPath;
import com.gdn.partners.product.analytics.service.SellerDetailService;

public class SellerDetailControllerTest {

  private static final String STORE_ID = "storeId";
  private static final String CLIENT_ID = "clientId";
  private static final String CHANNEL_ID = "channelId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String MERCHANT_CODE = "merchant_code";

  private MockMvc mockMvc;

  @InjectMocks
  private SellerDetailController sellerDetailController;

  @Mock
  private SellerDetailService sellerDetailService;

  @BeforeEach
  public void setUp() {
    initMocks(this);
    this.mockMvc = standaloneSetup(this.sellerDetailController).build();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(sellerDetailService);
  }

  @Test
  public void findByMerchantAndCategoryTest() throws Exception {
    this.mockMvc.perform(get(SellerApiPath.BASE_PATH + SellerApiPath.FIND_BY_SELLER_CODE).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USERNAME).param("merchantCode", MERCHANT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", nullValue())).andExpect(jsonPath("$.errorCode", nullValue()))
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(sellerDetailService).findSellerDetailByMerchantCode(MERCHANT_CODE);
  }

  @Test
  public void findByMerchantAndCategoryExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(sellerDetailService).findSellerDetailByMerchantCode(MERCHANT_CODE);
    this.mockMvc.perform(get(SellerApiPath.BASE_PATH + SellerApiPath.FIND_BY_SELLER_CODE).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
        .param("username", USERNAME).param("merchantCode", MERCHANT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(sellerDetailService).findSellerDetailByMerchantCode(MERCHANT_CODE);
  }
}