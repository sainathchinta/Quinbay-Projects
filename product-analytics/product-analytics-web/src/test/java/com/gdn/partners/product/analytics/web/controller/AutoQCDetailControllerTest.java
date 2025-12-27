package com.gdn.partners.product.analytics.web.controller;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.Matchers.nullValue;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
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

import com.gdn.partners.product.analytics.model.AutoQCApiPath;
import com.gdn.partners.product.analytics.model.enums.ErrorCode;
import com.gdn.partners.product.analytics.service.AutoQCDetailService;
import com.gdn.partners.product.analytics.web.model.AutoQCDetailResponse;

public class AutoQCDetailControllerTest {

  @InjectMocks
  private AutoQCDetailController autoQCDetailController;

  @Mock
  private AutoQCDetailService autoQCDetailService;

  public static final String STORE_ID = "storeId";
  public static final String CLIENT_ID = "clientId";
  public static final String CHANNEL_ID = "channelId";
  public static final String REQUEST_ID = "requestId";
  public static final String USERNAME = "username";
  public static final String MERCHANT_CODE = "merchant_code";
  public static final String CATEGORY_CODE = "category_code";

  private MockMvc mockMvc;
  private AutoQCDetailResponse autoQCDetailResponse = new AutoQCDetailResponse();

  @BeforeEach
  public void setUp() throws Exception {
    initMocks(this);
    this.mockMvc = standaloneSetup(this.autoQCDetailController).build();
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.autoQCDetailService);
  }

  @Test
  public void findByMerchantAndCategoryTest() throws Exception {
    Mockito.when(autoQCDetailService.findByMerchantCodeAndCategoryCode(MERCHANT_CODE, CATEGORY_CODE))
        .thenReturn(autoQCDetailResponse);
    this.mockMvc.perform(
        get(AutoQCApiPath.BASE_PATH + AutoQCApiPath.FIND_BY_MERCHANT_CATEGORY).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USERNAME).param("categoryCode", CATEGORY_CODE).param("merchantCode", MERCHANT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(jsonPath("$.errorCode", nullValue())).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(autoQCDetailService).findByMerchantCodeAndCategoryCode(MERCHANT_CODE, CATEGORY_CODE);
  }

  @Test
  public void findByMerchantAndCategoryExceptionTest() throws Exception {
    doThrow(RuntimeException.class).when(autoQCDetailService)
        .findByMerchantCodeAndCategoryCode(MERCHANT_CODE, CATEGORY_CODE);
    this.mockMvc.perform(
        get(AutoQCApiPath.BASE_PATH + AutoQCApiPath.FIND_BY_MERCHANT_CATEGORY).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USERNAME).param("categoryCode", CATEGORY_CODE).param("merchantCode", MERCHANT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(jsonPath("$.errorCode", equalTo(ErrorCode.FIND_AUTO_QC.getCode())))
        .andExpect(jsonPath("$.success", equalTo(false))).andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(autoQCDetailService).findByMerchantCodeAndCategoryCode(MERCHANT_CODE, CATEGORY_CODE);
  }
}
