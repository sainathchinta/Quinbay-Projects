package com.gdn.mta.bulk.controller;

import static org.hamcrest.CoreMatchers.equalTo;
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
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.service.StoreCopyServiceWrapper;

public class StoreCopyControllerTest {

  private static final String STORE_ID = "store-id";
  private static final String CLIENT_ID = "client-id";
  private static final String REQUEST_ID = "request-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String USERNAME = "username";
  private static final int PAGE = 0;
  private static final int SIZE = 25;
  private static final String CANCEL_REQUEST = "/cancel-request";
  private static final String REQUEST_CODE = "code";
  private static final String ROOT = "/";
  private static final String SELLER_CODE = "RE-0000001";
  private static final String FILE_NAME = "fileName";
  private static final String SCHEDULED_TIME = "scheduledTime";

  private MockMvc mockMvc;
  private ObjectMapper objectMapper = new ObjectMapper();

  @InjectMocks
  private StoreCopyController storeCopyController;

  @Mock
  private StoreCopyServiceWrapper storeCopyServiceWrapper;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = standaloneSetup(this.storeCopyController).build();
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(storeCopyServiceWrapper);
  }

  @Test
  public void downloadTargetSellerTemplate() throws Exception {
    Mockito.when(storeCopyServiceWrapper.downloadTargetSellerTemplate(STORE_ID, SELLER_CODE, USERNAME, REQUEST_ID))
        .thenReturn(FILE_NAME);
    this.mockMvc.perform(
            get(StoreCopyController.BASE_PATH + StoreCopyController.DOWNLOAD_TARGET_SELLER_TEMPLATE,
                SELLER_CODE).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", STORE_ID).param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true))).andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    Mockito.verify(storeCopyServiceWrapper).downloadTargetSellerTemplate(STORE_ID, SELLER_CODE, USERNAME, REQUEST_ID);
  }

  @Test
  public void downloadTargetSellerTemplateException() throws Exception {
    Mockito.when(storeCopyServiceWrapper.downloadTargetSellerTemplate(STORE_ID, SELLER_CODE, USERNAME, REQUEST_ID))
        .thenThrow(Exception.class);
    this.mockMvc.perform(
            get(StoreCopyController.BASE_PATH + StoreCopyController.DOWNLOAD_TARGET_SELLER_TEMPLATE,
                SELLER_CODE).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", STORE_ID).param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false))).andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    Mockito.verify(storeCopyServiceWrapper).downloadTargetSellerTemplate(STORE_ID, SELLER_CODE, USERNAME, REQUEST_ID);
  }
}