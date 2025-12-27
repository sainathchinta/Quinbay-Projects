package com.gdn.mta.bulk.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.dto.product.DormantSellerProductUpdateRequest;
import com.gdn.mta.bulk.service.DormantSellerService;
import com.gdn.mta.bulk.service.DormantSellerServiceWrapper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.Arrays;

import static org.hamcrest.Matchers.is;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

public class SellerControllerTest {
  private static final String STORE_ID = "storeId";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String REQUEST_ID = "requestId";
  private static final String USERNAME = "username";
  private static final String PAGE_SIZE = "pageSize";
  private static final String BATCH_SIZE = "batchSize";
  private static final String DAYS = "days";
  private static final String PROCESS_TYPE = "processType";

  private ObjectMapper objectMapper = new ObjectMapper();

  @Mock
  private DormantSellerService dormantSellerService;

  @Mock
  private DormantSellerServiceWrapper dormantSellerServiceWrapper;

  @Autowired
  private MockMvc mockMvc;

  @InjectMocks
  private SellerController sellerController;

  @BeforeEach
  public void init() {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(sellerController).build();
  }

  @AfterEach
  public void teardown() {
    verifyNoMoreInteractions(dormantSellerService);
  }

  @Test
  public void processPendingDormantSellerEventTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
      get(SellerController.BASE_PATH + SellerController.PROCESS_PENDING_SELLER)
        .param(STORE_ID, STORE_ID)
        .param(CHANNEL_ID, CHANNEL_ID)
        .param(CLIENT_ID, CLIENT_ID)
        .param(REQUEST_ID, REQUEST_ID)
        .param(USERNAME, USERNAME)
        .param(PROCESS_TYPE, PROCESS_TYPE)
        .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    Mockito.verify(this.dormantSellerServiceWrapper).processPendingDormantSellerEvent(STORE_ID,
      REQUEST_ID, USERNAME, PROCESS_TYPE);
  }

  @Test
  public void processPendingDormantSellerEvent_exceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(this.dormantSellerServiceWrapper)
      .processPendingDormantSellerEvent(STORE_ID, REQUEST_ID, USERNAME, PROCESS_TYPE);
    MockHttpServletRequestBuilder requestBuilder =
      get(SellerController.BASE_PATH + SellerController.PROCESS_PENDING_SELLER).param(STORE_ID, STORE_ID)
        .param(CHANNEL_ID, CHANNEL_ID).param(CLIENT_ID, CLIENT_ID).param(REQUEST_ID, REQUEST_ID).param(USERNAME, USERNAME)
        .param(PROCESS_TYPE, PROCESS_TYPE)
        .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(false)));
    Mockito.verify(this.dormantSellerServiceWrapper).processPendingDormantSellerEvent(STORE_ID,
      REQUEST_ID, USERNAME, PROCESS_TYPE);
  }

  @Test
  public void updateViewConfigForItemsTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
      get(SellerController.BASE_PATH + SellerController.UPDATE_VIEWCONFIG_ITEM_SKU)
        .param(STORE_ID, STORE_ID)
        .param(CHANNEL_ID, CHANNEL_ID)
        .param(CLIENT_ID, CLIENT_ID)
        .param(REQUEST_ID, REQUEST_ID)
        .param(USERNAME, USERNAME)
        .param(PROCESS_TYPE, PROCESS_TYPE)
        .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    Mockito.verify(this.dormantSellerService).updateViewConfigForItemsOfDormantSeller(STORE_ID,
      REQUEST_ID, USERNAME, PROCESS_TYPE);
  }

  @Test
  public void updateDormantSellerStatusTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(SellerController.BASE_PATH + SellerController.UPDATE_SELLER_STATUS).param(STORE_ID, STORE_ID)
            .param(CHANNEL_ID, CHANNEL_ID).param(CLIENT_ID, CLIENT_ID).param(REQUEST_ID, REQUEST_ID)
            .param(USERNAME, USERNAME).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    Mockito.verify(this.dormantSellerService).updateDormantSellerStatus(STORE_ID, REQUEST_ID, USERNAME);
  }

  @Test
  public void updateDormantSellerStatusExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(this.dormantSellerService)
        .updateDormantSellerStatus(STORE_ID, REQUEST_ID, USERNAME);
    MockHttpServletRequestBuilder requestBuilder =
        get(SellerController.BASE_PATH + SellerController.UPDATE_SELLER_STATUS).param(STORE_ID, STORE_ID)
            .param(CHANNEL_ID, CHANNEL_ID).param(CLIENT_ID, CLIENT_ID).param(REQUEST_ID, REQUEST_ID)
            .param(USERNAME, USERNAME).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(false)));
    Mockito.verify(this.dormantSellerService).updateDormantSellerStatus(STORE_ID, REQUEST_ID, USERNAME);
  }

  @Test
  public void deleteDormantSellerProductTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(SellerController.BASE_PATH + SellerController.DELETE_DORMANT_SELLER_PRODUCT).param(STORE_ID, STORE_ID)
            .param(CHANNEL_ID, CHANNEL_ID).param(CLIENT_ID, CLIENT_ID).param(REQUEST_ID, REQUEST_ID)
            .param(USERNAME, USERNAME).param(PAGE_SIZE, String.valueOf(10)).param(BATCH_SIZE, String.valueOf(100))
            .param(DAYS, String.valueOf(30)).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    Mockito.verify(this.dormantSellerService).deleteDormantSellerProduct(STORE_ID, REQUEST_ID, USERNAME, 10, 100, 30);
  }

  @Test
  public void deleteDormantSellerProductExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(this.dormantSellerService)
        .deleteDormantSellerProduct(STORE_ID, REQUEST_ID, USERNAME, 10, 100, 30);
    MockHttpServletRequestBuilder requestBuilder =
        get(SellerController.BASE_PATH + SellerController.DELETE_DORMANT_SELLER_PRODUCT).param(STORE_ID, STORE_ID)
            .param(CHANNEL_ID, CHANNEL_ID).param(CLIENT_ID, CLIENT_ID).param(REQUEST_ID, REQUEST_ID)
            .param(USERNAME, USERNAME).param(PAGE_SIZE, String.valueOf(10)).param(BATCH_SIZE, String.valueOf(100))
            .param(DAYS, String.valueOf(30)).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(false)));
    Mockito.verify(this.dormantSellerService).deleteDormantSellerProduct(STORE_ID, REQUEST_ID, USERNAME, 10, 100, 30);
  }

  @Test
  public void overrideDormantSellerProductStatusTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
      post(SellerController.BASE_PATH + SellerController.OVERRIDE_DORMANT_SELLER_PRODUCT_STATUS)
        .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
        .content(objectMapper.writeValueAsString(Arrays.asList(new DormantSellerProductUpdateRequest())))
        .param(STORE_ID, STORE_ID)
        .param(CHANNEL_ID, CHANNEL_ID)
        .param(CLIENT_ID, CLIENT_ID)
        .param(REQUEST_ID, REQUEST_ID)
        .param(USERNAME, USERNAME)
        .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    Mockito.verify(this.dormantSellerService)
      .overrideDormantSellerProductStatus(eq(STORE_ID), eq(REQUEST_ID), eq(USERNAME), Mockito.anyList());
  }

  @Test
  public void overrideDormantSellerProductStatus_exceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(this.dormantSellerService)
      .overrideDormantSellerProductStatus(eq(STORE_ID), eq(REQUEST_ID), eq(USERNAME), Mockito.anyList());
    MockHttpServletRequestBuilder requestBuilder =
      post(SellerController.BASE_PATH + SellerController.OVERRIDE_DORMANT_SELLER_PRODUCT_STATUS)
        .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
        .content(objectMapper.writeValueAsString(Arrays.asList(new DormantSellerProductUpdateRequest())))
        .param(STORE_ID, STORE_ID)
        .param(CHANNEL_ID, CHANNEL_ID)
        .param(CLIENT_ID, CLIENT_ID)
        .param(REQUEST_ID, REQUEST_ID)
        .param(USERNAME, USERNAME)
        .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(false)));
    Mockito.verify(this.dormantSellerService)
      .overrideDormantSellerProductStatus(eq(STORE_ID), eq(REQUEST_ID), eq(USERNAME), Mockito.anyList());
  }

  @Test
  public void updateDormantSellerEventTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        get(SellerController.BASE_PATH + SellerController.UPDATE_DORMANT_SELLER_EVENT).param(STORE_ID, STORE_ID)
            .param(CHANNEL_ID, CHANNEL_ID).param(CLIENT_ID, CLIENT_ID).param(REQUEST_ID, REQUEST_ID)
            .param(USERNAME, USERNAME).param("status", "status").param("businessPartnerCodes", "code")
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    Mockito.verify(this.dormantSellerService)
        .updateDormantSellerEvent(STORE_ID, REQUEST_ID, USERNAME, "status", Arrays.asList("code"),
            false);
  }

  @Test
  public void retryDormantSellerProcessTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        put(SellerController.BASE_PATH + SellerController.RETRY_DORMANT_SELLER_PROCESS).accept(
                MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE).param(STORE_ID, STORE_ID)
            .param(CHANNEL_ID, CHANNEL_ID).param(CLIENT_ID, CLIENT_ID).param(REQUEST_ID, REQUEST_ID)
            .param(USERNAME, USERNAME).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    Mockito.verify(this.dormantSellerService).retryDormantSeller(eq(STORE_ID));
  }

  @Test
  public void retryDormantSellerProcess_exceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(this.dormantSellerService).retryDormantSeller(eq(STORE_ID));
    MockHttpServletRequestBuilder requestBuilder =
        put(SellerController.BASE_PATH + SellerController.RETRY_DORMANT_SELLER_PROCESS).accept(
                MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE).param(STORE_ID, STORE_ID)
            .param(CHANNEL_ID, CHANNEL_ID).param(CLIENT_ID, CLIENT_ID).param(REQUEST_ID, REQUEST_ID)
            .param(USERNAME, USERNAME).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(false)));
    Mockito.verify(this.dormantSellerService).retryDormantSeller(eq(STORE_ID));
  }

  @Test
  public void notifyDormantSellerProcessTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        put(SellerController.BASE_PATH + SellerController.NOTIFY_STUCK_DORMANT_PROCESS).accept(
                MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE).param(STORE_ID, STORE_ID)
            .param(CHANNEL_ID, CHANNEL_ID).param(CLIENT_ID, CLIENT_ID).param(REQUEST_ID, REQUEST_ID)
            .param(USERNAME, USERNAME).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(true)));
    Mockito.verify(this.dormantSellerService).notifyStuckDormantProcess(eq(STORE_ID));
  }

  @Test
  public void notifyDormantSellerProcess_exceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(this.dormantSellerService).notifyStuckDormantProcess(eq(STORE_ID));
    MockHttpServletRequestBuilder requestBuilder =
        put(SellerController.BASE_PATH + SellerController.NOTIFY_STUCK_DORMANT_PROCESS).accept(
                MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE).param(STORE_ID, STORE_ID)
            .param(CHANNEL_ID, CHANNEL_ID).param(CLIENT_ID, CLIENT_ID).param(REQUEST_ID, REQUEST_ID)
            .param(USERNAME, USERNAME).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", is(false)));
    Mockito.verify(this.dormantSellerService).notifyStuckDormantProcess(eq(STORE_ID));
  }

}
