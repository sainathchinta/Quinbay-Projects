package com.gdn.mta.bulk.controller;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.mta.bulk.dto.RecatProcessSummaryRequest;
import java.util.ArrayList;

import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.mta.bulk.dto.RecatProductCountResponse;
import com.gdn.mta.bulk.dto.RecatProductSummaryRequest;
import com.gdn.mta.bulk.service.ProductRecatStatusService;
import com.gdn.mta.bulk.service.RecatProcessServiceWrapper;
import com.gdn.partners.bulk.util.RecatConstants;

public class RecatProcessControllerTest {

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
  private static final String RECAT_REQUEST_CODE = "RE-0000001";
  private static final String FILE_NAME = "fileName";
  private static final String SCHEDULED_TIME = "scheduledTime";

  private MockMvc mockMvc;
  private ObjectMapper objectMapper = new ObjectMapper();

  @InjectMocks
  private RecatProcessController recatProcessController;

  @Mock
  private RecatProcessServiceWrapper recatProcessServiceWrapper;

  @Mock
  private ProductRecatStatusService productRecatStatusService;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = standaloneSetup(this.recatProcessController).build();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(recatProcessServiceWrapper);
  }

  @Test
  public void processNewRecatRequestsTest() throws Exception {
    this.mockMvc.perform(get(RecatProcessController.BASE_PATH +
        RecatProcessController.PROCESS_NEW_REQUESTS).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("username", USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.recatProcessServiceWrapper).processNewRecatProcess(STORE_ID);
  }

  @Test
  public void processNewRecatRequestsExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(recatProcessServiceWrapper).processNewRecatProcess(STORE_ID);
    this.mockMvc.perform(get(RecatProcessController.BASE_PATH +
        RecatProcessController.PROCESS_NEW_REQUESTS).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("username", USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.recatProcessServiceWrapper).processNewRecatProcess(STORE_ID);
  }

  @Test
  public void getProductStatusCountsTest() throws Exception {
    Mockito.when(recatProcessServiceWrapper.getProductCountsByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE))
        .thenReturn(new RecatProductCountResponse());
    this.mockMvc.perform(
        get(RecatProcessController.BASE_PATH + RecatProcessController.GET_PRODUCT_COUNTS_BY_REQUEST_CODE,
            RECAT_REQUEST_CODE).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID).param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true))).andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.recatProcessServiceWrapper).getProductCountsByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
  }

  @Test
  public void getProductStatusCountsErrorTest() throws Exception {
    Mockito.when(recatProcessServiceWrapper.getProductCountsByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE))
        .thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(
        get(RecatProcessController.BASE_PATH + RecatProcessController.GET_PRODUCT_COUNTS_BY_REQUEST_CODE,
            RECAT_REQUEST_CODE).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID).param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false))).andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.recatProcessServiceWrapper).getProductCountsByRecatRequestCode(STORE_ID, RECAT_REQUEST_CODE);
  }

  @Test
  public void publishPendingProductsTest() throws Exception {
    this.mockMvc.perform(get(RecatProcessController.BASE_PATH +
        RecatProcessController.PUBLISH_PENDING_PRODUCTS).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("username", USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.recatProcessServiceWrapper).publishPendingProducts(STORE_ID);
  }

  @Test
  public void publishPendingProductsExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(recatProcessServiceWrapper).publishPendingProducts(STORE_ID);
    this.mockMvc.perform(get(RecatProcessController.BASE_PATH +
        RecatProcessController.PUBLISH_PENDING_PRODUCTS).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("username", USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.recatProcessServiceWrapper).publishPendingProducts(STORE_ID);
  }


  @Test
  public void getFailedProductsMailTest() throws Exception {
    this.mockMvc.perform(get(RecatProcessController.BASE_PATH +
        RecatProcessController.RECAT_REQUEST_CODE + RecatProcessController.GET_FAILED_PRODUCTS_MAIL, RECAT_REQUEST_CODE)
        .accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("username", USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.recatProcessServiceWrapper).getFailedProductsMail(STORE_ID, RECAT_REQUEST_CODE, USERNAME, REQUEST_ID);
  }

  @Test
  public void getFailedProductsMailExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(recatProcessServiceWrapper)
        .getFailedProductsMail(STORE_ID, RECAT_REQUEST_CODE, USERNAME, REQUEST_ID);
    this.mockMvc.perform(get(RecatProcessController.BASE_PATH +
            RecatProcessController.RECAT_REQUEST_CODE + RecatProcessController.GET_FAILED_PRODUCTS_MAIL, RECAT_REQUEST_CODE)
        .accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("username", USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.recatProcessServiceWrapper).getFailedProductsMail(STORE_ID, RECAT_REQUEST_CODE, USERNAME, REQUEST_ID);
  }

  @Test
  public void uploadNewRecatRequestTest() throws Exception {
    this.mockMvc.perform(post(RecatProcessController.BASE_PATH +
        RecatProcessController.UPLOAD_NEW_REQUEST).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("username", USERNAME)
        .param("recatRequestCode", RECAT_REQUEST_CODE)
        .param("fileName", FILE_NAME)
        .param("scheduledTime", SCHEDULED_TIME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.recatProcessServiceWrapper)
        .uploadNewRecatRequest(STORE_ID, RECAT_REQUEST_CODE, FILE_NAME, SCHEDULED_TIME);
  }

  @Test
  public void uploadNewRecatRequestExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(recatProcessServiceWrapper)
        .uploadNewRecatRequest(STORE_ID, RECAT_REQUEST_CODE, FILE_NAME, SCHEDULED_TIME);
    this.mockMvc.perform(post(RecatProcessController.BASE_PATH +
        RecatProcessController.UPLOAD_NEW_REQUEST).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("username", USERNAME)
        .param("recatRequestCode", RECAT_REQUEST_CODE)
        .param("fileName", FILE_NAME)
        .param("scheduledTime", SCHEDULED_TIME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.recatProcessServiceWrapper)
        .uploadNewRecatRequest(STORE_ID, RECAT_REQUEST_CODE, FILE_NAME, SCHEDULED_TIME);
  }

  @Test
  public void getRecatProcessSummaryTest() throws Exception {
    Mockito.when(this.recatProcessServiceWrapper
        .getRecatProcessSummary(eq(STORE_ID), Mockito.any(RecatProcessSummaryRequest.class), eq(PAGE),
            eq(SIZE))).thenReturn(new PageImpl(new ArrayList()));
    this.mockMvc.perform(post(RecatProcessController.BASE_PATH +
        RecatProcessController.REQUEST_FILTER_SUMMARY).content(objectMapper.writeValueAsString(new RecatProcessSummaryRequest())).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("username", USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.recatProcessServiceWrapper)
        .getRecatProcessSummary(eq(STORE_ID), Mockito.any(RecatProcessSummaryRequest.class), eq(PAGE),
            eq(SIZE));
  }

  @Test
  public void getRecatProcessSummaryExceptionTest() throws Exception {
    Mockito.when(this.recatProcessServiceWrapper
        .getRecatProcessSummary(eq(STORE_ID), Mockito.any(RecatProcessSummaryRequest.class),
            eq(PAGE), eq(SIZE))).thenThrow(RuntimeException.class);
    this.mockMvc.perform(post(RecatProcessController.BASE_PATH +
        RecatProcessController.REQUEST_FILTER_SUMMARY).content(objectMapper.writeValueAsString(new RecatProcessSummaryRequest())).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("username", USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.recatProcessServiceWrapper)
        .getRecatProcessSummary(eq(STORE_ID), Mockito.any(RecatProcessSummaryRequest.class),
            eq(PAGE), eq(SIZE));
  }

  @Test
  public void updateFinalStatusTest() throws Exception {
    this.mockMvc.perform(get(RecatProcessController.BASE_PATH +
        RecatProcessController.UPDATE_FINAL_STATUS).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("username", USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.recatProcessServiceWrapper).updateRecatProcessFinalStatus(STORE_ID);
  }

  @Test
  public void updateFinalStatusExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(recatProcessServiceWrapper).updateRecatProcessFinalStatus(STORE_ID);
    this.mockMvc.perform(get(RecatProcessController.BASE_PATH +
        RecatProcessController.UPDATE_FINAL_STATUS).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("username", USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.recatProcessServiceWrapper).updateRecatProcessFinalStatus(STORE_ID);
  }

  @Test
  public void cancelRequestTest() throws Exception {
    this.mockMvc.perform(
        get(RecatProcessController.BASE_PATH + RecatProcessControllerTest.ROOT + RecatProcessControllerTest.REQUEST_CODE
            + RecatProcessControllerTest.CANCEL_REQUEST).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("requestId", REQUEST_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("username", USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.recatProcessServiceWrapper).cancelRecatRequest(STORE_ID, REQUEST_CODE, false, USERNAME);
  }

  @Test
  public void cancelRequestExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(recatProcessServiceWrapper)
        .cancelRecatRequest(STORE_ID, REQUEST_CODE, false, USERNAME);
    this.mockMvc.perform(
        get(RecatProcessController.BASE_PATH + RecatProcessControllerTest.ROOT + RecatProcessControllerTest.REQUEST_CODE
            + RecatProcessControllerTest.CANCEL_REQUEST).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
            .param("requestId", REQUEST_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("username", USERNAME)).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.recatProcessServiceWrapper).cancelRecatRequest(STORE_ID, REQUEST_CODE, false, USERNAME);
  }

  @Test
  public void getProductSummaryTest() throws Exception {
    RecatProductSummaryRequest recatProductSummaryRequest =
        new RecatProductSummaryRequest(RecatConstants.FINISHED, StringUtils.EMPTY);
    when(productRecatStatusService
        .getRecatProductSummary(STORE_ID, RECAT_REQUEST_CODE, recatProductSummaryRequest, 0, 1))
        .thenReturn(new PageImpl<>(new ArrayList<>()));
    this.mockMvc.perform(
        post(RecatProcessController.BASE_PATH + RecatProcessController.RECAT_PRODUCT_SUMMARY, RECAT_REQUEST_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(new ObjectMapper().writeValueAsString(recatProductSummaryRequest))
            .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("requestId", REQUEST_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("username", USERNAME).param("page", "0")
            .param("size", "1")).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.productRecatStatusService)
        .getRecatProductSummary(STORE_ID, RECAT_REQUEST_CODE, recatProductSummaryRequest, 0, 1);
  }

  @Test
  public void getProductSummaryExceptionTest() throws Exception {
    RecatProductSummaryRequest recatProductSummaryRequest =
        new RecatProductSummaryRequest(RecatConstants.FINISHED, StringUtils.EMPTY);
    when(productRecatStatusService
        .getRecatProductSummary(STORE_ID, RECAT_REQUEST_CODE, recatProductSummaryRequest, 0, 1))
        .thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(
        post(RecatProcessController.BASE_PATH + RecatProcessController.RECAT_PRODUCT_SUMMARY, RECAT_REQUEST_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(new ObjectMapper().writeValueAsString(recatProductSummaryRequest))
            .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("requestId", REQUEST_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("username", USERNAME).param("page", "0")
            .param("size", "1")).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(this.productRecatStatusService)
        .getRecatProductSummary(STORE_ID, RECAT_REQUEST_CODE, recatProductSummaryRequest, 0, 1);
  }

}