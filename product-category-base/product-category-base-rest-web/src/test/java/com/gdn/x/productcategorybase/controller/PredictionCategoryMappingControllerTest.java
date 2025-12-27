package com.gdn.x.productcategorybase.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.PredictionCategoryMappingApiPath;
import com.gdn.x.productcategorybase.dto.request.PredictionCategoryMappingRequest;
import com.gdn.x.productcategorybase.dto.request.PredictionIdsRequest;
import com.gdn.x.productcategorybase.dto.response.PredictionIdAndCategoryCodeResponse;
import com.gdn.x.productcategorybase.service.PredictionCategoryMappingService;

public class PredictionCategoryMappingControllerTest {

  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
  private static final String PREDICTION_ID = "predictionId";
  private static final String PREDICTION_ID_1 = "predictionId1";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "API";
  private static final String DEFAULT_CLIENT_ID = "CLIENT";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_USERNAME = "USERNAME";
  private MockMvc mockMvc;

  @InjectMocks
  private PredictionCategoryMappingController predictionCategoryMappingController;

  @Mock
  private PredictionCategoryMappingService predictionCategoryMappingService;

  @BeforeEach
  public void setUp() {
    initMocks(this);
    this.mockMvc = standaloneSetup(this.predictionCategoryMappingController).build();
  }

  @Test
  public void upsertPredictionCategoryMappingTest() throws Exception {
    List<PredictionCategoryMappingRequest> requestList = new ArrayList<>();
    PredictionCategoryMappingRequest predictionCategoryMappingRequest = new PredictionCategoryMappingRequest();
    predictionCategoryMappingRequest.setPredictionId(PREDICTION_ID);
    predictionCategoryMappingRequest.setCategoryCode(CATEGORY_CODE);
    requestList.add(predictionCategoryMappingRequest);
    String requestString = PredictionCategoryMappingControllerTest.OBJECT_MAPPER.writeValueAsString(requestList);
    this.mockMvc.perform(
            put(PredictionCategoryMappingApiPath.BASE_PATH + PredictionCategoryMappingApiPath.UPSERT).accept(
                    MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE).content(requestString)
                .param("storeId", PredictionCategoryMappingControllerTest.DEFAULT_STORE_ID)
                .param("channelId", PredictionCategoryMappingControllerTest.DEFAULT_CHANNEL_ID)
                .param("clientId", PredictionCategoryMappingControllerTest.DEFAULT_CLIENT_ID)
                .param("requestId", PredictionCategoryMappingControllerTest.DEFAULT_REQUEST_ID)
                .param("username", PredictionCategoryMappingControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(PredictionCategoryMappingControllerTest.DEFAULT_REQUEST_ID)))
        .andReturn();
    Mockito.verify(predictionCategoryMappingService).upsertPredictionCategoryMapping(DEFAULT_STORE_ID, requestList);
  }

  @Test
  public void upsertPredictionCategoryMappingExceptionTest() throws Exception {
    List<PredictionCategoryMappingRequest> requestList = new ArrayList<>();
    String requestString = PredictionCategoryMappingControllerTest.OBJECT_MAPPER.writeValueAsString(requestList);
    this.mockMvc.perform(
            put(PredictionCategoryMappingApiPath.BASE_PATH + PredictionCategoryMappingApiPath.UPSERT).accept(
                    MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE).content(requestString)
                .param("storeId", PredictionCategoryMappingControllerTest.DEFAULT_STORE_ID)
                .param("channelId", PredictionCategoryMappingControllerTest.DEFAULT_CHANNEL_ID)
                .param("clientId", PredictionCategoryMappingControllerTest.DEFAULT_CLIENT_ID)
                .param("requestId", PredictionCategoryMappingControllerTest.DEFAULT_REQUEST_ID)
                .param("username", PredictionCategoryMappingControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(PredictionCategoryMappingControllerTest.DEFAULT_REQUEST_ID)))
        .andReturn();
  }

  @Test
  public void upsertPredictionCategoryMappingException1Test() throws Exception {
    List<PredictionCategoryMappingRequest> requestList = new ArrayList<>();
    PredictionCategoryMappingRequest predictionCategoryMappingRequest = new PredictionCategoryMappingRequest();
    predictionCategoryMappingRequest.setPredictionId(PREDICTION_ID);
    predictionCategoryMappingRequest.setCategoryCode(CATEGORY_CODE);
    requestList.add(predictionCategoryMappingRequest);
    Mockito.doThrow(Exception.class).when(this.predictionCategoryMappingService)
        .upsertPredictionCategoryMapping(Mockito.anyString(), Mockito.anyList());
    String requestString = PredictionCategoryMappingControllerTest.OBJECT_MAPPER.writeValueAsString(requestList);
    this.mockMvc.perform(
            put(PredictionCategoryMappingApiPath.BASE_PATH + PredictionCategoryMappingApiPath.UPSERT).accept(
                    MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE).content(requestString)
                .param("storeId", PredictionCategoryMappingControllerTest.DEFAULT_STORE_ID)
                .param("channelId", PredictionCategoryMappingControllerTest.DEFAULT_CHANNEL_ID)
                .param("clientId", PredictionCategoryMappingControllerTest.DEFAULT_CLIENT_ID)
                .param("requestId", PredictionCategoryMappingControllerTest.DEFAULT_REQUEST_ID)
                .param("username", PredictionCategoryMappingControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(PredictionCategoryMappingControllerTest.DEFAULT_REQUEST_ID)))
        .andReturn();
    Mockito.verify(predictionCategoryMappingService).upsertPredictionCategoryMapping(DEFAULT_STORE_ID, requestList);
  }

  @Test
  public void predictionIdAndCategoryCodeResponseTest() throws Exception {
    List<String> predictionIdList = new ArrayList<>();
    predictionIdList.add(PREDICTION_ID);
    predictionIdList.add(PREDICTION_ID_1);
    PredictionIdsRequest predictionIdsRequest = new PredictionIdsRequest();
    predictionIdsRequest.setPredictionIdList(predictionIdList);
    String requestString =
        PredictionCategoryMappingControllerTest.OBJECT_MAPPER.writeValueAsString(predictionIdsRequest);
    Mockito.when(
            predictionCategoryMappingService.getPredictionIdAndCategoryCodeResponse(DEFAULT_STORE_ID, predictionIdList))
        .thenReturn(Arrays.asList(new PredictionIdAndCategoryCodeResponse()));
    this.mockMvc.perform(post(PredictionCategoryMappingApiPath.BASE_PATH
            + PredictionCategoryMappingApiPath.GET_PREDICTION_ID_AND_CATEGORY_CODE).accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(requestString)
            .param("storeId", PredictionCategoryMappingControllerTest.DEFAULT_STORE_ID)
            .param("channelId", PredictionCategoryMappingControllerTest.DEFAULT_CHANNEL_ID)
            .param("clientId", PredictionCategoryMappingControllerTest.DEFAULT_CLIENT_ID)
            .param("requestId", PredictionCategoryMappingControllerTest.DEFAULT_REQUEST_ID)
            .param("username", PredictionCategoryMappingControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(PredictionCategoryMappingControllerTest.DEFAULT_REQUEST_ID)))
        .andReturn();
    Mockito.verify(predictionCategoryMappingService)
        .getPredictionIdAndCategoryCodeResponse(DEFAULT_STORE_ID, predictionIdList);
  }

  @Test
  public void predictionIdAndCategoryCodeResponseApplicationRuntimeExceptionTest() throws Exception {
    List<String> predictionIdList = new ArrayList<>();
    predictionIdList.add(PREDICTION_ID);
    predictionIdList.add(PREDICTION_ID_1);
    PredictionIdsRequest predictionIdsRequest = new PredictionIdsRequest();
    predictionIdsRequest.setPredictionIdList(predictionIdList);
    String requestString =
        PredictionCategoryMappingControllerTest.OBJECT_MAPPER.writeValueAsString(predictionIdsRequest);
    Mockito.when(
            predictionCategoryMappingService.getPredictionIdAndCategoryCodeResponse(DEFAULT_STORE_ID, predictionIdList))
        .thenThrow(new ApplicationRuntimeException());
    this.mockMvc.perform(post(PredictionCategoryMappingApiPath.BASE_PATH
            + PredictionCategoryMappingApiPath.GET_PREDICTION_ID_AND_CATEGORY_CODE).accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(requestString)
            .param("storeId", PredictionCategoryMappingControllerTest.DEFAULT_STORE_ID)
            .param("channelId", PredictionCategoryMappingControllerTest.DEFAULT_CHANNEL_ID)
            .param("clientId", PredictionCategoryMappingControllerTest.DEFAULT_CLIENT_ID)
            .param("requestId", PredictionCategoryMappingControllerTest.DEFAULT_REQUEST_ID)
            .param("username", PredictionCategoryMappingControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(PredictionCategoryMappingControllerTest.DEFAULT_REQUEST_ID)))
        .andReturn();
    Mockito.verify(predictionCategoryMappingService)
        .getPredictionIdAndCategoryCodeResponse(DEFAULT_STORE_ID, predictionIdList);
  }

  @Test
  public void predictionIdAndCategoryCodeResponseExceptionTest() throws Exception {
    List<String> predictionIdList = new ArrayList<>();
    predictionIdList.add(PREDICTION_ID);
    predictionIdList.add(PREDICTION_ID_1);
    PredictionIdsRequest predictionIdsRequest = new PredictionIdsRequest();
    predictionIdsRequest.setPredictionIdList(predictionIdList);
    String requestString =
        PredictionCategoryMappingControllerTest.OBJECT_MAPPER.writeValueAsString(predictionIdsRequest);
    Mockito.when(
            predictionCategoryMappingService.getPredictionIdAndCategoryCodeResponse(DEFAULT_STORE_ID, predictionIdList))
        .thenThrow(new Exception());
    this.mockMvc.perform(post(PredictionCategoryMappingApiPath.BASE_PATH
            + PredictionCategoryMappingApiPath.GET_PREDICTION_ID_AND_CATEGORY_CODE).accept(MediaType.APPLICATION_JSON_VALUE)
            .contentType(MediaType.APPLICATION_JSON_VALUE).content(requestString)
            .param("storeId", PredictionCategoryMappingControllerTest.DEFAULT_STORE_ID)
            .param("channelId", PredictionCategoryMappingControllerTest.DEFAULT_CHANNEL_ID)
            .param("clientId", PredictionCategoryMappingControllerTest.DEFAULT_CLIENT_ID)
            .param("requestId", PredictionCategoryMappingControllerTest.DEFAULT_REQUEST_ID)
            .param("username", PredictionCategoryMappingControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(PredictionCategoryMappingControllerTest.DEFAULT_REQUEST_ID)))
        .andReturn();
    Mockito.verify(predictionCategoryMappingService)
        .getPredictionIdAndCategoryCodeResponse(DEFAULT_STORE_ID, predictionIdList);
  }

  @Test
  public void getPredictionListByCategoryCodeTest() throws Exception {
    this.mockMvc.perform(get(PredictionCategoryMappingApiPath.BASE_PATH
        + PredictionCategoryMappingApiPath.GET_PREDICTION_LIST_BY_CATEGORY_CODE, CATEGORY_CODE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", DEFAULT_STORE_ID)
        .param("channelId", DEFAULT_CHANNEL_ID)
        .param("clientId", DEFAULT_CLIENT_ID)
        .param("requestId", DEFAULT_REQUEST_ID)
        .param("username", DEFAULT_USERNAME)).andExpect(status().isOk());
    Mockito.verify(predictionCategoryMappingService).getPredictionListByCategoryCode(DEFAULT_STORE_ID, CATEGORY_CODE);
  }

  @Test
  public void getPredictionListByCategoryCodeExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(predictionCategoryMappingService)
        .getPredictionListByCategoryCode(DEFAULT_STORE_ID, CATEGORY_CODE);
    this.mockMvc.perform(get(PredictionCategoryMappingApiPath.BASE_PATH
        + PredictionCategoryMappingApiPath.GET_PREDICTION_LIST_BY_CATEGORY_CODE, CATEGORY_CODE)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", DEFAULT_STORE_ID)
        .param("channelId", DEFAULT_CHANNEL_ID)
        .param("clientId", DEFAULT_CLIENT_ID)
        .param("requestId", DEFAULT_REQUEST_ID)
        .param("username", DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(predictionCategoryMappingService).getPredictionListByCategoryCode(DEFAULT_STORE_ID, CATEGORY_CODE);
  }
}
