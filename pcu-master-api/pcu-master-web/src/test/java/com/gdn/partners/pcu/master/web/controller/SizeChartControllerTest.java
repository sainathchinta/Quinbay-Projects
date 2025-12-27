package com.gdn.partners.pcu.master.web.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.partners.pcu.master.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.master.client.model.BooleanResponse;
import com.gdn.partners.pcu.master.client.model.SizeChartFilterRequest;
import com.gdn.partners.pcu.master.client.model.SizeChartFilterResponse;
import com.gdn.partners.pcu.master.client.model.SizeChartRequest;
import com.gdn.partners.pcu.master.client.model.SizeChartResponse;
import com.gdn.partners.pcu.master.model.Constants;
import com.gdn.partners.pcu.master.model.SizeChartApiPath;
import com.gdn.partners.pcu.master.service.SizeChartService;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;


import static org.hamcrest.Matchers.equalTo;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@ExtendWith(MockitoExtension.class)
class SizeChartControllerTest {

  private static final String STORE_ID = "10001";
  private static final String REQUEST_ID = "requestId";
  private static final String BRAND = "BR-001";
  private static final String SIZE_CHART_CODE = "SC-001";
  private static final String SIZE_CHART_NAME = "sizeChartName";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";

  @InjectMocks
  private SizeChartController sizeChartController;

  @Mock
  private SizeChartService sizeChartService;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  private ObjectMapper objectMapper;
  private MockMvc mockMvc;
  private SizeChartRequest sizeChartRequest;

  @BeforeEach
  void beforeEach() {
    this.mockMvc = MockMvcBuilders.standaloneSetup(this.sizeChartController).build();
    objectMapper = new ObjectMapper();
    sizeChartRequest = new SizeChartRequest();
    sizeChartRequest.setBrand(BRAND);
  }


  @AfterEach
  void teardown() {
    Mockito.verifyNoMoreInteractions(sizeChartService);
    Mockito.verifyNoMoreInteractions(clientParameterHelper);
  }

  @Test
  void upsertSizeChartTest() throws Exception {
    GdnBaseRestResponse serviceResponse = new GdnBaseRestResponse(true);
    when(sizeChartService.upsertSizeChart(STORE_ID, sizeChartRequest)).thenReturn(
        serviceResponse);
    when(clientParameterHelper.getStoreId()).thenReturn(STORE_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(SizeChartApiPath.BASE_PATH + SizeChartApiPath.UPSERT).contentType(
                MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(sizeChartRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    Mockito.verify(sizeChartService).upsertSizeChart(eq(STORE_ID), any());
  }

  @Test
  void fetchSizeChartTest() throws Exception {
    when(sizeChartService.fetchSizeChart(SIZE_CHART_CODE, true)).thenReturn(
      new SizeChartResponse());
    when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
      get(SizeChartApiPath.BASE_PATH + SizeChartApiPath.FETCH_SIZE_CHART, SIZE_CHART_CODE).param(
        "preview", "true");
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(sizeChartService).fetchSizeChart(SIZE_CHART_CODE, true);
  }

  @Test
  void deleteSizeChartTest() throws Exception {
    MockHttpServletRequestBuilder requestBuilder =
        delete(SizeChartApiPath.BASE_PATH + SizeChartApiPath.DELETE, SIZE_CHART_CODE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(sizeChartService).deleteSizeChart(SIZE_CHART_CODE);
  }

  @Test
  void listSizeChartBasedOnFilterTest() throws Exception {
    GdnRestListResponse<SizeChartFilterResponse> serviceResponse = new GdnRestListResponse<>();
    SizeChartFilterRequest request = new SizeChartFilterRequest();
    request.setBrandCode(BRAND);
    when(sizeChartService.filter(0,10,request)).thenReturn(
        serviceResponse);
    when(clientParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(SizeChartApiPath.BASE_PATH + SizeChartApiPath.FILTER).contentType(
                MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(request))
            .param("page","0")
            .param("size","10");
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    Mockito.verify(sizeChartService).filter(0,10, request);
  }

  @Test
  void validateSizeChartTest() throws Exception {
    GdnRestSingleResponse<SizeChartResponse> serviceResponse = new GdnRestSingleResponse<>();
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(sizeChartService.validate(SIZE_CHART_NAME, BUSINESS_PARTNER_CODE)).thenReturn(
        serviceResponse);
    MockHttpServletRequestBuilder requestBuilder =
        get(SizeChartApiPath.BASE_PATH + SizeChartApiPath.VALIDATE).param("sizeChartName",
            SIZE_CHART_NAME).param("businessPartnerCode", BUSINESS_PARTNER_CODE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(sizeChartService).validate(SIZE_CHART_NAME, BUSINESS_PARTNER_CODE);
  }

  @Test
  void validateCategoryTest() throws Exception {
    when(sizeChartService.validateCategory(CATEGORY_CODE, SIZE_CHART_CODE)).thenReturn(
        new BooleanResponse(true));
    MockHttpServletRequestBuilder requestBuilder =
        get(SizeChartApiPath.BASE_PATH + SizeChartApiPath.VALID_CATEGORY).param("categoryCode",
            CATEGORY_CODE).param("sizeChartCode", SIZE_CHART_CODE);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(sizeChartService).validateCategory(CATEGORY_CODE, SIZE_CHART_CODE);
  }
}
