package com.gdn.x.productcategorybase.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.SizeChartApiPath;
import com.gdn.x.productcategorybase.config.MandatoryParameterHelper;
import com.gdn.x.productcategorybase.dto.request.SizeChartFilterRequest;
import com.gdn.x.productcategorybase.dto.response.SizeChartResponse;
import com.gdn.x.productcategorybase.entity.SizeChart;
import com.gdn.x.productcategorybase.exception.ValidationException;
import com.gdn.x.productcategorybase.service.SizeChartService;


import com.gdn.x.productcategorybase.dto.request.SizeChartRequest;
import com.gdn.x.productcategorybase.service.SizeChartServiceWrapper;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.MockitoAnnotations.initMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

public class SizeChartControllerTest {

  private static final String CLIENT_ID = "client-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String REQUEST_ID = "REQ-001";
  private static final String STORE_ID = "10001";
  private static final String USERNAME = "developer";
  private static final String SIZE_CHART_CODE = "SIZ-000";
  private static final String SIZE_CHART_NAME = "SIZE_CHART_NAME";
  private static final String BUSINESS_PARTNER_CODE = "BUSINESS_PARTNER_CODE";
  private static final String CATEGORY_CODE = "CATEGORY_CODE";

  @InjectMocks
  private SizeChartController controller;

  @Mock
  private SizeChartService sizeChartService;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;

  @Mock
  private SizeChartServiceWrapper sizeChartServiceWrapper;

  private static final ObjectMapper objectMapper = new ObjectMapper();
  private MockMvc mockMvc;
  private SizeChartFilterRequest sizeChartFilterRequest;

  private SizeChartRequest sizeChartRequest;

  @BeforeEach
  public void setUp() {
    initMocks(this);this.mockMvc = standaloneSetup(this.controller).build();
    sizeChartFilterRequest = new SizeChartFilterRequest();
    this.mockMvc = standaloneSetup(this.controller).build();
    sizeChartRequest = new SizeChartRequest();
    sizeChartRequest.setSizeChartCode(SIZE_CHART_CODE);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(sizeChartServiceWrapper);
    Mockito.verifyNoMoreInteractions(sizeChartService , mandatoryParameterHelper);
  }


  @Test
  public void filterTest() throws Exception {
    Mockito.when(sizeChartService.filter(STORE_ID, sizeChartFilterRequest, PageRequest.of(0, 10)))
        .thenReturn(new PageImpl<>(Collections.singletonList(SizeChartResponse.builder().build())));
    this.mockMvc.perform(post(SizeChartApiPath.BASE_PATH + SizeChartApiPath.FILTER).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(sizeChartFilterRequest)).param("storeId",
                STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId",
                REQUEST_ID)
            .param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(sizeChartService)
        .filter(STORE_ID, sizeChartFilterRequest, PageRequest.of(0, 10));
  }

  @Test
  public void upsertSizeChartTest_success() throws Exception {
    this.mockMvc.perform(
        post(SizeChartApiPath.BASE_PATH + SizeChartApiPath.UPSERT).accept(MediaType.APPLICATION_JSON)
          .contentType(MediaType.APPLICATION_JSON)
          .content(objectMapper.writeValueAsString(sizeChartRequest)).param("storeId", STORE_ID)
          .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
          .param("username", USERNAME)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(sizeChartServiceWrapper).upsertSizeChart(STORE_ID, sizeChartRequest);
  }

  @Test
  public void fetchSizeChartDetailsTest_success() throws Exception {
    this.mockMvc.perform(
        get(SizeChartApiPath.BASE_PATH + SizeChartApiPath.DETAIL, SIZE_CHART_CODE).contentType(
            MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
          .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME))
      .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(sizeChartServiceWrapper).fetchSizeChartDetails(STORE_ID, false, SIZE_CHART_CODE);
  }

  @Test
  public void deleteSizeChartTest_successMarkForDeleteTrue() throws Exception {
    this.mockMvc.perform(
            post(SizeChartApiPath.BASE_PATH + SizeChartApiPath.UPDATE_SIZE_CHART_STATUS,
                SIZE_CHART_CODE).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME)
                .param("markForDelete", Boolean.TRUE.toString())
                .param("businessPartnerCode", BUSINESS_PARTNER_CODE))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(sizeChartService)
        .updateSizeChartStatusBySizeChartCode(STORE_ID, SIZE_CHART_CODE, BUSINESS_PARTNER_CODE,null, true);
  }

  @Test
  public void findByNameTest_success() throws Exception {
    Mockito.when(sizeChartService.findByNameAndBusinessPartnerCode(STORE_ID, SIZE_CHART_NAME,
            BUSINESS_PARTNER_CODE))
        .thenReturn(SizeChartResponse.builder().sizeChartName(SIZE_CHART_NAME).build());
    this.mockMvc.perform(get(SizeChartApiPath.BASE_PATH
            + SizeChartApiPath.FIND_BY_NAME_AND_BUSINESS_PARTNER_CODE).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)
            .param("sizeChartName", SIZE_CHART_NAME)
            .param("businessPartnerCode", BUSINESS_PARTNER_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(sizeChartService)
        .findByNameAndBusinessPartnerCode(STORE_ID, SIZE_CHART_NAME, BUSINESS_PARTNER_CODE);
  }

  @Test
  public void validateSizeChartCodeTest() throws Exception {
    SizeChart sizeChart = new SizeChart();
    sizeChart.setSizeChartCode(SIZE_CHART_CODE);
    Mockito.when(sizeChartService.findBySizeChartCodeAndMarkForDeleteFalse(STORE_ID, SIZE_CHART_CODE))
        .thenReturn(sizeChart);
    Mockito.when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
   this.mockMvc.perform(
            get(SizeChartApiPath.BASE_PATH + SizeChartApiPath.VALIDATE_SIZE_CHART_CODE.replace("{sizeChartCode}",
                SIZE_CHART_CODE)).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(sizeChartService).findBySizeChartCodeAndMarkForDeleteFalse(STORE_ID, SIZE_CHART_CODE);
    Mockito.verify(mandatoryParameterHelper).getStoreId();
  }

  @Test
  public void validateSizeChartCodeTest_Failure() throws Exception {
    SizeChart sizeChart = new SizeChart();
    sizeChart.setSizeChartCode(SIZE_CHART_CODE);
    Mockito.doThrow(ValidationException.class).when(sizeChartService)
        .findBySizeChartCodeAndMarkForDeleteFalse(STORE_ID, SIZE_CHART_CODE);

    Mockito.when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    Mockito.when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    this.mockMvc.perform(
            get(SizeChartApiPath.BASE_PATH + SizeChartApiPath.VALIDATE_SIZE_CHART_CODE.replace("{sizeChartCode}",
                SIZE_CHART_CODE)).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(sizeChartService).findBySizeChartCodeAndMarkForDeleteFalse(STORE_ID, SIZE_CHART_CODE);
    Mockito.verify(mandatoryParameterHelper).getStoreId();
    Mockito.verify(mandatoryParameterHelper).getRequestId();
  }

  @Test
  public void validCategoryTest() throws Exception {
    Mockito.when(sizeChartService.validateCategoryAttributesForSizeChart(STORE_ID, SIZE_CHART_CODE, CATEGORY_CODE))
        .thenReturn(true);
    Mockito.when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    this.mockMvc.perform(
            get(SizeChartApiPath.BASE_PATH + SizeChartApiPath.VALIDATE_CATEGORY_CODE)
                .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
                .param("categoryCode", CATEGORY_CODE).param("sizeChartCode", SIZE_CHART_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(sizeChartService).validateCategoryAttributesForSizeChart(STORE_ID, SIZE_CHART_CODE, CATEGORY_CODE);
    Mockito.verify(mandatoryParameterHelper).getStoreId();
  }

  @Test
  public void validCategoryExceptionTest() throws Exception {
    Mockito.doThrow(ValidationException.class).when(sizeChartService)
        .validateCategoryAttributesForSizeChart(STORE_ID, SIZE_CHART_CODE, CATEGORY_CODE);

    Mockito.when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);
    this.mockMvc.perform(
            get(SizeChartApiPath.BASE_PATH + SizeChartApiPath.VALIDATE_CATEGORY_CODE.replace("{sizeChartCode}",
                SIZE_CHART_CODE)).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME).param("categoryCode", CATEGORY_CODE)
                .param("sizeChartCode", SIZE_CHART_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(sizeChartService).validateCategoryAttributesForSizeChart(STORE_ID, SIZE_CHART_CODE, CATEGORY_CODE);
    Mockito.verify(mandatoryParameterHelper).getStoreId();
  }

  @Test
  public void getSizeChartNameBySizeChartCodeTest() throws Exception {
    SizeChart sizeChart = new SizeChart();
    sizeChart.setSizeChartCode(SIZE_CHART_CODE);
    List<SizeChart> sizeCharts = new ArrayList<>();
    sizeCharts.add(sizeChart);
    Mockito.when(
            sizeChartService.findSizeChartsBySizeChartCode(STORE_ID,
                Arrays.asList(SIZE_CHART_CODE)))
        .thenReturn(sizeCharts);
    Mockito.when(mandatoryParameterHelper.getStoreId()).thenReturn(STORE_ID);

    this.mockMvc.perform(
            post(SizeChartApiPath.BASE_PATH + SizeChartApiPath.GET_SIZE_CHART_CODE).accept(
                    MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId",
                    CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME)
                .content(objectMapper.writeValueAsString(Arrays.asList(SIZE_CHART_CODE))))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    Mockito.verify(sizeChartService)
        .findSizeChartsBySizeChartCode(STORE_ID, Arrays.asList(SIZE_CHART_CODE));
    Mockito.verify(mandatoryParameterHelper).getStoreId();
  }
}
