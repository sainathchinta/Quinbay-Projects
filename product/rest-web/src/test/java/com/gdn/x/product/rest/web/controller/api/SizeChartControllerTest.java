package com.gdn.x.product.rest.web.controller.api;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.net.URI;

import org.apache.http.client.utils.URIBuilder;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.http.converter.ResourceHttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.enums.CurationStatus;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.ProductV2ApiPath;
import com.gdn.x.product.rest.web.model.SizeChartApiPath;
import com.gdn.x.product.rest.web.model.request.ProductSizeChartUpdateRequest;
import com.gdn.x.product.rest.web.model.response.SizeChartResponse;
import com.gdn.x.product.service.api.SizeChartService;
import com.gdn.x.product.service.util.ModelConverter;

public class SizeChartControllerTest {

  private static final String SIZE_CHART_CODE = "SIZE_CHART_CODE";
  private static final String DEFAULT_STORE_ID = "DEFAULT_STORE_ID";
  private static final String DEFAULT_CHANNEL_ID = "DEFAULT_CHANNEL_ID";
  private static final String DEFAULT_CLIENT_ID = "DEFAULT_CLIENT_ID";
  private static final String DEFAULT_USERNAME = "DEFAULT_USERNAME";
  private static final String DEFAULT_REQUEST_ID = "DEFAULT_REQUEST_ID";

  @InjectMocks
  private SizeChartController sizeChartController;

  @Mock
  private SizeChartService sizeChartService;

  @Mock
  private ModelConverter modelConverter;

  private MockMvc mockMvc;
  private ObjectMapper objectMapper;
  private ProductSizeChartUpdateRequest productSizeChartUpdateRequest;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(this.sizeChartController)
        .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
            new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
            new MappingJackson2HttpMessageConverter()).build();
    objectMapper = new ObjectMapper();
    productSizeChartUpdateRequest = new ProductSizeChartUpdateRequest();
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(sizeChartService);
    Mockito.verifyNoMoreInteractions(modelConverter);
  }

  public MockMvc getMockMvc() {
    return this.mockMvc;
  }

  @Test
  public void getProductScoreRuleTest() throws Exception {
    SizeChartResponse sizeChartResponse = new SizeChartResponse();
    Mockito.when(this.sizeChartService.fetchSizeChartDetails(DEFAULT_STORE_ID, SIZE_CHART_CODE))
        .thenReturn(sizeChartResponse);
    this.mockMvc.perform(
            get(SizeChartApiPath.BASE_PATH + SizeChartApiPath.GET_SIZE_CHART_DETAILS, SIZE_CHART_CODE).accept(
                    MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", DEFAULT_STORE_ID)
                .param("channelId", DEFAULT_CHANNEL_ID).param("clientId", DEFAULT_CLIENT_ID)
                .param("requestId", DEFAULT_REQUEST_ID).param("username", DEFAULT_USERNAME)
                .param("sizeChartCode", SIZE_CHART_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(modelConverter).updateSizeChartResponse(sizeChartResponse);
    Mockito.verify(this.sizeChartService).fetchSizeChartDetails(Mockito.eq(DEFAULT_STORE_ID), Mockito.anyString());
  }

  @Test
  public void getProductScoreRuleExceptionTest() throws Exception {
    Mockito.when(this.sizeChartService.fetchSizeChartDetails(Mockito.eq(DEFAULT_STORE_ID), Mockito.anyString()))
        .thenThrow(ApplicationRuntimeException.class);
    URI uri = new URIBuilder().setPath(SizeChartApiPath.BASE_PATH + SizeChartApiPath.GET_SIZE_CHART_DETAILS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("sizeChartCode", SIZE_CHART_CODE).build();
    getMockMvc().perform(MockMvcRequestBuilders.get(uri.toString(), SIZE_CHART_CODE).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.sizeChartService).fetchSizeChartDetails(Mockito.eq(DEFAULT_STORE_ID), Mockito.anyString());
  }


  @Test
  public void getProductScoreRuleGenericExceptionTest() throws Exception {
    Mockito.when(this.sizeChartService.fetchSizeChartDetails(Mockito.eq(DEFAULT_STORE_ID), Mockito.anyString()))
        .thenThrow(ApplicationRuntimeException.class);
    URI uri = new URIBuilder().setPath(SizeChartApiPath.BASE_PATH + SizeChartApiPath.GET_SIZE_CHART_DETAILS)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_CHANNEL_ID)
        .addParameter("requestId", DEFAULT_REQUEST_ID).addParameter("username", DEFAULT_USERNAME)
        .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("sizeChartCode", SIZE_CHART_CODE).build();
    getMockMvc().perform(MockMvcRequestBuilders.get(uri.toString(), SIZE_CHART_CODE).accept(MediaType.APPLICATION_JSON)
        .contentType(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.sizeChartService).fetchSizeChartDetails(Mockito.eq(DEFAULT_STORE_ID), Mockito.anyString());
  }

  @Test
  public void updateProductSizeChartTest() throws Exception {
    Mockito.doNothing().when(this.sizeChartService)
        .updateProductSizeChartCode(DEFAULT_STORE_ID, SIZE_CHART_CODE, productSizeChartUpdateRequest);
    this.mockMvc.perform(
            post(SizeChartApiPath.BASE_PATH + SizeChartApiPath.UPDATE_PRODUCT_SIZE_CHART, SIZE_CHART_CODE).accept(
                    MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", DEFAULT_STORE_ID)
                .param("channelId", DEFAULT_CHANNEL_ID).param("clientId", DEFAULT_CLIENT_ID)
                .param("requestId", DEFAULT_REQUEST_ID).param("username", DEFAULT_USERNAME)
                .content(objectMapper.writeValueAsBytes(productSizeChartUpdateRequest))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.sizeChartService)
        .updateProductSizeChartCode(DEFAULT_STORE_ID, SIZE_CHART_CODE, productSizeChartUpdateRequest);
  }

  @Test
  public void updateProductSizeChartValidationErrorTest() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.PRODUCT_NOT_FOUND_ERROR))
        .when(this.sizeChartService)
        .updateProductSizeChartCode(DEFAULT_STORE_ID, SIZE_CHART_CODE, productSizeChartUpdateRequest);
    this.mockMvc.perform(
            post(SizeChartApiPath.BASE_PATH + SizeChartApiPath.UPDATE_PRODUCT_SIZE_CHART, SIZE_CHART_CODE).accept(
                    MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", DEFAULT_STORE_ID)
                .param("channelId", DEFAULT_CHANNEL_ID).param("clientId", DEFAULT_CLIENT_ID)
                .param("requestId", DEFAULT_REQUEST_ID).param("username", DEFAULT_USERNAME)
                .content(objectMapper.writeValueAsBytes(productSizeChartUpdateRequest))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.sizeChartService)
        .updateProductSizeChartCode(DEFAULT_STORE_ID, SIZE_CHART_CODE, productSizeChartUpdateRequest);
  }

  @Test
  public void updateProductSizeChartErrorTest() throws Exception {
    Mockito.doThrow(new RuntimeException(ErrorMessages.PRODUCT_NOT_FOUND_ERROR))
        .when(this.sizeChartService)
        .updateProductSizeChartCode(DEFAULT_STORE_ID, SIZE_CHART_CODE, productSizeChartUpdateRequest);
    this.mockMvc.perform(
            post(SizeChartApiPath.BASE_PATH + SizeChartApiPath.UPDATE_PRODUCT_SIZE_CHART, SIZE_CHART_CODE).accept(
                    MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", DEFAULT_STORE_ID)
                .param("channelId", DEFAULT_CHANNEL_ID).param("clientId", DEFAULT_CLIENT_ID)
                .param("requestId", DEFAULT_REQUEST_ID).param("username", DEFAULT_USERNAME)
                .content(objectMapper.writeValueAsBytes(productSizeChartUpdateRequest))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    Mockito.verify(this.sizeChartService)
        .updateProductSizeChartCode(DEFAULT_STORE_ID, SIZE_CHART_CODE, productSizeChartUpdateRequest);
  }
}
