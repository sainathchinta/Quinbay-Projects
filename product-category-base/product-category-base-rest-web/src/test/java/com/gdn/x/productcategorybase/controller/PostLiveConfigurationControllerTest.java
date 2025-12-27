package com.gdn.x.productcategorybase.controller;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.delete;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.x.productcategorybase.dto.CategoryConfigurationRequest;
import com.gdn.x.productcategorybase.dto.ConfigurationStatusRequest;
import com.gdn.x.productcategorybase.dto.MerchantConfigurationRequest;
import com.gdn.x.productcategorybase.dto.PostLiveConfigurationControllerPath;
import com.gdn.x.productcategorybase.dto.request.AttributeCodesRequest;
import com.gdn.x.productcategorybase.dto.request.CategoryConfigurationRequestList;
import com.gdn.x.productcategorybase.dto.request.ConfigurationFilterRequest;
import com.gdn.x.productcategorybase.dto.request.MerchantConfigurationRequestList;
import com.gdn.x.productcategorybase.dto.response.BulkCategoryConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.BulkConfigDataResponse;
import com.gdn.x.productcategorybase.dto.response.BulkMerchantConfigUploadResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationFilterResponse;
import com.gdn.x.productcategorybase.dto.response.CategoryConfigurationHistoryResponse;
import com.gdn.x.productcategorybase.dto.response.ConfigurationCountResponse;
import com.gdn.x.productcategorybase.dto.response.MerchantConfigurationHistoryResponse;
import com.gdn.x.productcategorybase.service.PostLiveConfigurationService;

public class PostLiveConfigurationControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "API";
  private static final String DEFAULT_CLIENT_ID = "CLIENT";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_USERNAME = "USERNAME";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "MTA-0001";
  private static final String DEFAULT_BUSINESS_PARTNER_NAME = "BliBli";
  private static final int PAGE = 0;
  private static final int SIZE = 25;
  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String TYPE = "Merchant";
  private static final String REVIEW_CONFIG = "reviewConfig";
  private static final String TYPE_OF_BUSINESS = "typeOfBusiness";
  private static final String CODE = "CODE";
  private static final String NAME = "name";
  private static final Long DATE_IN_MILLIS = new Date().getTime();
  private static final long TOTAL_RECORDS = 0;

  private MerchantConfigurationRequest merchantConfigurationRequest = new MerchantConfigurationRequest();
  private MockMvc mockMvc;
  private ConfigurationFilterRequest configurationFilterRequest =
      new ConfigurationFilterRequest();
  private List<CategoryConfigurationRequest> categoryConfigurationRequestList;
  private CategoryConfigurationRequest categoryConfigurationRequest;
  private List<MerchantConfigurationRequest> merchantConfigurationRequestList;
  private MerchantConfigurationRequest merchantConfigurationRequest1;
  private ConfigurationStatusRequest configurationStatusRequest;
  private AttributeCodesRequest attributeCodesRequest;
  private CategoryConfigurationRequestList bulkCategoryConfigurationRequestList;
  private MerchantConfigurationRequestList bulkMerchantConfigurationRequestList;
  private List<BulkCategoryConfigUploadResponse> bulkCategoryConfigUploadResponseList;
  private List<BulkMerchantConfigUploadResponse> bulkMerchantConfigUploadResponsesList;
  private BulkConfigDataResponse bulkConfigDataResponse;
  private List<MerchantConfigurationHistoryResponse> merchantConfigurationHistoryResponseList = new ArrayList<>();
  private List<CategoryConfigurationHistoryResponse> categoryConfigurationHistoryResponseList = new ArrayList<>();
  private List<CategoryConfigurationFilterResponse> categoryConfigurationFilterResponseList = new ArrayList<>();
  private Pageable pageable = PageRequest.of(PAGE, SIZE);

  @Mock
  private PostLiveConfigurationService postLiveConfigurationService;

  @Captor
  private ArgumentCaptor<Date> dateArgumentCaptor;

  @InjectMocks
  private PostLiveConfigurationController postLiveConfigurationController;

  @BeforeEach
  public void initializeTest() {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = standaloneSetup(this.postLiveConfigurationController).build();
    merchantConfigurationRequest.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    merchantConfigurationRequest.setBusinessPartnerName(DEFAULT_BUSINESS_PARTNER_NAME);

    categoryConfigurationRequest = new CategoryConfigurationRequest();
    categoryConfigurationRequest.setCategoryCode(CATEGORY_CODE);
    categoryConfigurationRequest.setReviewConfig(REVIEW_CONFIG);
    merchantConfigurationRequest1 = new MerchantConfigurationRequest();
    merchantConfigurationRequest1.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    merchantConfigurationRequest1.setReviewConfig(REVIEW_CONFIG);
    merchantConfigurationRequestList = new ArrayList<>();
    merchantConfigurationRequestList.add(merchantConfigurationRequest1);

    categoryConfigurationRequestList = new ArrayList<>();
    categoryConfigurationRequestList.add(categoryConfigurationRequest);

    configurationStatusRequest =
        ConfigurationStatusRequest.builder().businessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE)
            .categoryCode(CATEGORY_CODE).build();
    bulkCategoryConfigurationRequestList = new CategoryConfigurationRequestList();
    bulkCategoryConfigurationRequestList.setCategoryConfigurationRequestList(categoryConfigurationRequestList);
    bulkCategoryConfigUploadResponseList = new ArrayList<>();
    bulkCategoryConfigUploadResponseList.add(new BulkCategoryConfigUploadResponse());
    bulkMerchantConfigurationRequestList = new MerchantConfigurationRequestList();
    bulkMerchantConfigurationRequestList.setMerchantConfigurationRequestList(merchantConfigurationRequestList);
    bulkMerchantConfigUploadResponsesList = new ArrayList<>();
    bulkMerchantConfigUploadResponsesList.add(new BulkMerchantConfigUploadResponse());

    attributeCodesRequest = new AttributeCodesRequest();
    attributeCodesRequest.setAttributeCodes(Arrays.asList(CODE));

    bulkConfigDataResponse = BulkConfigDataResponse.builder().code(CODE).name(NAME).reviewConfig(REVIEW_CONFIG).build();
  }

  @AfterEach
  public void teardown() {
    Mockito.verifyNoMoreInteractions(postLiveConfigurationService);
  }

  @Test
  public void fetchMerchantSearchResultTest() throws Exception {
    Mockito
        .when(this.postLiveConfigurationService.fetchMerchantConfiguration(Arrays.asList(merchantConfigurationRequest)))
        .thenReturn(new ArrayList<>());
    String requestString = PostLiveConfigurationControllerTest.OBJECT_MAPPER
        .writeValueAsString(Arrays.asList(merchantConfigurationRequest));
    this.mockMvc.perform(post(PostLiveConfigurationControllerPath.BASE_PATH
        + PostLiveConfigurationControllerPath.FETCH_MERCHANT_CONFIGURATION).contentType(MediaType.APPLICATION_JSON)
        .content(requestString).param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    Mockito.verify(this.postLiveConfigurationService).fetchMerchantConfiguration(Arrays.asList(merchantConfigurationRequest));
  }

  @Test
  public void fetchMerchantSearchResultTest_expectException() throws Exception {
    Mockito
        .when(this.postLiveConfigurationService.fetchMerchantConfiguration(Arrays.asList(merchantConfigurationRequest)))
        .thenThrow(RuntimeException.class);
    String requestString = PostLiveConfigurationControllerTest.OBJECT_MAPPER
        .writeValueAsString(Arrays.asList(merchantConfigurationRequest));
    this.mockMvc.perform(post(PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.FETCH_MERCHANT_CONFIGURATION).contentType(MediaType.APPLICATION_JSON)
        .content(requestString).param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(jsonPath("$.errorCode", notNullValue()));
    Mockito.verify(this.postLiveConfigurationService)
        .fetchMerchantConfiguration(Arrays.asList(merchantConfigurationRequest));

  }

  @Test
  public void addCategoryConfigurationTest() throws Exception {
    Mockito.doNothing().when(this.postLiveConfigurationService)
        .addCategoryConfiguration(DEFAULT_STORE_ID, categoryConfigurationRequestList);
    String requestString =
        PostLiveConfigurationControllerTest.OBJECT_MAPPER.writeValueAsString(categoryConfigurationRequestList);
    this.mockMvc.perform(post(
        PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.ADD_CATEGORY_CONFIGURATION)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    Mockito.verify(this.postLiveConfigurationService)
        .addCategoryConfiguration(DEFAULT_STORE_ID, categoryConfigurationRequestList);
  }

  @Test
  public void addCategoryConfigurationTest_expectException() throws Exception {
    Mockito.doThrow(Exception.class).when(this.postLiveConfigurationService)
        .addCategoryConfiguration(DEFAULT_STORE_ID, categoryConfigurationRequestList);
    String requestString =
        PostLiveConfigurationControllerTest.OBJECT_MAPPER.writeValueAsString(categoryConfigurationRequestList);
    this.mockMvc.perform(post(
        PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.ADD_CATEGORY_CONFIGURATION)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", nullValue())).andExpect(jsonPath("$.errorCode", notNullValue()));
    Mockito.verify(this.postLiveConfigurationService)
        .addCategoryConfiguration(DEFAULT_STORE_ID, categoryConfigurationRequestList);
  }

  @Test
  public void updateCategoryConfigurationTest() throws Exception {
    Mockito.doNothing().when(this.postLiveConfigurationService)
        .updateCategoryConfiguration(DEFAULT_STORE_ID, categoryConfigurationRequest);
    String requestString =
        PostLiveConfigurationControllerTest.OBJECT_MAPPER.writeValueAsString(categoryConfigurationRequest);
    this.mockMvc.perform(put(PostLiveConfigurationControllerPath.BASE_PATH
            + PostLiveConfigurationControllerPath.UPDATE_CATEGORY_CONFIGURATION).accept(MediaType.APPLICATION_JSON_VALUE)
        .contentType(MediaType.APPLICATION_JSON_VALUE).content(requestString)
        .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID).param("username", DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.postLiveConfigurationService)
        .updateCategoryConfiguration(DEFAULT_STORE_ID, categoryConfigurationRequest);
  }

  @Test
  public void updateCategoryConfigurationTest_expectException() throws Exception {
    Mockito.doThrow(Exception.class).when(this.postLiveConfigurationService)
        .updateCategoryConfiguration(DEFAULT_STORE_ID, categoryConfigurationRequest);
    String requestString =
        PostLiveConfigurationControllerTest.OBJECT_MAPPER.writeValueAsString(categoryConfigurationRequest);
    this.mockMvc.perform(put(PostLiveConfigurationControllerPath.BASE_PATH
            + PostLiveConfigurationControllerPath.UPDATE_CATEGORY_CONFIGURATION).accept(MediaType.APPLICATION_JSON_VALUE)
        .contentType(MediaType.APPLICATION_JSON_VALUE).content(requestString)
        .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", nullValue())).andExpect(jsonPath("$.errorCode", notNullValue()));
    Mockito.verify(this.postLiveConfigurationService)
        .updateCategoryConfiguration(DEFAULT_STORE_ID, categoryConfigurationRequest);
  }

  @Test
  public void deleteCategoryConfigurationTest() throws Exception {
    Mockito.doNothing().when(this.postLiveConfigurationService)
        .deleteCategoryConfiguration(DEFAULT_STORE_ID, CATEGORY_CODE);
    this.mockMvc.perform(delete(PostLiveConfigurationControllerPath.BASE_PATH
        + PostLiveConfigurationControllerPath.DELETE_CATEGORY_CONFIGURATION, CATEGORY_CODE)
        .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
        .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(true)));
    Mockito.verify(this.postLiveConfigurationService).deleteCategoryConfiguration(DEFAULT_STORE_ID, CATEGORY_CODE);
  }

  @Test
  public void deleteCategoryConfigurationTest_Exception() throws Exception {
    Mockito.doThrow(Exception.class).when(this.postLiveConfigurationService)
        .deleteCategoryConfiguration(DEFAULT_STORE_ID, CATEGORY_CODE);
    this.mockMvc.perform(delete(PostLiveConfigurationControllerPath.BASE_PATH
        + PostLiveConfigurationControllerPath.DELETE_CATEGORY_CONFIGURATION, CATEGORY_CODE)
        .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
        .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", nullValue())).andExpect(jsonPath("$.errorCode", notNullValue()));
    Mockito.verify(this.postLiveConfigurationService).deleteCategoryConfiguration(DEFAULT_STORE_ID, CATEGORY_CODE);
  }

  @Test
  public void addMerchantConfigurationTest() throws Exception {
    Mockito.doNothing().when(this.postLiveConfigurationService)
        .addMerchantConfiguration(DEFAULT_STORE_ID, merchantConfigurationRequestList);
    GdnBaseRestResponse gdnBaseRestResponse = this.postLiveConfigurationController
        .addMerchantConfiguration(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID, DEFAULT_REQUEST_ID,
            DEFAULT_USERNAME, merchantConfigurationRequestList);
    Mockito.verify(this.postLiveConfigurationService)
        .addMerchantConfiguration(DEFAULT_STORE_ID, merchantConfigurationRequestList);
    Assertions.assertTrue(gdnBaseRestResponse.isSuccess());
  }

  @Test
  public void addMerchantConfigurationTest_expectException() throws Exception {
    Mockito.doThrow(Exception.class).when(this.postLiveConfigurationService)
        .addMerchantConfiguration(DEFAULT_STORE_ID, merchantConfigurationRequestList);
    GdnBaseRestResponse gdnBaseRestResponse = this.postLiveConfigurationController
        .addMerchantConfiguration(DEFAULT_STORE_ID, DEFAULT_CHANNEL_ID, DEFAULT_CLIENT_ID, DEFAULT_REQUEST_ID,
            DEFAULT_USERNAME, merchantConfigurationRequestList);
    Mockito.verify(this.postLiveConfigurationService)
        .addMerchantConfiguration(DEFAULT_STORE_ID, merchantConfigurationRequestList);
    Assertions.assertFalse(gdnBaseRestResponse.isSuccess());
  }

  @Test
  public void updateMerchantConfigurationTest() throws Exception {
    Mockito.doNothing().when(this.postLiveConfigurationService)
        .updateMerchantConfiguration(DEFAULT_STORE_ID, merchantConfigurationRequest);
    String requestString =
        PostLiveConfigurationControllerTest.OBJECT_MAPPER.writeValueAsString(merchantConfigurationRequest);
    this.mockMvc.perform(put(PostLiveConfigurationControllerPath.BASE_PATH
            + PostLiveConfigurationControllerPath.UPDATE_MERCHANT_CONFIGURATION).accept(MediaType.APPLICATION_JSON_VALUE)
        .contentType(MediaType.APPLICATION_JSON_VALUE).content(requestString)
        .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID).param("username", DEFAULT_USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.postLiveConfigurationService)
        .updateMerchantConfiguration(DEFAULT_STORE_ID, merchantConfigurationRequest);
  }

  @Test
  public void updateMerchantConfigurationTest_expectException() throws Exception {
    Mockito.doThrow(Exception.class).when(this.postLiveConfigurationService)
        .updateMerchantConfiguration(DEFAULT_STORE_ID, merchantConfigurationRequest);
    String requestString =
        PostLiveConfigurationControllerTest.OBJECT_MAPPER.writeValueAsString(merchantConfigurationRequest);
    this.mockMvc.perform(put(PostLiveConfigurationControllerPath.BASE_PATH
            + PostLiveConfigurationControllerPath.UPDATE_MERCHANT_CONFIGURATION).accept(MediaType.APPLICATION_JSON_VALUE)
        .contentType(MediaType.APPLICATION_JSON_VALUE).content(requestString)
        .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", nullValue())).andExpect(jsonPath("$.errorCode", notNullValue()));
    Mockito.verify(this.postLiveConfigurationService)
        .updateMerchantConfiguration(DEFAULT_STORE_ID, merchantConfigurationRequest);
  }

  @Test
  public void deleteMerchantConfigurationTest() throws Exception {
    Mockito.doNothing().when(this.postLiveConfigurationService)
        .deleteMerchantConfiguration(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    this.mockMvc.perform(delete(PostLiveConfigurationControllerPath.BASE_PATH
        + PostLiveConfigurationControllerPath.DELETE_MERCHANT_CONFIGURATION, DEFAULT_BUSINESS_PARTNER_CODE)
        .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
        .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(true)));
    Mockito.verify(this.postLiveConfigurationService)
        .deleteMerchantConfiguration(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void deleteMerchantConfigurationTest_Exception() throws Exception {
    Mockito.doThrow(Exception.class).when(this.postLiveConfigurationService)
        .deleteMerchantConfiguration(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
    this.mockMvc.perform(delete(PostLiveConfigurationControllerPath.BASE_PATH
        + PostLiveConfigurationControllerPath.DELETE_MERCHANT_CONFIGURATION, DEFAULT_BUSINESS_PARTNER_CODE)
        .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
        .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", nullValue())).andExpect(jsonPath("$.errorCode", notNullValue()));
    Mockito.verify(this.postLiveConfigurationService)
        .deleteMerchantConfiguration(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void fetchConfigurationCountsTest() throws Exception {
    Mockito.when(this.postLiveConfigurationService.fetchConfigurationCounts(DEFAULT_STORE_ID))
        .thenReturn(new ConfigurationCountResponse());
    this.mockMvc.perform(get(PostLiveConfigurationControllerPath.BASE_PATH
        + PostLiveConfigurationControllerPath.FETCH_CONFIGURATION_COUNT).contentType(MediaType.APPLICATION_JSON_VALUE)
        .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    Mockito.verify(this.postLiveConfigurationService).fetchConfigurationCounts(DEFAULT_STORE_ID);
  }

  @Test
  public void fetchConfigurationCountsTest_expectException() throws Exception {
    Mockito.when(this.postLiveConfigurationService.fetchConfigurationCounts(DEFAULT_STORE_ID))
        .thenThrow(RuntimeException.class);
    this.mockMvc.perform(get(PostLiveConfigurationControllerPath.BASE_PATH
        + PostLiveConfigurationControllerPath.FETCH_CONFIGURATION_COUNT).contentType(MediaType.APPLICATION_JSON_VALUE)
        .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(jsonPath("$.errorCode", notNullValue()));;
    Mockito.verify(this.postLiveConfigurationService).fetchConfigurationCounts(DEFAULT_STORE_ID);
  }

  @Test
  public void getConfigurationStatusTest() throws Exception {
    Mockito.when(this.postLiveConfigurationService
        .getConfigurations(DEFAULT_STORE_ID, Arrays.asList(configurationStatusRequest))).thenReturn(new ArrayList<>());
    String requestString =
        PostLiveConfigurationControllerTest.OBJECT_MAPPER.writeValueAsString(Arrays.asList(configurationStatusRequest));
    this.mockMvc.perform(post(
        PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.GET_CONFIGURATION_STATUS)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    Mockito.verify(this.postLiveConfigurationService)
        .getConfigurations(DEFAULT_STORE_ID, Arrays.asList(configurationStatusRequest));
  }

  @Test
  public void getConfigurationStatusTest_expectException() throws Exception {
    Mockito.when(this.postLiveConfigurationService
        .getConfigurations(DEFAULT_STORE_ID, Arrays.asList(configurationStatusRequest))).thenThrow(Exception.class);
    String requestString =
        PostLiveConfigurationControllerTest.OBJECT_MAPPER.writeValueAsString(Arrays.asList(configurationStatusRequest));
    this.mockMvc.perform(post(
        PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.GET_CONFIGURATION_STATUS)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", nullValue())).andExpect(jsonPath("$.errorCode", notNullValue()));
    Mockito.verify(this.postLiveConfigurationService)
        .getConfigurations(DEFAULT_STORE_ID, Arrays.asList(configurationStatusRequest));
  }
  public void bulkCategoryConfigUploadTest() throws Exception {
    Mockito.when(this.postLiveConfigurationService.bulkCategoryConfigUpload(DEFAULT_STORE_ID,
        bulkCategoryConfigurationRequestList.getCategoryConfigurationRequestList()))
        .thenReturn(bulkCategoryConfigUploadResponseList);
    String requestString =
        PostLiveConfigurationControllerTest.OBJECT_MAPPER.writeValueAsString(bulkCategoryConfigurationRequestList);
    this.mockMvc.perform(
        post(PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.BULK_CONFIG_CATEGORY)
            .contentType(MediaType.APPLICATION_JSON).content(requestString)
            .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
            .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
            .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
            .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
            .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.postLiveConfigurationService).bulkCategoryConfigUpload(DEFAULT_STORE_ID,
        bulkCategoryConfigurationRequestList.getCategoryConfigurationRequestList());
  }

  @Test
  public void bulkCategoryConfigUploadExceptionTest() throws Exception {
    Mockito.when(this.postLiveConfigurationService.bulkCategoryConfigUpload(DEFAULT_STORE_ID,
        bulkCategoryConfigurationRequestList.getCategoryConfigurationRequestList())).thenThrow(Exception.class);
    String requestString =
        PostLiveConfigurationControllerTest.OBJECT_MAPPER.writeValueAsString(bulkCategoryConfigurationRequestList);
    this.mockMvc.perform(
        post(PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.BULK_CONFIG_CATEGORY)
            .contentType(MediaType.APPLICATION_JSON).content(requestString)
            .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
            .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
            .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
            .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
            .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.postLiveConfigurationService).bulkCategoryConfigUpload(DEFAULT_STORE_ID,
        bulkCategoryConfigurationRequestList.getCategoryConfigurationRequestList());
  }

  @Test
  public void bulkMerchantConfigUploadTest() throws Exception {
    Mockito.when(this.postLiveConfigurationService.bulkMerchantConfigUpload(DEFAULT_STORE_ID,
        bulkMerchantConfigurationRequestList.getMerchantConfigurationRequestList()))
        .thenReturn(bulkMerchantConfigUploadResponsesList);
    String requestString =
        PostLiveConfigurationControllerTest.OBJECT_MAPPER.writeValueAsString(bulkMerchantConfigurationRequestList);
    this.mockMvc.perform(
        post(PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.BULK_CONFIG_MERCHANT)
            .contentType(MediaType.APPLICATION_JSON).content(requestString)
            .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
            .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
            .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
            .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
            .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.postLiveConfigurationService).bulkMerchantConfigUpload(DEFAULT_STORE_ID,
        bulkMerchantConfigurationRequestList.getMerchantConfigurationRequestList());
  }

  @Test
  public void bulkMerchantConfigUploadExceptionTest() throws Exception {
    Mockito.when(this.postLiveConfigurationService.bulkMerchantConfigUpload(DEFAULT_STORE_ID,
        bulkMerchantConfigurationRequestList.getMerchantConfigurationRequestList())).thenThrow(Exception.class);
    String requestString =
        PostLiveConfigurationControllerTest.OBJECT_MAPPER.writeValueAsString(bulkMerchantConfigurationRequestList);
    this.mockMvc.perform(
        post(PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.BULK_CONFIG_MERCHANT)
            .contentType(MediaType.APPLICATION_JSON).content(requestString)
            .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
            .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
            .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
            .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
            .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.postLiveConfigurationService).bulkMerchantConfigUpload(DEFAULT_STORE_ID,
        bulkMerchantConfigurationRequestList.getMerchantConfigurationRequestList());
  }

  @Test
  public void getConfigurationChangesTest() throws Exception {
    Mockito.when(this.postLiveConfigurationService.getConfigurationChangesByDate(Mockito.eq(DEFAULT_STORE_ID),
        Mockito.any(Date.class))).thenReturn(new ArrayList<>());
    this.mockMvc.perform(
        get(PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.GET_CONFIGURATION_CHANGES)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
            .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
            .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
            .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
            .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME)
            .param("fromDateInMillis", OBJECT_MAPPER.writeValueAsString(PostLiveConfigurationControllerTest.DATE_IN_MILLIS)))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.postLiveConfigurationService).getConfigurationChangesByDate(Mockito.eq(DEFAULT_STORE_ID),
        dateArgumentCaptor.capture());
    Assertions.assertEquals(DATE_IN_MILLIS, dateArgumentCaptor.getValue().getTime(), 0);
  }

  @Test
  public void getConfigurationChangesExceptionTest() throws Exception {
    Mockito.when(this.postLiveConfigurationService.getConfigurationChangesByDate(Mockito.eq(DEFAULT_STORE_ID),
        Mockito.any(Date.class))).thenThrow(new ApplicationRuntimeException());
    this.mockMvc.perform(
        get(PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.GET_CONFIGURATION_CHANGES)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
            .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
            .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
            .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
            .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME)
            .param("fromDateInMillis", OBJECT_MAPPER.writeValueAsString(PostLiveConfigurationControllerTest.DATE_IN_MILLIS)))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.postLiveConfigurationService).getConfigurationChangesByDate(Mockito.eq(DEFAULT_STORE_ID),
        dateArgumentCaptor.capture());
    Assertions.assertEquals(DATE_IN_MILLIS, dateArgumentCaptor.getValue().getTime(), 0);
  }

  @Test
  public void getCategoryConfigurationListTest() throws Exception {
    Mockito.when(this.postLiveConfigurationService
        .getCategoryConfigurationList(DEFAULT_STORE_ID, configurationFilterRequest, PAGE, SIZE))
        .thenReturn(new PageImpl<>(categoryConfigurationFilterResponseList, pageable,TOTAL_RECORDS));
    String requestString = PostLiveConfigurationControllerTest.OBJECT_MAPPER
        .writeValueAsString(configurationFilterRequest);
    this.mockMvc.perform(post(PostLiveConfigurationControllerPath.BASE_PATH
        + PostLiveConfigurationControllerPath.FILTER_CATEGORY_CONFIGURATION).contentType(MediaType.APPLICATION_JSON)
        .content(requestString).param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(true)))
        .andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(jsonPath("$.errorCode", nullValue()));
    Mockito.verify(this.postLiveConfigurationService)
        .getCategoryConfigurationList(DEFAULT_STORE_ID, configurationFilterRequest, PAGE, SIZE);
  }

  @Test
  public void getCategoryConfigurationListTest_expectException() throws Exception {
    Mockito.when(this.postLiveConfigurationService
        .getCategoryConfigurationList(DEFAULT_STORE_ID, configurationFilterRequest, PAGE, SIZE))
        .thenThrow(RuntimeException.class);
    String requestString = PostLiveConfigurationControllerTest.OBJECT_MAPPER
        .writeValueAsString(configurationFilterRequest);
    this.mockMvc.perform(post(PostLiveConfigurationControllerPath.BASE_PATH
        + PostLiveConfigurationControllerPath.FILTER_CATEGORY_CONFIGURATION).contentType(MediaType.APPLICATION_JSON)
        .content(requestString).param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)));
    Mockito.verify(this.postLiveConfigurationService)
        .getCategoryConfigurationList(DEFAULT_STORE_ID, configurationFilterRequest, PAGE, SIZE);
  }

  @Test
  public void getMerchantConfigurationListTest_expectException() throws Exception {
    Mockito.when(this.postLiveConfigurationService
        .getMerchantConfigurationList(DEFAULT_STORE_ID, configurationFilterRequest, PAGE, SIZE))
        .thenThrow(Exception.class);
    String requestString = PostLiveConfigurationControllerTest.OBJECT_MAPPER
        .writeValueAsString(configurationFilterRequest);
    this.mockMvc.perform(post(PostLiveConfigurationControllerPath.BASE_PATH
        + PostLiveConfigurationControllerPath.FILTER_MERCHANT_CONFIGURATION).contentType(MediaType.APPLICATION_JSON)
        .content(requestString).param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)));
    Mockito.verify(this.postLiveConfigurationService)
        .getMerchantConfigurationList(DEFAULT_STORE_ID, configurationFilterRequest, PAGE, SIZE);
  }

  @Test
  public void getMerchantConfigurationListTest() throws Exception {
    Mockito.when(this.postLiveConfigurationService
        .getMerchantConfigurationHistory(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, PAGE, SIZE))
        .thenReturn(new PageImpl<>(merchantConfigurationHistoryResponseList, pageable,TOTAL_RECORDS));
    this.mockMvc.perform(get(PostLiveConfigurationControllerPath.BASE_PATH
        + PostLiveConfigurationControllerPath.MERCHANT_CONFIGURATION_HISTORY, DEFAULT_BUSINESS_PARTNER_CODE)
        .contentType(MediaType.APPLICATION_JSON_VALUE)
        .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    Mockito.verify(this.postLiveConfigurationService)
        .getMerchantConfigurationHistory(DEFAULT_STORE_ID, DEFAULT_BUSINESS_PARTNER_CODE, PAGE, SIZE);
  }

  @Test
  public void getCategoryConfigurationHistoryTest() throws Exception {
    Mockito.when(
        this.postLiveConfigurationService.getCategoryConfigurationHistory(DEFAULT_STORE_ID, CATEGORY_CODE, PAGE, SIZE))
        .thenReturn(new PageImpl<>(categoryConfigurationHistoryResponseList, pageable,TOTAL_RECORDS));
    this.mockMvc.perform(get(PostLiveConfigurationControllerPath.BASE_PATH
        + PostLiveConfigurationControllerPath.CATEGORY_CONFIGURATION_HISTORY, CATEGORY_CODE).param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(true)))
        .andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(jsonPath("$.errorCode", nullValue()));
    Mockito.verify(this.postLiveConfigurationService)
        .getCategoryConfigurationHistory(DEFAULT_STORE_ID, CATEGORY_CODE, PAGE, SIZE);
  }

  @Test
  public void getCategoryConfigurationHistoryTest_expectException() throws Exception {
    Mockito.when(
        this.postLiveConfigurationService.getCategoryConfigurationHistory(DEFAULT_STORE_ID, CATEGORY_CODE, PAGE, SIZE))
        .thenThrow(RuntimeException.class);
    this.mockMvc.perform(get(PostLiveConfigurationControllerPath.BASE_PATH
        + PostLiveConfigurationControllerPath.CATEGORY_CONFIGURATION_HISTORY, CATEGORY_CODE).param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(jsonPath("$.errorCode", notNullValue()));
    Mockito.verify(this.postLiveConfigurationService)
        .getCategoryConfigurationHistory(DEFAULT_STORE_ID, CATEGORY_CODE, PAGE, SIZE);
  }

  @Test
  public void fetchConfigDetailsByCodesTest() throws Exception {
    Mockito.when(this.postLiveConfigurationService
        .fetchConfigDetailsByConfigTypeForCodes(DEFAULT_STORE_ID, TYPE, Arrays.asList(CODE)))
        .thenReturn(Arrays.asList(bulkConfigDataResponse));
    String requestString = PostLiveConfigurationControllerTest.OBJECT_MAPPER.writeValueAsString(attributeCodesRequest);
    this.mockMvc.perform(
        post(PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.FETCH_DETAILS_BY_CODE)
            .contentType(MediaType.APPLICATION_JSON).content(requestString)
            .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
            .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
            .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
            .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
            .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME).param("configType", TYPE))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.postLiveConfigurationService)
        .fetchConfigDetailsByConfigTypeForCodes(DEFAULT_STORE_ID, TYPE, Arrays.asList(CODE));
  }

  @Test
  public void fetchConfigDetailsByCodesExceptionTest() throws Exception {
    Mockito.when(this.postLiveConfigurationService
        .fetchConfigDetailsByConfigTypeForCodes(DEFAULT_STORE_ID, TYPE, Arrays.asList(CODE)))
        .thenThrow(Exception.class);
    String requestString = PostLiveConfigurationControllerTest.OBJECT_MAPPER.writeValueAsString(attributeCodesRequest);
    this.mockMvc.perform(
        post(PostLiveConfigurationControllerPath.BASE_PATH + PostLiveConfigurationControllerPath.FETCH_DETAILS_BY_CODE)
            .contentType(MediaType.APPLICATION_JSON).content(requestString)
            .param("storeId", PostLiveConfigurationControllerTest.DEFAULT_STORE_ID)
            .param("channelId", PostLiveConfigurationControllerTest.DEFAULT_CHANNEL_ID)
            .param("clientId", PostLiveConfigurationControllerTest.DEFAULT_CLIENT_ID)
            .param("requestId", PostLiveConfigurationControllerTest.DEFAULT_REQUEST_ID)
            .param("username", PostLiveConfigurationControllerTest.DEFAULT_USERNAME).param("configType", TYPE))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.postLiveConfigurationService)
        .fetchConfigDetailsByConfigTypeForCodes(DEFAULT_STORE_ID, TYPE, Arrays.asList(CODE));
  }
}
