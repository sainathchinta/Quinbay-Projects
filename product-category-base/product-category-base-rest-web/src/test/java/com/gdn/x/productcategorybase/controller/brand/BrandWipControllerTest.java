package com.gdn.x.productcategorybase.controller.brand;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import com.gdn.x.productcategorybase.dto.BrandInReviewResponse;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.utils.URIBuilder;
import org.hamcrest.CoreMatchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationException;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.productcategorybase.ErrorMessage;
import com.gdn.x.productcategorybase.BrandWipControllerPath;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectionInfoResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandApproveRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandRejectRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistoryResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipHistorySummaryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandWipResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandWipSummaryRequest;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandResponse;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipRequest;
import com.gdn.x.productcategorybase.dto.brand.CreateBrandWipResponse;
import com.gdn.x.productcategorybase.entity.brand.BrandWip;
import com.gdn.x.productcategorybase.entity.brand.BrandWipState;
import com.gdn.x.productcategorybase.service.brand.BrandServiceWrapper;
import com.gdn.x.productcategorybase.service.brand.BrandWipService;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

public class BrandWipControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_ID = "ID";
  private static final String DEFAULT_CHANNEL_ID = "API";
  private static final String DEFAULT_CLIENT_ID = "CLIENT";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_USERNAME = "USERNAME";
  private static final String DEFAULT_BUSINESS_PARTNER_CODE = "MTA-0001";
  private static final String DEFAULT_BUSINESS_PARTNER_NAME = "BliBli";
  private static final String DEFAULT_BRAND_NAME = "Blibli.com&amp;";
  private static final String DEFAULT_BRAND_NAME_AFTER_ESCAPING = "Blibli.com&";
  private static final String DEFAULT_BRAND_REQUEST_CODE = "BR-0001-0001";
  private static final String DEFAULT_BRAND_DESCRIPTION_STRING = "default description";
  private static final byte[] DEFAULT_BRAND_DESCRIPTION = DEFAULT_BRAND_DESCRIPTION_STRING.getBytes();
  private static final String DEFAULT_BRAND_CODE = "brandCode";
  private MockMvc mockMvc;
  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
  private BrandWipResponse brandWipResponse = new BrandWipResponse();
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String DEFAULT_DESCRIPTION_STRING = "default description";
  private static final String DEFAULT_NOT_FOUND_MESSAGE = "Required data not found";
  private BrandWipHistorySummaryRequest brandWipHistorySummaryRequest = new BrandWipHistorySummaryRequest();
  private BrandWipHistoryResponse brandWipHistoryResponse = new BrandWipHistoryResponse();
  private List<BrandWipHistoryResponse> brandWipHistoryResponseList = new ArrayList<>();
  private GdnRestListResponse<BrandWipHistoryResponse> brandWipHistoryResponseGdnRestListResponse =
      new GdnRestListResponse<>();
  private Page<BrandWipHistoryResponse> page;
  private BrandWipSummaryRequest brandWipSummaryRequest = new BrandWipSummaryRequest();
  private static final int size = 10;
  private Page<BrandWipResponse> brandWipResponsePage;
  private BrandApproveRequest brandApproveRequest = new BrandApproveRequest();
  private BrandInReviewResponse brandInReviewResponse = new BrandInReviewResponse();

  CreateBrandWipRequest request = new CreateBrandWipRequest();
  CreateBrandWipResponse response = new CreateBrandWipResponse();
  BrandWip brandWip = new BrandWip();
  BrandRejectionInfoResponse brandRejectionInfoResponse;
  private BrandRejectRequest brandRejectRequest;
  private ObjectMapper objectMapper;

  @Mock
  private BrandWipService brandWipService;

  @Mock
  private BrandServiceWrapper brandServiceWrapper;

  @InjectMocks
  private BrandWipController brandWipController;

  @BeforeEach
  public void initializeTest() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = standaloneSetup(this.brandWipController).build();
    request.setBrandName(DEFAULT_BRAND_NAME);
    request.setBrandDescription(DEFAULT_BRAND_DESCRIPTION_STRING);
    request.setBrandLogoPath(null);
    request.setProfileBannerPath(null);
    request.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    request.setBusinessPartnerName(DEFAULT_BUSINESS_PARTNER_NAME);
    request.setProtectedBrand(true);
    brandWip.setBrandDescription(DEFAULT_BRAND_DESCRIPTION);
    brandWip.setBusinessPartnerCode(DEFAULT_BUSINESS_PARTNER_CODE);
    brandWip.setBusinessPartnerName(DEFAULT_BUSINESS_PARTNER_NAME);
    brandWip.setBrandName(DEFAULT_BRAND_NAME_AFTER_ESCAPING);
    brandWip.setProtectedBrand(true);
    brandWipResponse.setBrandDescription(DEFAULT_BRAND_DESCRIPTION.toString());
    brandWipResponse.setBrandName(DEFAULT_BRAND_NAME);
    brandWipResponse.setBrandCode(DEFAULT_BRAND_REQUEST_CODE);
    brandWipHistorySummaryRequest.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandWipHistoryResponse.setDescription(DEFAULT_DESCRIPTION_STRING);
    brandWipHistoryResponse.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandWipHistoryResponse.setState(BrandWipState.DRAFT.getDescription());
    brandWipHistoryResponseList.add(brandWipHistoryResponse);
    page = new PageImpl<>(brandWipHistoryResponseList);
    brandWipSummaryRequest.setBrandName("apple");
    brandWipSummaryRequest.setState("DRAFT");
    List<BrandWipResponse> brandWipResponseList = new ArrayList<>();
    brandWipResponsePage = new PageImpl<>(brandWipResponseList);
    brandRejectionInfoResponse = new BrandRejectionInfoResponse();
    brandRejectionInfoResponse.setRejectionReason(DEFAULT_BRAND_DESCRIPTION_STRING);
    brandRejectionInfoResponse.setBrandName(DEFAULT_BRAND_NAME);
    brandRejectionInfoResponse.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandRejectionInfoResponse.setId(DEFAULT_ID);
    brandWipResponsePage = new PageImpl<>(brandWipResponseList);

    this.objectMapper = new ObjectMapper(new JsonFactory());

    brandApproveRequest = new BrandApproveRequest();
    brandApproveRequest.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
    brandApproveRequest.setBrandName(DEFAULT_BRAND_NAME);
    brandApproveRequest.setBrandDescription(DEFAULT_BRAND_DESCRIPTION_STRING);

    brandRejectRequest = new BrandRejectRequest();
    brandRejectRequest.setBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
  }

  @AfterEach
  public void finalizeTest() throws Exception {
    Mockito.verifyNoMoreInteractions(this.brandWipService);
    Mockito.verifyNoMoreInteractions(this.brandServiceWrapper);
  }


  @Test
  public void createTest() throws Exception {
    String requestString = BrandWipControllerTest.OBJECT_MAPPER.writeValueAsString(request);
    Mockito.when(this.brandServiceWrapper.createBrand(DEFAULT_STORE_ID, brandWip))
      .thenReturn(DEFAULT_BRAND_REQUEST_CODE);
    this.mockMvc.perform(post(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.CREATE)
        .contentType(MediaType.APPLICATION_JSON)
        .content(requestString)
        .param("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
        .param("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .param("username", BrandWipControllerTest.DEFAULT_USERNAME)
    ).andExpect(status().isOk());
    Mockito.verify(this.brandServiceWrapper).createBrand(DEFAULT_STORE_ID, brandWip);
  }

  @Test
  public void create_expectsException() throws Exception {
    String requestString = BrandWipControllerTest.OBJECT_MAPPER.writeValueAsString(request);
    Mockito.when(this.brandServiceWrapper.createBrand(DEFAULT_STORE_ID, brandWip)).thenThrow(IOException.class);
    this.mockMvc.perform(
        post(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.CREATE)
            .contentType(MediaType.APPLICATION_JSON)
            .content(requestString).param("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
            .param("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
            .param("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
            .param("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
            .param("username", BrandWipControllerTest.DEFAULT_USERNAME))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(jsonPath("$.errorCode", notNullValue()));
    Mockito.verify(this.brandServiceWrapper).createBrand(DEFAULT_STORE_ID, brandWip);
  }

  @Test
  public void createTest_throwsApplicationRuntimeException() throws Exception {
    brandWip.setBusinessPartnerName(StringUtils.EMPTY);
    brandWip.setBusinessPartnerCode(StringUtils.EMPTY);
    request.setBusinessPartnerName(StringUtils.EMPTY);
    request.setBusinessPartnerCode(StringUtils.EMPTY);
    Mockito.when(this.brandServiceWrapper.createBrand(DEFAULT_STORE_ID, brandWip))
      .thenThrow(ApplicationRuntimeException.class);
    String requestString = BrandWipControllerTest.OBJECT_MAPPER.writeValueAsString(request);
      this.mockMvc.perform(
          post(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.CREATE)
              .contentType(MediaType.APPLICATION_JSON)
              .content(requestString).param("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
              .param("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
              .param("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
              .param("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
              .param("username", BrandWipControllerTest.DEFAULT_USERNAME))
          .andExpect(status().isOk())
          .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
          .andExpect(jsonPath("$.errorMessage", nullValue()))
          .andExpect(jsonPath("$.errorCode", notNullValue()));
    Mockito.verify(this.brandServiceWrapper).createBrand(DEFAULT_STORE_ID, brandWip);
  }

  @Test
  public void getBrandWipDetailTest() throws Exception {
    brandWip.setBrandName(DEFAULT_BRAND_NAME);
    Mockito.when(this.brandWipService.getBrandWipDetail(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE))
        .thenReturn(brandWipResponse);
    this.mockMvc
        .perform(get(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.DETAIL)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
        .param("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .param("username", BrandWipControllerTest.DEFAULT_USERNAME)
            .param("brandRequestCode", BrandWipControllerTest.DEFAULT_BRAND_REQUEST_CODE)
    ).andExpect(status().isOk());
    Mockito.verify(this.brandWipService).getBrandWipDetail(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE);
  }

  @Test
  public void getBrandRejectedInfoResponseTest() throws Exception {
    Mockito.when(this.brandWipService.getBrandRejectionInfoResponse(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE))
        .thenReturn(brandRejectionInfoResponse);
    this.mockMvc
        .perform(get(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.REJECTED_REASON, DEFAULT_BRAND_REQUEST_CODE)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
            .param("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
            .param("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
            .param("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
            .param("username", BrandWipControllerTest.DEFAULT_USERNAME)
        ).andExpect(status().isOk());
    Mockito.verify(this.brandWipService).getBrandRejectionInfoResponse(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE);
  }

  @Test
  public void getBrandRejectedInfoResponseApplicationRuntimeExceptionTest() throws Exception {
    Mockito.when(this.brandWipService.getBrandRejectionInfoResponse(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE))
        .thenThrow(new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND,
            ErrorMessage.BRAND_WIP_NOT_FOUND.getMessage()));
    this.mockMvc.perform(get(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.REJECTED_REASON, DEFAULT_BRAND_REQUEST_CODE)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
        .param("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .param("username", BrandWipControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", CoreMatchers.equalTo(ErrorCategory.DATA_NOT_FOUND.getMessage()
            + ErrorMessage.BRAND_WIP_NOT_FOUND.getMessage())))
        .andExpect(jsonPath("$.errorCode", CoreMatchers.equalTo(ErrorCategory.DATA_NOT_FOUND.getCode())));
    Mockito.verify(this.brandWipService).getBrandRejectionInfoResponse(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE);
  }

  @Test
  public void getBrandRejectedInfoResponseExceptionTest() throws Exception {
    Mockito.when(this.brandWipService.getBrandRejectionInfoResponse(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE))
        .thenThrow(RuntimeException.class);
    this.mockMvc.perform(get(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.REJECTED_REASON, DEFAULT_BRAND_REQUEST_CODE)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
        .param("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .param("username", BrandWipControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", CoreMatchers.equalTo(ErrorCategory.UNSPECIFIED.getMessage())))
        .andExpect(jsonPath("$.errorCode", CoreMatchers.equalTo(ErrorCategory.UNSPECIFIED.getCode())));
    Mockito.verify(this.brandWipService).getBrandRejectionInfoResponse(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE);
  }

  @Test
  public void getBrandWipDetail_expectExceptionTest() throws Exception {
    brandWip.setBrandName(DEFAULT_BRAND_NAME);
    Mockito.when(this.brandWipService.getBrandWipDetail(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE))
        .thenThrow(RuntimeException.class);
    this.mockMvc
        .perform(get(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.DETAIL)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
        .param("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .param("username", BrandWipControllerTest.DEFAULT_USERNAME)
        .param("brandRequestCode", BrandWipControllerTest.DEFAULT_BRAND_REQUEST_CODE)
    ).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(jsonPath("$.errorCode", notNullValue()));
    Mockito.verify(this.brandWipService).getBrandWipDetail(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE);
  }

  @Test
  public void getBrandWipListTest() throws Exception {
    Mockito.when(this.brandWipService.getBrandWipList(brandWipSummaryRequest, PAGE, size))
        .thenReturn(brandWipResponsePage);
    String requestString = BrandWipControllerTest.OBJECT_MAPPER.writeValueAsString(brandWipSummaryRequest);
    this.mockMvc.perform(post(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.FILTER_SUMMARY)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .param("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
        .param("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .param("username", BrandWipControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    Mockito.verify(this.brandWipService).getBrandWipList(brandWipSummaryRequest, PAGE, size);
  }

  @Test
  public void getBrandWipList_expectExceptionTest() throws Exception {
    Mockito.when(this.brandWipService.getBrandWipList(brandWipSummaryRequest, PAGE, size))
        .thenThrow(RuntimeException.class);
    String requestString = BrandWipControllerTest.OBJECT_MAPPER.writeValueAsString(brandWipSummaryRequest);
    this.mockMvc.perform(post(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.FILTER_SUMMARY)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .param("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
        .param("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .param("username", BrandWipControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", notNullValue())).andExpect(jsonPath("$.errorCode", nullValue()));
    Mockito.verify(this.brandWipService).getBrandWipList(brandWipSummaryRequest, PAGE, size);
  }

  public void getBrandWipDetail_expectApplicationRuntimeExceptionTest() throws Exception {
    brandWip.setBrandName(DEFAULT_BRAND_NAME);
    Mockito.when(this.brandWipService.getBrandWipDetail(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE))
        .thenThrow(new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, "No such Brand Wip found"));
    this.mockMvc
        .perform(get(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.DETAIL)
            .contentType(MediaType.APPLICATION_JSON)
            .param("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
            .param("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
            .param("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
            .param("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
            .param("username", BrandWipControllerTest.DEFAULT_USERNAME)
            .param("brandRequestCode", BrandWipControllerTest.DEFAULT_BRAND_REQUEST_CODE)
        ).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.errorCode", notNullValue()));
    Mockito.verify(this.brandWipService).getBrandWipDetail(DEFAULT_STORE_ID, DEFAULT_BRAND_REQUEST_CODE);
  }

  @Test
  public void getBrandWipHistoryTest() throws Exception {
    Mockito.when(this.brandWipService.getBrandWipHistory(brandWipHistorySummaryRequest, PAGE, SIZE))
        .thenReturn(page);
    String requestString = BrandWipControllerTest.OBJECT_MAPPER.writeValueAsString(brandWipHistorySummaryRequest);
    this.mockMvc
        .perform(post(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.HISTORY_FILTER_SUMMARY)
            .contentType(MediaType.APPLICATION_JSON)
            .content(requestString)
            .param("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
            .param("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
            .param("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
            .param("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
            .param("username", BrandWipControllerTest.DEFAULT_USERNAME)
        ).andExpect(status().isOk());
    Mockito.verify(this.brandWipService).getBrandWipHistory(brandWipHistorySummaryRequest, PAGE, SIZE);
  }

  @Test
  public void getBrandWipHistoryTest_expectException() throws Exception {
    Mockito.when(this.brandWipService.getBrandWipHistory(brandWipHistorySummaryRequest, PAGE, SIZE))
        .thenThrow(RuntimeException.class);
    String requestString = BrandWipControllerTest.OBJECT_MAPPER.writeValueAsString(brandWipHistorySummaryRequest);
    this.mockMvc
        .perform(post(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.HISTORY_FILTER_SUMMARY)
            .contentType(MediaType.APPLICATION_JSON)
            .content(requestString)
            .param("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
            .param("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
            .param("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
            .param("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
            .param("username", BrandWipControllerTest.DEFAULT_USERNAME)
        ).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(jsonPath("$.errorCode", nullValue()));;
    Mockito.verify(this.brandWipService).getBrandWipHistory(brandWipHistorySummaryRequest, PAGE, SIZE);
  }

  @Test
  public void getBrandWipHistoryTest_expectApplicationRuntimeException() throws Exception {
    Mockito.when(this.brandWipService.getBrandWipHistory(brandWipHistorySummaryRequest, PAGE, SIZE))
        .thenThrow(new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, DEFAULT_NOT_FOUND_MESSAGE));
    String requestString = BrandWipControllerTest.OBJECT_MAPPER.writeValueAsString(brandWipHistorySummaryRequest);
    this.mockMvc.perform(post(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.HISTORY_FILTER_SUMMARY).contentType(MediaType.APPLICATION_JSON).content(requestString)
        .param("storeId", BrandWipControllerTest.DEFAULT_STORE_ID).param("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID).param("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .param("username", BrandWipControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.errorCode", CoreMatchers.equalTo(ErrorCategory.DATA_NOT_FOUND.getMessage())));
    Mockito.verify(this.brandWipService).getBrandWipHistory(brandWipHistorySummaryRequest, PAGE, SIZE);
  }

  @Test
  public void updateTest() throws Exception {
    doNothing().when(this.brandServiceWrapper).updateBrand(DEFAULT_STORE_ID, brandApproveRequest);
    String requestString = BrandWipControllerTest.OBJECT_MAPPER.writeValueAsString(brandApproveRequest);
    this.mockMvc.perform(post(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.UPDATE)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .param("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
        .param("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .param("username", BrandWipControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk());
    Mockito.verify(brandServiceWrapper, times(1)).updateBrand(DEFAULT_STORE_ID, brandApproveRequest);
  }

  @Test
  public void updateTest_expectException() throws Exception {
    doThrow(Exception.class).when(this.brandServiceWrapper).updateBrand(DEFAULT_STORE_ID, brandApproveRequest);
    String requestString = BrandWipControllerTest.OBJECT_MAPPER.writeValueAsString(brandApproveRequest);
    this.mockMvc.perform(post(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.UPDATE)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .param("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
        .param("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .param("username", BrandWipControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.errorCode", nullValue()));
    Mockito.verify(brandServiceWrapper, times(1)).updateBrand(DEFAULT_STORE_ID, brandApproveRequest);
  }

  @Test
  public void updateTest_expectApplicationRuntimeException() throws Exception {
    doThrow(new ApplicationRuntimeException(ErrorCategory.UNSPECIFIED)).when(this.brandServiceWrapper)
        .updateBrand(DEFAULT_STORE_ID, brandApproveRequest);
    String requestString = BrandWipControllerTest.OBJECT_MAPPER.writeValueAsString(brandApproveRequest);
    this.mockMvc.perform(post(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.UPDATE)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .param("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
        .param("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .param("username", BrandWipControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", notNullValue()))
        .andExpect(jsonPath("$.errorCode", CoreMatchers.equalTo(ErrorCategory.UNSPECIFIED.getMessage())));
    Mockito.verify(brandServiceWrapper, times(1)).updateBrand(DEFAULT_STORE_ID, brandApproveRequest);
  }

  @Test
  public void getBrandWipList_expectApplicationRuntimeExceptionTest() throws Exception {
    Mockito.when(this.brandWipService.getBrandWipList(brandWipSummaryRequest, PAGE, size))
        .thenThrow(new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, "No Brand Wips found"));
    String requestString = BrandWipControllerTest.OBJECT_MAPPER.writeValueAsString(brandWipSummaryRequest);
    this.mockMvc.perform(post(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.FILTER_SUMMARY)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .param("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
        .param("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .param("username", BrandWipControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", notNullValue())).andExpect(jsonPath("$.errorCode", notNullValue()));
    Mockito.verify(this.brandWipService).getBrandWipList(brandWipSummaryRequest, PAGE, size);
  }

  @Test
  public void approveBrandTest() throws Exception {
    when(brandServiceWrapper.approveBrand(brandApproveRequest)).thenReturn(new CreateBrandResponse());
    URI uri = new URIBuilder()
        .setPath(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.APPROVE)
        .addParameter("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandWipControllerTest.DEFAULT_USERNAME).build();
    String requestBody = this.objectMapper.writeValueAsString(brandApproveRequest);
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody)
        .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(brandServiceWrapper).approveBrand(brandApproveRequest);
  }

  @Test
  public void approveBrandExceptionTest() throws Exception {
    when(brandServiceWrapper.approveBrand(brandApproveRequest)).thenThrow(Exception.class);
    URI uri = new URIBuilder()
        .setPath(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.APPROVE)
        .addParameter("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandWipControllerTest.DEFAULT_USERNAME).build();
    String requestBody = this.objectMapper.writeValueAsString(brandApproveRequest);
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody)
        .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(brandServiceWrapper).approveBrand(brandApproveRequest);
  }

  @Test
  public void rejectBrandTest() throws Exception {
    Mockito.when(brandServiceWrapper.rejectBrand(brandRejectRequest)).thenReturn(new BrandWipResponse());
    URI uri = new URIBuilder()
        .setPath(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.REJECT)
        .addParameter("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandWipControllerTest.DEFAULT_USERNAME).build();
    String requestBody = this.objectMapper.writeValueAsString(brandRejectRequest);
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody)
        .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(brandServiceWrapper).rejectBrand(brandRejectRequest);
  }

  @Test
  public void rejectBrandExceptionTest() throws Exception {
    Mockito.when(brandServiceWrapper.rejectBrand(brandRejectRequest)).thenThrow(ApplicationRuntimeException.class);
    URI uri = new URIBuilder()
        .setPath(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.REJECT)
        .addParameter("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandWipControllerTest.DEFAULT_USERNAME).build();
    String requestBody = this.objectMapper.writeValueAsString(brandRejectRequest);
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody)
        .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(brandServiceWrapper).rejectBrand(brandRejectRequest);
  }

  @Test
  public void getBrandWipDetailByBrandCodeTest() throws Exception {
    Mockito.when(brandWipService.getBrandWipDetailByBrandCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE))
        .thenReturn(brandWipResponse);
    URI uri = new URIBuilder().setPath(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.DETAIL_BRAND_CODE)
        .addParameter("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandWipControllerTest.DEFAULT_USERNAME)
        .addParameter("brandCode", BrandWipControllerTest.DEFAULT_BRAND_CODE).build();
    this.mockMvc.perform(get(uri).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(brandWipService).getBrandWipDetailByBrandCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
  }

  @Test
  public void getBrandWipDetailByBrandCodeTest_expectException() throws Exception {
    doThrow(RuntimeException.class).when(brandWipService).getBrandWipDetailByBrandCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    URI uri = new URIBuilder().setPath(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.DETAIL_BRAND_CODE)
        .addParameter("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandWipControllerTest.DEFAULT_USERNAME)
        .addParameter("brandCode", BrandWipControllerTest.DEFAULT_BRAND_CODE).build();
    this.mockMvc.perform(get(uri).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.UNSPECIFIED.getCode())));
    verify(brandWipService).getBrandWipDetailByBrandCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
  }

  @Test
  public void getBrandWipDetailByBrandCodeTest_expectApplicationRuntimeException() throws Exception {
    doThrow(new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, "No such Brand Wip found"))
        .when(brandWipService).getBrandWipDetailByBrandCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
    URI uri = new URIBuilder().setPath(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.DETAIL_BRAND_CODE)
        .addParameter("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandWipControllerTest.DEFAULT_USERNAME)
        .addParameter("brandCode", BrandWipControllerTest.DEFAULT_BRAND_CODE).build();
    this.mockMvc.perform(get(uri).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", equalTo("Can not find data :No such Brand Wip found")))
        .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.DATA_NOT_FOUND.toString())));
    verify(brandWipService).getBrandWipDetailByBrandCode(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE);
  }

  @Test
  public void findByBrandNameAndBusinessPartnerCodeTest() throws Exception {
    Mockito.when(
        this.brandWipService.findByBrandNameAndBusinessPartnerCode(DEFAULT_BRAND_NAME, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenReturn(new BrandWipResponse());
    this.mockMvc.perform(get(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.FILTER_BRAND_NAME)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
        .param("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .param("username", BrandWipControllerTest.DEFAULT_USERNAME)
        .param("brandName", BrandWipControllerTest.DEFAULT_BRAND_NAME)
        .param("businessPartnerCode", BrandWipControllerTest.DEFAULT_BUSINESS_PARTNER_CODE))
    .andExpect(status().isOk());
    Mockito.verify(this.brandWipService)
        .findByBrandNameAndBusinessPartnerCode(DEFAULT_BRAND_NAME, DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void findByBrandNameAndBusinessPartnerCode_expectException() throws Exception {
    Mockito.when(
        this.brandWipService.findByBrandNameAndBusinessPartnerCode(DEFAULT_BRAND_NAME, DEFAULT_BUSINESS_PARTNER_CODE))
        .thenThrow(new ApplicationException());
    this.mockMvc.perform(get(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.FILTER_BRAND_NAME)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
        .param("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .param("username", BrandWipControllerTest.DEFAULT_USERNAME)
        .param("brandName", BrandWipControllerTest.DEFAULT_BRAND_NAME)
        .param("businessPartnerCode", BrandWipControllerTest.DEFAULT_BUSINESS_PARTNER_CODE))
        .andExpect(status().isOk());
    Mockito.verify(this.brandWipService)
        .findByBrandNameAndBusinessPartnerCode(DEFAULT_BRAND_NAME, DEFAULT_BUSINESS_PARTNER_CODE);
  }

  @Test
  public void filterByBrandRequestCodeTest() throws Exception {
    Mockito.when(this.brandWipService.filterByBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE))
        .thenReturn(new BrandWipResponse());
    URI uri = new URIBuilder()
        .setPath(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.FILTER_BRAND_REQUEST_CODE)
        .addParameter("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandWipControllerTest.DEFAULT_USERNAME)
        .addParameter("brandRequestCode", BrandWipControllerTest.DEFAULT_BRAND_REQUEST_CODE).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri)
        .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.brandWipService).filterByBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
  }

  @Test
  public void filterByBrandRequestCodeTest_expectException() throws Exception {
    Mockito.when(this.brandWipService.filterByBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE))
        .thenThrow(new NullPointerException());
    URI uri = new URIBuilder()
        .setPath(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.FILTER_BRAND_REQUEST_CODE)
        .addParameter("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandWipControllerTest.DEFAULT_USERNAME)
        .addParameter("brandRequestCode", BrandWipControllerTest.DEFAULT_BRAND_REQUEST_CODE).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri)
        .contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk());
    Mockito.verify(this.brandWipService).filterByBrandRequestCode(DEFAULT_BRAND_REQUEST_CODE);
  }

  @Test
  public void getAllInReviewBrandsTest() throws Exception {
    Mockito.when(brandWipService.getAllInReviewBrands(DEFAULT_STORE_ID))
      .thenReturn(Arrays.asList(brandInReviewResponse));
    URI uri = new URIBuilder().setPath(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.IN_REVIEW_BRANDS)
      .addParameter("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
      .addParameter("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
      .addParameter("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
      .addParameter("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
      .addParameter("username", BrandWipControllerTest.DEFAULT_USERNAME).build();
    this.mockMvc.perform(get(uri).contentType(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(brandWipService).getAllInReviewBrands(DEFAULT_STORE_ID);
  }

  @Test
  public void getAllInReviewBrandsTest_expectException() throws Exception {
    doThrow(RuntimeException.class).when(brandWipService).getAllInReviewBrands(DEFAULT_STORE_ID);
    URI uri = new URIBuilder().setPath(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.IN_REVIEW_BRANDS)
      .addParameter("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
      .addParameter("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
      .addParameter("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
      .addParameter("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
      .addParameter("username", BrandWipControllerTest.DEFAULT_USERNAME).build();
    this.mockMvc.perform(get(uri).contentType(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.errorMessage", notNullValue()))
      .andExpect(jsonPath("$.errorCode", equalTo(null)));
    verify(brandWipService).getAllInReviewBrands(DEFAULT_STORE_ID);
  }

  @Test
  public void getAllInReviewBrandsTest_expectApplicationRuntimeException() throws Exception {
    doThrow(new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, "No such Brand Wip found"))
      .when(brandWipService).getAllInReviewBrands(DEFAULT_STORE_ID);
    URI uri = new URIBuilder().setPath(BrandWipControllerPath.BASE_PATH + BrandWipControllerPath.IN_REVIEW_BRANDS)
      .addParameter("storeId", BrandWipControllerTest.DEFAULT_STORE_ID)
      .addParameter("channelId", BrandWipControllerTest.DEFAULT_CHANNEL_ID)
      .addParameter("clientId", BrandWipControllerTest.DEFAULT_CLIENT_ID)
      .addParameter("requestId", BrandWipControllerTest.DEFAULT_REQUEST_ID)
      .addParameter("username", BrandWipControllerTest.DEFAULT_USERNAME).build();
    this.mockMvc.perform(get(uri).contentType(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
      .andExpect(jsonPath("$.errorMessage", equalTo("Can not find data :No such Brand Wip found")))
      .andExpect(jsonPath("$.errorCode", equalTo(ErrorCategory.DATA_NOT_FOUND.toString())));
    verify(brandWipService).getAllInReviewBrands(DEFAULT_STORE_ID);
  }
}
