package com.gdn.x.productcategorybase.controller.brand;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.UUID;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.BrandAuthorisationStatus;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthBulkDownloadRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthBulkDownloadResponse;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateRequest;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthDeleteRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthHistoryResponse;
import com.gdn.x.productcategorybase.dto.brand.ProductBrandValidationRequest;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import com.gdn.x.productcategorybase.dto.response.BrandAuthorisationDetailResponse;
import com.gdn.x.productcategorybase.service.brand.BrandAuthHistoryService;
import com.gdn.x.productcategorybase.service.brand.BrandAuthServiceWrapper;
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
import org.mockito.verification.VerificationMode;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import com.gdn.x.productcategorybase.BrandAuthorisationPath;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthFilterRequest;
import com.gdn.x.productcategorybase.service.brand.BrandAuthorisationService;

public class BrandAuthorisationControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "API";
  private static final String DEFAULT_CLIENT_ID = "TEST";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_USERNAME = "DEVELOPER";
  private static final String DEFAULT_BRAND_CODE = "BRD-00001";
  private static final String DEFAULT_SELLER_CODE = "BRD-00001";
  private static final String DEFAULT_BRAND_NAME = "TEST-BRAND";

  private static final String FILTER_BRAND_CODE_PATH = "/BRD-00001/valid";
  public static final String BRAND_CODE = "BRD-00001";
  private static final String AUTH_DETAIL_API_PATH = "/detail/" + DEFAULT_BRAND_CODE;
  private static final String DELETE_API_PATH = "/delete";
  private static final String OLD_VALUE_AUTH = "Inactive";
  private static final String NEW_VALUE_AUTH = "Active";
  private static final String ACTIVITY_AUTH = "Change status";
  private static final String DEFAULT_NOT_FOUND_MESSAGE = "Required data not found";
  private static final int PAGE = 0;
  private static final int SIZE = 10;

  @Mock
  private BrandAuthorisationService brandAuthorisationService;

  @InjectMocks
  private BrandAuthorisationController brandAuthorisationController;

  @Mock
  private BrandAuthHistoryService brandAuthHistoryService;

  @Mock
  private BrandAuthServiceWrapper brandAuthServiceWrapper;

  private MockMvc mockMvc;
  private ObjectMapper objectMapper;
  private BrandAuthHistoryRequest brandAuthHistoryRequest;
  private BrandAuthHistoryResponse brandAuthHistoryResponse;
  private Page<BrandAuthHistoryResponse> page;
  private List<BrandAuthHistoryResponse> brandHistoryResponseList = new ArrayList<>();
  private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();
  private BrandAuthBulkDownloadResponse brandAuthBulkDownloadResponse;
  private BrandAuthBulkDownloadRequest brandAuthBulkDownloadRequest;
  private BrandAuthDeleteRequest brandAuthDeleteRequest;
  private List<BrandAuthDeleteRequest> brandAuthDeleteRequestList;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.mockMvc = standaloneSetup(this.brandAuthorisationController).build();
    this.objectMapper = new ObjectMapper(new JsonFactory());
    brandAuthHistoryRequest = new BrandAuthHistoryRequest();
    brandAuthHistoryRequest.setSellerCode(DEFAULT_SELLER_CODE);
    brandAuthHistoryRequest.setBrandCode(DEFAULT_BRAND_CODE);

    brandAuthHistoryResponse = new BrandAuthHistoryResponse();
    brandAuthHistoryResponse.setBrandCode(DEFAULT_BRAND_CODE);
    brandAuthHistoryResponse.setSellerCode(DEFAULT_SELLER_CODE);
    brandAuthHistoryResponse.setActivity(ACTIVITY_AUTH);
    brandAuthHistoryResponse.setOldStatus(OLD_VALUE_AUTH);
    brandAuthHistoryResponse.setNewStatus(NEW_VALUE_AUTH);
    brandHistoryResponseList.add(brandAuthHistoryResponse);
    page = new PageImpl<>(brandHistoryResponseList);

    brandAuthBulkDownloadResponse = new BrandAuthBulkDownloadResponse();
    brandAuthBulkDownloadRequest = new BrandAuthBulkDownloadRequest();
    brandAuthBulkDownloadResponse.setBrandName(DEFAULT_BRAND_NAME);
    brandAuthBulkDownloadResponse.setBrandCode(DEFAULT_BRAND_CODE);
    brandAuthBulkDownloadResponse.setSellerCode(DEFAULT_SELLER_CODE);
    brandAuthBulkDownloadResponse.setAuthStartDate(new Date(2022,01,01));
    brandAuthBulkDownloadResponse.setAuthExpireDate(new Date(2022,12,01));

    brandAuthDeleteRequest = new BrandAuthDeleteRequest();
    brandAuthDeleteRequest.setBrandCode(DEFAULT_BRAND_CODE);
    brandAuthDeleteRequest.setSellerCode(DEFAULT_SELLER_CODE);

    brandAuthDeleteRequestList = new ArrayList<>();
    brandAuthDeleteRequestList.add(brandAuthDeleteRequest);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(brandAuthorisationService);
    Mockito.verifyNoMoreInteractions(brandAuthServiceWrapper);
  }

  @Test
  public void checkBrandAuthBySellerCodeTest() throws Exception {
    when(this.brandAuthorisationService.checkBrandAuthBySellerCode(DEFAULT_STORE_ID, DEFAULT_SELLER_CODE,
        DEFAULT_BRAND_CODE, false)).thenReturn(false);
    URI uri = new URIBuilder().setPath(
            BrandAuthorisationPath.BASE_PATH + BrandAuthorisationControllerTest.FILTER_BRAND_CODE_PATH)
        .addParameter("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandAuthorisationControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandAuthorisationControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandAuthorisationControllerTest.DEFAULT_USERNAME)
        .addParameter("brandCode", BRAND_CODE).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandAuthorisationService)
        .checkBrandAuthBySellerCode(DEFAULT_STORE_ID, DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE,
          false);
  }

  @Test
  public void checkBrandAuthBySellerCodeExceptionTest() throws Exception {
    when(this.brandAuthorisationService.checkBrandAuthBySellerCode(DEFAULT_STORE_ID, DEFAULT_SELLER_CODE,
        DEFAULT_BRAND_CODE, false)).thenThrow(Exception.class);
    URI uri = new URIBuilder().setPath(
            BrandAuthorisationPath.BASE_PATH + BrandAuthorisationControllerTest.FILTER_BRAND_CODE_PATH)
        .addParameter("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandAuthorisationControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandAuthorisationControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandAuthorisationControllerTest.DEFAULT_USERNAME)
        .addParameter("brandCode", BRAND_CODE).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.brandAuthorisationService)
        .checkBrandAuthBySellerCode(DEFAULT_STORE_ID, DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE,
          false);
  }

  @Test
  public void getAuthTest() throws Exception {
    when(this.brandAuthorisationService.findBrandAuthorisationByFilter(eq(DEFAULT_STORE_ID), Mockito.any(
        BrandAuthFilterRequest.class), eq(0), eq(10))).thenReturn(new PageImpl<>(new ArrayList<>()));
    URI uri = new URIBuilder().setPath(
            BrandAuthorisationPath.BASE_PATH + BrandAuthorisationPath.GET_AUTHORISATIONS)
        .addParameter("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandAuthorisationControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandAuthorisationControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandAuthorisationControllerTest.DEFAULT_USERNAME)
        .addParameter("page", "0")
        .addParameter("size", "10").build();
    String requestBody = this.objectMapper.writeValueAsString(new BrandAuthFilterRequest());
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandAuthorisationService)
        .findBrandAuthorisationByFilter(eq(DEFAULT_STORE_ID), Mockito.any(
            BrandAuthFilterRequest.class), eq(0), eq(10));
  }

  @Test
  public void getAuthTestException() throws Exception {
    when(this.brandAuthorisationService.findBrandAuthorisationByFilter(eq(DEFAULT_STORE_ID), Mockito.any(
        BrandAuthFilterRequest.class), eq(0), eq(10))).thenThrow(RuntimeException.class);
    URI uri = new URIBuilder().setPath(
            BrandAuthorisationPath.BASE_PATH + BrandAuthorisationPath.GET_AUTHORISATIONS)
        .addParameter("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandAuthorisationControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandAuthorisationControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandAuthorisationControllerTest.DEFAULT_USERNAME)
        .addParameter("page", "0")
        .addParameter("size", "10").build();
    String requestBody = this.objectMapper.writeValueAsString(new BrandAuthFilterRequest());
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.brandAuthorisationService)
        .findBrandAuthorisationByFilter(eq(DEFAULT_STORE_ID), Mockito.any(
            BrandAuthFilterRequest.class), eq(0), eq(10));
  }

  @Test
  public void checkTakeDownBasedOnBrandTest() throws Exception {
    ProductBrandValidationRequest productBrandValidationRequest = new ProductBrandValidationRequest();
    String request = objectMapper.writeValueAsString(productBrandValidationRequest);
    when(brandAuthorisationService.takeDownProductBasedOnBrand(DEFAULT_STORE_ID,
        productBrandValidationRequest)).thenReturn(false);
    this.mockMvc.perform(
            post(BrandAuthorisationPath.BASE_PATH + BrandAuthorisationPath.TAKE_DOWN_BASED_ON_BRAND).contentType(
                    MediaType.APPLICATION_JSON).content(request)
                .param("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
                .param("channelId", BrandAuthorisationControllerTest.DEFAULT_CHANNEL_ID)
                .param("clientId", BrandAuthorisationControllerTest.DEFAULT_CLIENT_ID)
                .param("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
                .param("username", BrandAuthorisationControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(true))).andExpect(jsonPath("$.errorMessage", nullValue()))
        .andExpect(jsonPath("$.errorCode", nullValue()));
    Mockito.verify(brandAuthorisationService)
        .takeDownProductBasedOnBrand(DEFAULT_STORE_ID, productBrandValidationRequest);
  }

  @Test
  public void checkTakeDownBasedOnBrandExceptionTest() throws Exception {
    ProductBrandValidationRequest productBrandValidationRequest = new ProductBrandValidationRequest();
    String request = objectMapper.writeValueAsString(productBrandValidationRequest);
    when(brandAuthorisationService.takeDownProductBasedOnBrand(DEFAULT_STORE_ID,
        productBrandValidationRequest)).thenThrow(Exception.class);
    this.mockMvc.perform(
            post(BrandAuthorisationPath.BASE_PATH + BrandAuthorisationPath.TAKE_DOWN_BASED_ON_BRAND).contentType(
                    MediaType.APPLICATION_JSON).content(request)
                .param("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
                .param("channelId", BrandAuthorisationControllerTest.DEFAULT_CHANNEL_ID)
                .param("clientId", BrandAuthorisationControllerTest.DEFAULT_CLIENT_ID)
                .param("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
                .param("username", BrandAuthorisationControllerTest.DEFAULT_USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(jsonPath("$.errorMessage", nullValue())).andExpect(jsonPath("$.errorCode", notNullValue()));
    Mockito.verify(brandAuthorisationService)
        .takeDownProductBasedOnBrand(DEFAULT_STORE_ID, productBrandValidationRequest);
  }

  @Test
  public void getBrandAuthorisationDetailByCodeTest() throws Exception {
    when(this.brandAuthorisationService.getBrandAuthDetailByBrandCodeAndSellerCode(DEFAULT_STORE_ID,
      DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE)).thenReturn(new BrandAuthorisationDetailResponse());
    URI uri = new URIBuilder().setPath(
        BrandAuthorisationPath.BASE_PATH + BrandAuthorisationControllerTest.AUTH_DETAIL_API_PATH)
      .addParameter("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
      .addParameter("channelId", BrandAuthorisationControllerTest.DEFAULT_CHANNEL_ID)
      .addParameter("clientId", BrandAuthorisationControllerTest.DEFAULT_CLIENT_ID)
      .addParameter("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
      .addParameter("username", BrandAuthorisationControllerTest.DEFAULT_USERNAME)
      .addParameter("sellerCode", DEFAULT_SELLER_CODE).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandAuthorisationService)
      .getBrandAuthDetailByBrandCodeAndSellerCode(DEFAULT_STORE_ID, DEFAULT_SELLER_CODE,
        DEFAULT_BRAND_CODE);
  }

  @Test
  public void getBrandAuthorisationDetailByCodeExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(this.brandAuthorisationService)
      .getBrandAuthDetailByBrandCodeAndSellerCode(DEFAULT_STORE_ID, DEFAULT_SELLER_CODE,
        DEFAULT_BRAND_CODE);
    URI uri = new URIBuilder().setPath(
        BrandAuthorisationPath.BASE_PATH + BrandAuthorisationControllerTest.AUTH_DETAIL_API_PATH)
      .addParameter("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
      .addParameter("channelId", BrandAuthorisationControllerTest.DEFAULT_CHANNEL_ID)
      .addParameter("clientId", BrandAuthorisationControllerTest.DEFAULT_CLIENT_ID)
      .addParameter("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
      .addParameter("username", BrandAuthorisationControllerTest.DEFAULT_USERNAME)
      .addParameter("sellerCode", DEFAULT_SELLER_CODE).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.brandAuthorisationService)
      .getBrandAuthDetailByBrandCodeAndSellerCode(DEFAULT_STORE_ID, DEFAULT_SELLER_CODE,
        DEFAULT_BRAND_CODE);
  }

  @Test
  public void getBrandAuthorisationDetailByCodeNullTest() throws Exception {
    when(this.brandAuthorisationService.getBrandAuthDetailByBrandCodeAndSellerCode(DEFAULT_STORE_ID,
      DEFAULT_SELLER_CODE, DEFAULT_BRAND_CODE)).thenReturn(null);
    URI uri = new URIBuilder().setPath(
        BrandAuthorisationPath.BASE_PATH + BrandAuthorisationControllerTest.AUTH_DETAIL_API_PATH)
      .addParameter("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
      .addParameter("channelId", BrandAuthorisationControllerTest.DEFAULT_CHANNEL_ID)
      .addParameter("clientId", BrandAuthorisationControllerTest.DEFAULT_CLIENT_ID)
      .addParameter("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
      .addParameter("username", BrandAuthorisationControllerTest.DEFAULT_USERNAME)
      .addParameter("sellerCode", DEFAULT_SELLER_CODE).build();
    this.mockMvc.perform(MockMvcRequestBuilders.get(uri))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandAuthorisationService)
      .getBrandAuthDetailByBrandCodeAndSellerCode(DEFAULT_STORE_ID, DEFAULT_SELLER_CODE,
        DEFAULT_BRAND_CODE);
  }

  @Test
  public void createTest() throws Exception {
    URI uri =
      new URIBuilder().setPath(BrandAuthorisationPath.BASE_PATH + BrandAuthorisationPath.CREATE)
        .addParameter("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandAuthorisationControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandAuthorisationControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandAuthorisationControllerTest.DEFAULT_USERNAME).build();
    String requestBody =
      this.objectMapper.writeValueAsString(this.generateCreateBrandAuthRequest());
    this.mockMvc.perform(
      MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandAuthServiceWrapper)
      .createBrandAuthAndEvictCache(Mockito.any(BrandAuthCreateRequest.class), Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  public void createWithBrandNameExceptionTest() throws Exception {
    VerificationMode NEVER_CALLED = Mockito.times(0);
    Mockito.doThrow(Exception.class).when(this.brandAuthServiceWrapper)
      .createBrandAuthAndEvictCache(Mockito.any(BrandAuthCreateRequest.class), Mockito.anyString(),
        Mockito.anyString());
    URI uri =
      new URIBuilder().setPath(BrandAuthorisationPath.BASE_PATH + BrandAuthorisationPath.CREATE)
        .addParameter("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandAuthorisationControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandAuthorisationControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandAuthorisationControllerTest.DEFAULT_USERNAME).build();
    BrandAuthCreateRequest request = this.generateCreateBrandAuthRequest();
    request.setBrandName(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    this.mockMvc.perform(
      MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.brandAuthServiceWrapper)
      .createBrandAuthAndEvictCache(Mockito.any(BrandAuthCreateRequest.class), Mockito.anyString(), Mockito.anyString());
  }

  private BrandAuthCreateRequest generateCreateBrandAuthRequest() throws Exception {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, 100);
    Date expireDate = calendar.getTime();
    BrandAuthCreateRequest request = new BrandAuthCreateRequest();
    request.setBrandName(BrandAuthorisationControllerTest.DEFAULT_BRAND_NAME);
    request.setBrandCode(BrandAuthorisationControllerTest.DEFAULT_BRAND_CODE);
    request.setSellerCode(BrandAuthorisationControllerTest.DEFAULT_SELLER_CODE);
    request.setAuthExpireDate(expireDate);
    request.setAuthStartDate(new Date());
    request.setAuthorisationStatus(BrandAuthorisationStatus.ACTIVE.getValue());
    return request;
  }

  @Test
  public void deleteBrandAuthTest() throws Exception {
    URI deleteUri = new URIBuilder().setPath(
        BrandAuthorisationPath.BASE_PATH + BrandAuthorisationControllerTest.DELETE_API_PATH)
      .addParameter("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
      .addParameter("channelId", BrandAuthorisationControllerTest.DEFAULT_CHANNEL_ID)
      .addParameter("clientId", BrandAuthorisationControllerTest.DEFAULT_CLIENT_ID)
      .addParameter("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
      .addParameter("username", BrandAuthorisationControllerTest.DEFAULT_USERNAME)
      .build();
    String requestBody = BrandAuthorisationControllerTest.OBJECT_MAPPER.writeValueAsString(brandAuthDeleteRequestList);
    this.mockMvc.perform(MockMvcRequestBuilders.post(deleteUri).contentType(MediaType.APPLICATION_JSON).content(requestBody))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(Boolean.TRUE)));
    verify(this.brandAuthServiceWrapper)
        .deleteBrandAuthAndEvictCache(DEFAULT_STORE_ID, DEFAULT_USERNAME, brandAuthDeleteRequestList);
  }

  @Test()
  public void deleteBrandAuth_ApplicationExceptionTest() throws Exception {
    doThrow(new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND)).when(
        this.brandAuthServiceWrapper)
        .deleteBrandAuthAndEvictCache(DEFAULT_STORE_ID, DEFAULT_USERNAME, brandAuthDeleteRequestList);
    URI deleteUri = new URIBuilder().setPath(
        BrandAuthorisationPath.BASE_PATH + BrandAuthorisationControllerTest.DELETE_API_PATH)
      .addParameter("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
      .addParameter("channelId", BrandAuthorisationControllerTest.DEFAULT_CHANNEL_ID)
      .addParameter("clientId", BrandAuthorisationControllerTest.DEFAULT_CLIENT_ID)
      .addParameter("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
      .addParameter("username", BrandAuthorisationControllerTest.DEFAULT_USERNAME)
      .build();
    String requestBody = BrandAuthorisationControllerTest.OBJECT_MAPPER.writeValueAsString(brandAuthDeleteRequestList);
    this.mockMvc.perform(MockMvcRequestBuilders.post(deleteUri).contentType(MediaType.APPLICATION_JSON).content(requestBody))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(Boolean.FALSE))).andExpect(jsonPath("$.errorMessage", equalTo(
      ErrorCategory.DATA_NOT_FOUND.getMessage())));
    verify(this.brandAuthServiceWrapper)
        .deleteBrandAuthAndEvictCache(DEFAULT_STORE_ID, DEFAULT_USERNAME, brandAuthDeleteRequestList);
  }

  @Test()
  public void deleteBrandAuth_ExceptionTest() throws Exception {
    doThrow(Exception.class).when(this.brandAuthServiceWrapper)
        .deleteBrandAuthAndEvictCache(DEFAULT_STORE_ID, DEFAULT_USERNAME, brandAuthDeleteRequestList);
    URI deleteUri = new URIBuilder().setPath(
        BrandAuthorisationPath.BASE_PATH + BrandAuthorisationControllerTest.DELETE_API_PATH)
      .addParameter("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
      .addParameter("channelId", BrandAuthorisationControllerTest.DEFAULT_CHANNEL_ID)
      .addParameter("clientId", BrandAuthorisationControllerTest.DEFAULT_CLIENT_ID)
      .addParameter("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
      .addParameter("username", BrandAuthorisationControllerTest.DEFAULT_USERNAME)
      .build();
    String requestBody = BrandAuthorisationControllerTest.OBJECT_MAPPER.writeValueAsString(brandAuthDeleteRequestList);
    this.mockMvc.perform(MockMvcRequestBuilders.post(deleteUri).contentType(MediaType.APPLICATION_JSON).content(requestBody))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)));
    verify(this.brandAuthServiceWrapper)
        .deleteBrandAuthAndEvictCache(DEFAULT_STORE_ID, DEFAULT_USERNAME, brandAuthDeleteRequestList);
  }

  @Test
  public void getBrandWipHistoryTest() throws Exception {
    Mockito.when(this.brandAuthHistoryService
      .getBrandAuthHistory(DEFAULT_STORE_ID, brandAuthHistoryRequest, PAGE, SIZE)).thenReturn(page);
    String requestString =
      BrandAuthorisationControllerTest.OBJECT_MAPPER.writeValueAsString(brandAuthHistoryResponse);
    this.mockMvc.perform(
      post(BrandAuthorisationPath.BASE_PATH + BrandAuthorisationPath.HISTORY_FILTER_SUMMARY)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .param("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", BrandAuthorisationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", BrandAuthorisationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", BrandAuthorisationControllerTest.DEFAULT_USERNAME))
      .andExpect(status().isOk());
    Mockito.verify(this.brandAuthHistoryService)
      .getBrandAuthHistory(DEFAULT_STORE_ID, brandAuthHistoryRequest, PAGE, SIZE);
  }

  @Test
  public void getBrandAuthHistoryTest_expectException() throws Exception {
    Mockito.when(this.brandAuthHistoryService
      .getBrandAuthHistory(DEFAULT_STORE_ID, brandAuthHistoryRequest, PAGE, SIZE))
      .thenThrow(RuntimeException.class);
    String requestString =
      BrandAuthorisationControllerTest.OBJECT_MAPPER.writeValueAsString(brandAuthHistoryResponse);
    this.mockMvc.perform(
      post(BrandAuthorisationPath.BASE_PATH + BrandAuthorisationPath.HISTORY_FILTER_SUMMARY)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .param("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", BrandAuthorisationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", BrandAuthorisationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", BrandAuthorisationControllerTest.DEFAULT_USERNAME))
      .andExpect(status().isOk()).andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
      .andExpect(jsonPath("$.errorMessage", nullValue()))
      .andExpect(jsonPath("$.errorCode", nullValue()));
    ;
    Mockito.verify(this.brandAuthHistoryService)
      .getBrandAuthHistory(DEFAULT_STORE_ID, brandAuthHistoryRequest, PAGE, SIZE);
  }

  @Test
  public void getBrandWipHistoryTest_expectApplicationRuntimeException() throws Exception {
    Mockito.when(this.brandAuthHistoryService
      .getBrandAuthHistory(DEFAULT_STORE_ID, brandAuthHistoryRequest, PAGE, SIZE)).thenThrow(
      new ApplicationRuntimeException(ErrorCategory.DATA_NOT_FOUND, DEFAULT_NOT_FOUND_MESSAGE));
    String requestString =
      BrandAuthorisationControllerTest.OBJECT_MAPPER.writeValueAsString(brandAuthHistoryResponse);
    this.mockMvc.perform(
      post(BrandAuthorisationPath.BASE_PATH + BrandAuthorisationPath.HISTORY_FILTER_SUMMARY)
        .contentType(MediaType.APPLICATION_JSON).content(requestString)
        .param("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
        .param("channelId", BrandAuthorisationControllerTest.DEFAULT_CHANNEL_ID)
        .param("clientId", BrandAuthorisationControllerTest.DEFAULT_CLIENT_ID)
        .param("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
        .param("username", BrandAuthorisationControllerTest.DEFAULT_USERNAME))
      .andExpect(status().isOk()).andExpect(jsonPath("$.success", CoreMatchers.equalTo(false)))
      .andExpect(jsonPath("$.errorMessage", notNullValue())).andExpect(
      jsonPath("$.errorCode", CoreMatchers.equalTo(ErrorCategory.DATA_NOT_FOUND.getMessage())));
    Mockito.verify(this.brandAuthHistoryService)
      .getBrandAuthHistory(DEFAULT_STORE_ID, brandAuthHistoryRequest, PAGE, SIZE);
  }


  @Test
  public void getBrandAuthBulkTest() throws Exception {
    List<String> ids = Arrays.asList("id1", "id2");
    brandAuthBulkDownloadRequest.setIds(ids);
    Mockito.when(this.brandAuthorisationService.getBrandAuthBulkResponse(DEFAULT_STORE_ID, ids))
      .thenReturn(Arrays.asList(brandAuthBulkDownloadResponse));
    URI uri = new URIBuilder()
      .setPath(BrandAuthorisationPath.BASE_PATH + BrandAuthorisationPath.GET_BRAND_AUTH_BY_IDS)
      .addParameter("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
      .addParameter("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
      .addParameter("page", "0").addParameter("size", "10").build();
    String requestBody = this.objectMapper.writeValueAsString(brandAuthBulkDownloadRequest);
    this.mockMvc.perform(
      MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandAuthorisationService).getBrandAuthBulkResponse(DEFAULT_STORE_ID, ids);
  }

  @Test
  public void getBrandAuthBulkEmptyResponseTest() throws Exception {
    List<String> ids = Arrays.asList("id1", "id2");
    brandAuthBulkDownloadRequest.setIds(ids);
    Mockito.when(this.brandAuthorisationService.getBrandAuthBulkResponse(DEFAULT_STORE_ID, ids))
      .thenReturn(new ArrayList<>());
    URI uri = new URIBuilder()
      .setPath(BrandAuthorisationPath.BASE_PATH + BrandAuthorisationPath.GET_BRAND_AUTH_BY_IDS)
      .addParameter("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
      .addParameter("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
      .addParameter("page", "0").addParameter("size", "10").build();
    String requestBody = this.objectMapper.writeValueAsString(brandAuthBulkDownloadRequest);
    this.mockMvc.perform(
      MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandAuthorisationService).getBrandAuthBulkResponse(DEFAULT_STORE_ID, ids);
  }

  @Test
  public void getBrandAuthBulkResponseExceptionTest() throws Exception {
    List<String> ids = Arrays.asList("id1", "id2");
    brandAuthBulkDownloadRequest.setIds(ids);
    Mockito.doThrow(RuntimeException.class).when(brandAuthorisationService).getBrandAuthBulkResponse(DEFAULT_STORE_ID,ids);
    /*Mockito.when(this.brandAuthorisationService.getBrandAuthBulkResponse(DEFAULT_STORE_ID, ids))
      .thenReturn(new ArrayList<>());*/
    URI uri = new URIBuilder()
      .setPath(BrandAuthorisationPath.BASE_PATH + BrandAuthorisationPath.GET_BRAND_AUTH_BY_IDS)
      .addParameter("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
      .addParameter("channelId", BrandAuthorisationControllerTest.DEFAULT_CHANNEL_ID)
      .addParameter("clientId", BrandAuthorisationControllerTest.DEFAULT_CLIENT_ID)
      .addParameter("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
      .addParameter("username", BrandAuthorisationControllerTest.DEFAULT_USERNAME)
      .addParameter("page", "0").addParameter("size", "10").build();
    String requestBody = this.objectMapper.writeValueAsString(brandAuthBulkDownloadRequest);
    this.mockMvc.perform(
      MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.brandAuthorisationService).getBrandAuthBulkResponse(DEFAULT_STORE_ID, ids);
  }

  @Test
  public void updateTest() throws Exception {
    BrandAuthUpdateRequest brandAuthUpdateRequest =
      BrandAuthUpdateRequest.builder().brandCode(BRAND_CODE).sellerCode(DEFAULT_SELLER_CODE)
        .authorisationStatus(BrandAuthorisationStatus.ACTIVE.name()).authStartDate(new Date())
        .authExpireDate(new Date()).build();
    Mockito.doNothing().when(this.brandAuthServiceWrapper)
        .editBrandAuthDetailsAndEvictCache(DEFAULT_STORE_ID, brandAuthUpdateRequest,
            DEFAULT_USERNAME);
    URI update =
      new URIBuilder().setPath(BrandAuthorisationPath.BASE_PATH + BrandAuthorisationPath.UPDATE)
        .addParameter("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandAuthorisationControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandAuthorisationControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandAuthorisationControllerTest.DEFAULT_USERNAME)
        .addParameter("sellerCode", DEFAULT_SELLER_CODE).build();
    String updateRequest = objectMapper.writeValueAsString(brandAuthUpdateRequest);
    this.mockMvc.perform(MockMvcRequestBuilders.post(update).content(updateRequest)
        .contentType(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(Boolean.TRUE)));
    verify(brandAuthServiceWrapper).editBrandAuthDetailsAndEvictCache(DEFAULT_STORE_ID,
        brandAuthUpdateRequest, DEFAULT_USERNAME);
  }

  @Test
  public void updateApplicationExceptionTest() throws Exception {
    BrandAuthUpdateRequest brandAuthUpdateRequest =
      BrandAuthUpdateRequest.builder().brandCode(BRAND_CODE).sellerCode(DEFAULT_SELLER_CODE)
        .authorisationStatus(BrandAuthorisationStatus.ACTIVE.name()).authStartDate(new Date())
        .authExpireDate(new Date()).build();
    Mockito.doThrow(new ApplicationRuntimeException()).when(this.brandAuthServiceWrapper)
        .editBrandAuthDetailsAndEvictCache(DEFAULT_STORE_ID, brandAuthUpdateRequest,
            DEFAULT_USERNAME);
    URI update =
      new URIBuilder().setPath(BrandAuthorisationPath.BASE_PATH + BrandAuthorisationPath.UPDATE)
        .addParameter("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandAuthorisationControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandAuthorisationControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandAuthorisationControllerTest.DEFAULT_USERNAME)
        .addParameter("sellerCode", DEFAULT_SELLER_CODE).build();
    String updateRequest = objectMapper.writeValueAsString(brandAuthUpdateRequest);
    try {
      this.mockMvc.perform(MockMvcRequestBuilders.post(update).content(updateRequest)
          .contentType(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)));
    } finally {
      verify(brandAuthServiceWrapper).editBrandAuthDetailsAndEvictCache(DEFAULT_STORE_ID,
        brandAuthUpdateRequest, DEFAULT_USERNAME);
    }
  }

  @Test
  public void updateExceptionTest() throws Exception {
    BrandAuthUpdateRequest brandAuthUpdateRequest =
      BrandAuthUpdateRequest.builder().brandCode(BRAND_CODE).sellerCode(DEFAULT_SELLER_CODE)
        .authorisationStatus(BrandAuthorisationStatus.ACTIVE.name()).authStartDate(new Date())
        .authExpireDate(new Date()).build();
    Mockito.doThrow(new Exception()).when(this.brandAuthServiceWrapper)
        .editBrandAuthDetailsAndEvictCache(DEFAULT_STORE_ID, brandAuthUpdateRequest,
            DEFAULT_USERNAME);
    URI update =
      new URIBuilder().setPath(BrandAuthorisationPath.BASE_PATH + BrandAuthorisationPath.UPDATE)
        .addParameter("storeId", BrandAuthorisationControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandAuthorisationControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandAuthorisationControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandAuthorisationControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandAuthorisationControllerTest.DEFAULT_USERNAME)
        .addParameter("sellerCode", DEFAULT_SELLER_CODE).build();
    String updateRequest = objectMapper.writeValueAsString(brandAuthUpdateRequest);
    try {
      this.mockMvc.perform(MockMvcRequestBuilders.post(update).content(updateRequest)
          .contentType(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", equalTo(Boolean.FALSE)));
    } finally {
      verify(brandAuthServiceWrapper).editBrandAuthDetailsAndEvictCache(DEFAULT_STORE_ID,
        brandAuthUpdateRequest, DEFAULT_USERNAME);
    }
  }

}