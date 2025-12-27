package com.gdn.x.productcategorybase.controller.brand;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.x.productcategorybase.BrandAuthorisationWipPath;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthorisationWipListResponse;
import com.gdn.x.productcategorybase.dto.request.BrandAuthorisationWipActionRequest;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthWipDetailResponse;
import com.gdn.x.productcategorybase.dto.request.BrandAuthUpdateRequest;
import com.gdn.x.productcategorybase.dto.request.BrandAuthorisationWipListRequest;
import com.gdn.x.productcategorybase.service.brand.BrandAuthorisationWipService;
import com.gdn.x.productcategorybase.service.brand.BrandAuthorisationWipServiceWrapper;
import org.apache.http.client.utils.URIBuilder;
import org.hamcrest.Matchers;
import static org.hamcrest.CoreMatchers.equalTo;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;
import java.net.URI;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import com.fasterxml.jackson.core.JsonFactory;
import com.gdn.x.productcategorybase.dto.brand.BrandAuthCreateWipRequest;
import com.gdn.x.productcategorybase.service.brand.BrandAuthServiceWrapper;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

@ExtendWith(MockitoExtension.class)
class BrandAuthorisationWipControllerTest {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "API";
  private static final String DEFAULT_CLIENT_ID = "TEST";
  private static final String DEFAULT_REQUEST_ID = UUID.randomUUID().toString();
  private static final String DEFAULT_USERNAME = "DEVELOPER";
  private static final String DEFAULT_BRAND_CODE = "BRD-00001";
  private static final String DEFAULT_IPR_REGISTRATION_NUMBER = "IPR-3334-XVU";
  private static final String DEFAULT_SELLER_CODE = "BRD-00001";
  private static final String DEFAULT_BRAND_NAME = "TEST-BRAND";
  private static final String STATUS = "IN_REVIEW";
  private static final String ID = "ID";
  @InjectMocks
  private BrandAuthorisationWipController brandAuthorisationWipController;


  @Mock
  private BrandAuthServiceWrapper brandAuthServiceWrapper;
  @Mock
  private BrandAuthorisationWipService brandAuthorisationWipService;

  @Mock
  private BrandAuthorisationWipServiceWrapper brandAuthorisationWipServiceWrapper;
  private MockMvc mockMvc;
  private ObjectMapper objectMapper;
  private Pageable pageable;
  private Page<BrandAuthorisationWipListResponse> brandAuthorisationWipListResponses;
  private BrandAuthorisationWipActionRequest brandAuthorisationWipActionRequest;

  private static final String STORE_ID = "store-id";
  private static final String CHANNEL_ID = "channel-id";
  private static final String CLIENT_ID = "client-id";
  private static final String USER_NAME = "user-name";
  private static final String REQUEST_ID = "request-id";
  private static final int PAGE = 0;
  private static final int SIZE = 10;


  @BeforeEach
  void setUp() throws Exception {
    this.mockMvc = standaloneSetup(this.brandAuthorisationWipController).build();
    brandAuthorisationWipActionRequest = new BrandAuthorisationWipActionRequest();
    this.objectMapper = new ObjectMapper(new JsonFactory());
    pageable = PageRequest.of(PAGE, SIZE);
    BrandAuthorisationWipListResponse brandAuthorisationWipListResponse = new BrandAuthorisationWipListResponse();
    brandAuthorisationWipListResponses = new PageImpl<>(List.of(brandAuthorisationWipListResponse), pageable, 1L);
  }


  @AfterEach
  void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(brandAuthServiceWrapper);
    Mockito.verifyNoMoreInteractions(brandAuthorisationWipService);
  }

  @Test
  void fetchBrandAuthWipDetailsTest() throws Exception {
    BrandAuthWipDetailResponse mockResponse = new BrandAuthWipDetailResponse();
    Mockito.when(this.brandAuthorisationWipService.fetchBrandAuthWipDetails(DEFAULT_STORE_ID, STATUS, ID)).thenReturn(mockResponse);

    this.mockMvc.perform(
            get(BrandAuthorisationWipPath.BASE_PATH + BrandAuthorisationWipPath.DETAILS,
                DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE).param("storeId", DEFAULT_STORE_ID)
                .param("requestId", DEFAULT_REQUEST_ID).param("status", STATUS)
                .param("id", ID)
                .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));

    Mockito.verify(this.brandAuthorisationWipService)
        .fetchBrandAuthWipDetails(DEFAULT_STORE_ID, STATUS, ID);
  }

  @Test
  void submitBrandAuthorisationRequestTest() throws Exception {
    BrandAuthUpdateRequest brandAuthUpdateRequest = new BrandAuthUpdateRequest();
    String requestBody = this.objectMapper.writeValueAsString(brandAuthUpdateRequest);
    URI uri = new URIBuilder().setPath(
            BrandAuthorisationWipPath.BASE_PATH + BrandAuthorisationWipPath.SUBMIT_AUTHORISATION_REQUEST)
        .addParameter("storeId", DEFAULT_STORE_ID).addParameter("channelId", DEFAULT_REQUEST_ID)
        .addParameter("clientId", DEFAULT_REQUEST_ID).addParameter("requestId", DEFAULT_REQUEST_ID)
        .addParameter("username", DEFAULT_REQUEST_ID).build();
    this.mockMvc.perform(MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandAuthorisationWipServiceWrapper)
      .submitBrandAuthorisationRequest(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID,
        brandAuthUpdateRequest);
  }

  @Test
  void brandAuthorisationActionTest() throws Exception {
    this.mockMvc.perform(MockMvcRequestBuilders.post(
                BrandAuthorisationWipPath.BASE_PATH + BrandAuthorisationWipPath.APPROVE)
            .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId",
                REQUEST_ID)
            .param("username", USER_NAME)
            .content(objectMapper.writeValueAsString(brandAuthorisationWipActionRequest))
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.brandAuthorisationWipServiceWrapper, Mockito.times(1))
        .brandAuthorisationWipAction(STORE_ID, USER_NAME, brandAuthorisationWipActionRequest);
  }

  @Test
  void createRequestTest() throws Exception {
    URI uri =
      new URIBuilder().setPath(BrandAuthorisationWipPath.BASE_PATH + BrandAuthorisationWipPath.CREATE_BRAND_AUTH)
        .addParameter("storeId", BrandAuthorisationWipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandAuthorisationWipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandAuthorisationWipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandAuthorisationWipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandAuthorisationWipControllerTest.DEFAULT_USERNAME).build();
    String requestBody =
      this.objectMapper.writeValueAsString(this.generateCreateBrandAuthRequest());
    this.mockMvc.perform(
      MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandAuthServiceWrapper)
      .createBrandAuthWip(Mockito.any(BrandAuthCreateWipRequest.class), Mockito.anyString(),
        Mockito.anyString());
  }

  @Test
  void createRequestWithBrandNameExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(this.brandAuthServiceWrapper)
      .createBrandAuthWip(Mockito.any(BrandAuthCreateWipRequest.class), Mockito.anyString(),
        Mockito.anyString());
    URI uri =
      new URIBuilder().setPath(BrandAuthorisationWipPath.BASE_PATH + BrandAuthorisationWipPath.CREATE_BRAND_AUTH)
        .addParameter("storeId", BrandAuthorisationWipControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", BrandAuthorisationWipControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("clientId", BrandAuthorisationWipControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("requestId", BrandAuthorisationWipControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", BrandAuthorisationWipControllerTest.DEFAULT_USERNAME).build();
      BrandAuthCreateWipRequest request = this.generateCreateBrandAuthRequest();
    request.setBrandName(null);
    String requestBody = this.objectMapper.writeValueAsString(request);
    this.mockMvc.perform(
      MockMvcRequestBuilders.post(uri).content(requestBody).contentType(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.brandAuthServiceWrapper)
      .createBrandAuthWip(Mockito.any(BrandAuthCreateWipRequest.class), Mockito.anyString(), Mockito.anyString());
  }

  private BrandAuthCreateWipRequest generateCreateBrandAuthRequest() throws Exception {
    Calendar calendar = Calendar.getInstance();
    calendar.add(Calendar.DATE, 100);
    Date expireDate = calendar.getTime();
    BrandAuthCreateWipRequest request = new BrandAuthCreateWipRequest();
    request.setBrandName(BrandAuthorisationWipControllerTest.DEFAULT_BRAND_NAME);
    request.setBrandCode(BrandAuthorisationWipControllerTest.DEFAULT_BRAND_CODE);
    request.setIprRegistrationNumber(BrandAuthorisationWipControllerTest.DEFAULT_IPR_REGISTRATION_NUMBER);
    request.setSellerCode(BrandAuthorisationWipControllerTest.DEFAULT_SELLER_CODE);
    request.setAuthExpireDate(expireDate);
    request.setAuthStartDate(new Date());
    return request;
  }

  @Test
  void validateBrandAuthRequestTest() throws Exception {
    Mockito.when(this.brandAuthorisationWipService.validateBrandAuthRequest(DEFAULT_STORE_ID,
      DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE, true)).thenReturn(true);
    this.mockMvc.perform(
        get(BrandAuthorisationWipPath.BASE_PATH + BrandAuthorisationWipPath.VALIDATE_BRAND_REQUEST,
          DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE).param("storeId", DEFAULT_STORE_ID)
          .param("requestId", DEFAULT_REQUEST_ID).param("edited", "true")
          .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", Matchers.equalTo(true)));

    Mockito.verify(this.brandAuthorisationWipService)
      .validateBrandAuthRequest(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE, true);
  }

  @Test
  void filterSummaryTest() throws Exception {
    BrandAuthorisationWipListRequest request = new BrandAuthorisationWipListRequest();
    request.setSellerCode(DEFAULT_SELLER_CODE);
    request.setKeyword(DEFAULT_BRAND_NAME);
    Mockito.when(
        this.brandAuthorisationWipService.getBrandAuthorisationWipListResponse(STORE_ID, request,
            pageable)).thenReturn(brandAuthorisationWipListResponses);
    this.mockMvc.perform(MockMvcRequestBuilders.post(
                BrandAuthorisationWipPath.BASE_PATH + BrandAuthorisationWipPath.FILTER_SUMMARY)
            .accept(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId",
                REQUEST_ID)
            .param("username", USER_NAME).param("size", "10").param("page", "0")
            .content(objectMapper.writeValueAsString(request)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.brandAuthorisationWipService)
        .getBrandAuthorisationWipListResponse(STORE_ID, request, pageable);
  }

  @Test
  void checkEligibilityTest() throws Exception {
    Mockito.when(this.brandAuthorisationWipServiceWrapper.checkEligibility(STORE_ID, DEFAULT_SELLER_CODE))
        .thenReturn(true);
    this.mockMvc.perform(MockMvcRequestBuilders.get(
                BrandAuthorisationWipPath.BASE_PATH + BrandAuthorisationWipPath.CREATION_ELIGIBILITY)
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USER_NAME)
            .param("sellerCode", DEFAULT_SELLER_CODE).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.brandAuthorisationWipServiceWrapper)
        .checkEligibility(STORE_ID, DEFAULT_SELLER_CODE);
  }
}
