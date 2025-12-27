package com.gdn.partners.pcu.internal.web.controller;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthCreateWipRequest;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthUpdateRequest;
import com.gdn.partners.pcu.internal.client.model.request.BrandAuthorisationWipListRequest;
import com.gdn.partners.pcu.internal.client.model.response.BrandAuthorisationWipListResponse;
import com.gdn.partners.pcu.internal.model.BrandAuthorisationWipPath;
import com.gdn.partners.pcu.internal.service.BrandAuthorisationWipService;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;
import com.gdn.partners.pcu.internal.web.model.request.BrandAuthorisationWipActionWebRequest;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import java.util.ArrayList;
import java.util.List;

import static org.hamcrest.Matchers.equalTo;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

@ExtendWith(MockitoExtension.class)
class BrandAuthorisationWipControllerTest extends TestHelper {

  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_SELLER_CODE = "seller-code";
  private static final String DEFAULT_BRAND_CODE = "brand-code";
  private static final String DEFAULT_REQUEST_ID = "request-id";
  private static final String DEFAULT_USERNAME = "username";
  private static final String STATUS = "ACTIVE";
  private static final String ID = "id";
  private static final int PAGE = 0;
  private static final int SIZE = 25;
  @InjectMocks
  private BrandAuthorisationWipController brandAuthorisationWipController;

  @Mock
  private MockMvc mockMvc;

  @Mock
  private BrandAuthorisationWipService brandAuthorisationWipService;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  private ObjectMapper objectMapper;
  private BrandAuthorisationWipActionWebRequest brandAuthorisationWipActionWebRequest;
  private BrandAuthCreateWipRequest brandAuthCreateWipRequest;
  private BrandAuthUpdateRequest brandAuthUpdateRequest;
  private BrandAuthorisationWipListRequest brandAuthorisationWipListRequest;
  private BrandAuthorisationWipListResponse brandAuthorisationWipListResponse;
  private final List<BrandAuthorisationWipListResponse> brandAuthorisationWipListResponses = new ArrayList<>();
  private Page<BrandAuthorisationWipListResponse> brandAuthorisationWipListResponsePage;


  @BeforeEach
  public void setUp() throws Exception {
    this.mockMvc = standaloneSetup(this.brandAuthorisationWipController).build();
    brandAuthorisationWipActionWebRequest = new BrandAuthorisationWipActionWebRequest();
    brandAuthCreateWipRequest = new BrandAuthCreateWipRequest();
    objectMapper = new ObjectMapper();
    brandAuthUpdateRequest = new BrandAuthUpdateRequest();
    brandAuthorisationWipListRequest = new BrandAuthorisationWipListRequest();
    brandAuthorisationWipListResponses.add(brandAuthorisationWipListResponse);
    brandAuthorisationWipListResponsePage =
        new PageImpl<>(brandAuthorisationWipListResponses, PageRequest.of(PAGE, SIZE), SIZE);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(brandAuthorisationWipService);
    Mockito.verifyNoMoreInteractions(clientParameterHelper);
  }

  @Test
  void fetchBrandAuthWipDetailsTest() throws Exception {
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(DEFAULT_STORE_ID);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(DEFAULT_REQUEST_ID);
    Mockito.when(clientParameterHelper.getBusinessPartnerCode()).thenReturn(DEFAULT_SELLER_CODE);
    this.mockMvc.perform(
        get(BrandAuthorisationWipPath.BASE_PATH + BrandAuthorisationWipPath.DETAILS)
            .param("status", STATUS)
            .param("id", ID)
            .contentType(MediaType.APPLICATION_JSON))
      .andExpect(status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(brandAuthorisationWipService)
      .fetchBrandAuthWipDetails(DEFAULT_STORE_ID, STATUS, ID, DEFAULT_SELLER_CODE);
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getStoreId();
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
  }

  @Test
  void brandAuthorisationActionTest() throws Exception {
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(DEFAULT_STORE_ID);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(DEFAULT_REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(DEFAULT_USERNAME);
    this.mockMvc.perform(MockMvcRequestBuilders.post(
                BrandAuthorisationWipPath.BASE_PATH + BrandAuthorisationWipPath.ACTION)
            .accept(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(brandAuthorisationWipActionWebRequest))
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.brandAuthorisationWipService, Mockito.times(1))
        .brandAuthorisationWipAction(DEFAULT_STORE_ID, DEFAULT_USERNAME,
            brandAuthorisationWipActionWebRequest);
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getStoreId();
    Mockito.verify(clientParameterHelper).getUsername();
  }

  @Test
  void validateBrandAuthRequestTest() throws Exception {
    this.mockMvc.perform(
        get(BrandAuthorisationWipPath.BASE_PATH + BrandAuthorisationWipPath.VALIDATE_BRAND_REQUEST,
          DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE).param("storeId", DEFAULT_STORE_ID)
          .param("edited", "true").param("requestId", DEFAULT_REQUEST_ID)
          .contentType(MediaType.APPLICATION_JSON)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(brandAuthorisationWipService)
        .validateBrandAuthRequest(DEFAULT_STORE_ID, DEFAULT_BRAND_CODE, DEFAULT_SELLER_CODE, true);
  }

  @Test
  void createBrandAuthRequestTest() throws Exception {
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(DEFAULT_STORE_ID);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(DEFAULT_REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(DEFAULT_USERNAME);
    this.mockMvc.perform(MockMvcRequestBuilders.post(
                BrandAuthorisationWipPath.BASE_PATH + BrandAuthorisationWipPath.CREATE_BRAND_AUTH)
            .accept(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(brandAuthCreateWipRequest))
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.brandAuthorisationWipService, Mockito.times(1))
        .createBrandAuthRequest(DEFAULT_STORE_ID, DEFAULT_USERNAME, DEFAULT_REQUEST_ID,
        brandAuthCreateWipRequest);
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getStoreId();
    Mockito.verify(clientParameterHelper).getUsername();
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
  }

  @Test
  void updateBrandAuthWipRequestTest() throws Exception {
    Mockito.when(clientParameterHelper.getStoreId()).thenReturn(DEFAULT_STORE_ID);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(DEFAULT_REQUEST_ID);
    Mockito.when(clientParameterHelper.getUsername()).thenReturn(DEFAULT_USERNAME);
    Mockito.when(clientParameterHelper.getBusinessPartnerCode()).thenReturn(DEFAULT_SELLER_CODE);
    brandAuthUpdateRequest.setSellerCode(DEFAULT_SELLER_CODE);
    this.mockMvc.perform(MockMvcRequestBuilders.post(
                BrandAuthorisationWipPath.BASE_PATH + BrandAuthorisationWipPath.UPDATE_BRAND_AUTH)
            .accept(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(brandAuthUpdateRequest))
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    Mockito.verify(this.brandAuthorisationWipService, Mockito.times(1))
        .updateBrandAuthWip(DEFAULT_STORE_ID, DEFAULT_USERNAME, brandAuthUpdateRequest);
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getStoreId();
    Mockito.verify(clientParameterHelper).getUsername();
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
  }

  @Test
  void filterSummaryTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(DEFAULT_REQUEST_ID);
    Mockito.when(brandAuthorisationWipService.getBrandAuthorisationWipList(PAGE, SIZE,
        brandAuthorisationWipListRequest)).thenReturn(brandAuthorisationWipListResponsePage);
    MockHttpServletRequestBuilder requestBuilder =
        post(BrandAuthorisationWipPath.BASE_PATH + BrandAuthorisationWipPath.FILTER_SUMMARY).content(
                toJson(brandAuthorisationWipListRequest)).param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.brandAuthorisationWipService, Mockito.times(1))
        .getBrandAuthorisationWipList(PAGE, SIZE, brandAuthorisationWipListRequest);
    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper, Mockito.times(2)).getBusinessPartnerCode();
  }

  @Test
  void checkEligibilityTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(DEFAULT_REQUEST_ID);
    Mockito.when(clientParameterHelper.getBusinessPartnerCode()).thenReturn(DEFAULT_SELLER_CODE);
    Mockito.when(brandAuthorisationWipService.checkEligibility(DEFAULT_SELLER_CODE))
        .thenReturn(true);

    MockHttpServletRequestBuilder requestBuilder =
        get(BrandAuthorisationWipPath.BASE_PATH + BrandAuthorisationWipPath.CREATION_ELIGIBILITY)
            .contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));

    Mockito.verify(clientParameterHelper).getRequestId();
    Mockito.verify(clientParameterHelper).getBusinessPartnerCode();
    Mockito.verify(brandAuthorisationWipService).checkEligibility(DEFAULT_SELLER_CODE);
  }
}