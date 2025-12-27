package com.gdn.partners.pcu.internal.web.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.ArrayList;
import java.util.List;

import com.gdn.partners.pcu.internal.web.model.response.BusinessPartnerProfileWebResponse;
import org.apache.commons.lang3.StringUtils;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.BusinessPartnerApiPath;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.service.BusinessPartnerService;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;
import com.gdn.partners.pcu.internal.web.model.request.ProductSuspensionFilterRequest;
import com.gdn.partners.pcu.internal.web.model.request.ReviewProductsFilterRequest;
import com.gdn.partners.pcu.internal.web.model.response.BusinessPartnerWebResponse;

@AutoConfigureMockMvc
@ExtendWith(MockitoExtension.class)
public class BusinessPartnerControllerTest extends TestHelper {

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @InjectMocks
  private BusinessPartnerController businessPartnerController;

  @Mock
  private BusinessPartnerService businessPartnerService;

  @Captor
  private ArgumentCaptor<ReviewProductsFilterRequest> requestArgumentCaptor;

  private static final long TOTAL_NUM_FOUND = 100;
  private static final String KEYWORD = "keyword";
  private static final String ALL = "ALL";
  private static final int PAGE = 0;
  private static final int SIZE = 10;
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String STATUS = "status";
  private static final String SEARCH_KEYWORD = "status";
  private Page<BusinessPartnerWebResponse> partnerMapperResponsePage;
  private List<BusinessPartnerWebResponse> partnerMapperResponseList;
  private ProductSuspensionFilterRequest productSuspensionFilterRequest;

  @BeforeEach
  public void setUp() throws Exception {
    mockMvc = MockMvcBuilders.standaloneSetup(businessPartnerController).build();
    partnerMapperResponseList = new ArrayList<>();
    BusinessPartnerWebResponse businessPartnerWebResponse = new BusinessPartnerWebResponse();
    businessPartnerWebResponse.setBusinessPartnerName(Constants.USER_NAME);
    businessPartnerWebResponse.setBusinessPartnerCode(Constants.BUSINESS_PARTNER_CODE);
    partnerMapperResponseList.add(businessPartnerWebResponse);
    partnerMapperResponsePage = new PageImpl<>(partnerMapperResponseList, PageRequest.of(PAGE, SIZE), TOTAL_NUM_FOUND);

    productSuspensionFilterRequest = new ProductSuspensionFilterRequest();
    productSuspensionFilterRequest.setCategoryCode(BUSINESS_PARTNER_CODE);
    productSuspensionFilterRequest.setCategoryCode(CATEGORY_CODE);
    productSuspensionFilterRequest.setCategoryCode(STATUS);
    productSuspensionFilterRequest.setCategoryCode(SEARCH_KEYWORD);
  }

  @AfterEach
  public void tearDown() throws Exception {
    Mockito.verifyNoMoreInteractions(businessPartnerService, clientParameterHelper);
  }

  @Test
  public void getBusinessPartnersByTimeAndStatusFilterTest() throws Exception {
    ReviewProductsFilterRequest reviewProductsFilterRequest =
        ReviewProductsFilterRequest.builder().statusFilter(ALL).timeFilter(ALL)
            .searchKeyword(StringUtils.EMPTY).build();
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(this.businessPartnerService
        .getBusinessPartnersByTimeAndStatusFilter(reviewProductsFilterRequest, Boolean.FALSE, Boolean.FALSE, PAGE,
            SIZE)).thenReturn(partnerMapperResponsePage);
    MockHttpServletRequestBuilder requestBuilder =
        post(BusinessPartnerApiPath.BASE_PATH + BusinessPartnerApiPath.BUSINESS_PARTNER_FILTER).param("activated", String.valueOf(Boolean.FALSE))
            .param("viewable", String.valueOf(Boolean.FALSE))
            .param("page", String.valueOf(PAGE)).param("size", String.valueOf(SIZE))
            .content(toJson(reviewProductsFilterRequest))
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    Mockito.verify(this.businessPartnerService)
        .getBusinessPartnersByTimeAndStatusFilter(requestArgumentCaptor.capture(), eq(Boolean.FALSE),
            eq(Boolean.FALSE), eq(PAGE), eq(SIZE));
  }

  @Test
  public void getAllActiveMerchantListTest() throws Exception {
    when(businessPartnerService.getAllActiveMerchantList(productSuspensionFilterRequest, PAGE, SIZE))
        .thenReturn(partnerMapperResponsePage);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(BusinessPartnerApiPath.BASE_PATH + BusinessPartnerApiPath.BUSINESS_PARTNER_LIST)
            .content(toJson(productSuspensionFilterRequest)).param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(clientParameterHelper).getRequestId();
    Mockito.verify(this.businessPartnerService).getAllActiveMerchantList(productSuspensionFilterRequest, PAGE, SIZE);
  }

  @Test
  public void getBusinessPartnerFlagsTest() throws Exception {
    BusinessPartnerProfileWebResponse mockResponse = new BusinessPartnerProfileWebResponse();
    when(businessPartnerService.fetchBusinessPartnerFlags(BUSINESS_PARTNER_CODE)).thenReturn(
      mockResponse);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);

    MockHttpServletRequestBuilder requestBuilder =
      get(BusinessPartnerApiPath.BASE_PATH + BusinessPartnerApiPath.BUSINESS_PARTNER_FLAGS).param(
          "businessPartnerCode", BUSINESS_PARTNER_CODE).contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON);

    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));

    verify(clientParameterHelper).getRequestId();
    verify(businessPartnerService).fetchBusinessPartnerFlags(BUSINESS_PARTNER_CODE);
  }
}