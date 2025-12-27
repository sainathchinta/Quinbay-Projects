package com.gdn.mta.product.controller;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.gda.mta.product.dto.HistoryRequest;
import com.gda.mta.product.dto.HistoryUpdateRequest;
import com.gda.mta.product.dto.SummaryFilterRequest;
import com.gda.mta.product.dto.response.AssigneeResponse;
import com.gda.mta.product.dto.response.FilterCountResponse;
import com.gda.mta.product.dto.response.HistoryResponse;
import com.gda.mta.product.dto.response.ReviewProductResponse;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.GdnRestSingleResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.mta.product.enums.StatusFilterType;
import com.gdn.mta.product.enums.TimeFilterType;
import com.gdn.mta.product.service.SummaryFilterService;
import com.gdn.mta.product.valueobject.SummaryFilterServiceRequest;
import com.gdn.mta.product.web.model.SummaryFilterApiPath;
import com.gdn.partners.pbp.commons.constants.Constants;
import com.gdn.partners.pbp.commons.util.SolrConstants;
import com.gdn.partners.pbp.commons.util.SolrFieldNames;
import com.gdn.x.mta.distributiontask.response.ProductBusinessPartnerMapperResponse;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.utils.URIBuilder;
import org.apache.solr.client.solrj.SolrServerException;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.mockito.Captor;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.http.MediaType;
import org.springframework.http.converter.ByteArrayHttpMessageConverter;
import org.springframework.http.converter.FormHttpMessageConverter;
import org.springframework.http.converter.ResourceHttpMessageConverter;
import org.springframework.http.converter.StringHttpMessageConverter;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.mockito.ArgumentMatchers.eq;


public class SummaryFilterControllerTest {

  private static final String DEFAULT_GDN_SKU = "Bibli-01-01-3";
  private static final String DEFAULT_PRODUCT_SKU = "Bibli-01-01";

  @InjectMocks
  private SummaryFilterController summaryFilterController;

  @Mock
  private SummaryFilterService summaryFilterService;

  @Captor
  private ArgumentCaptor<SummaryFilterServiceRequest> serviceRequest;

  private MockMvc mockMvc;
  private ObjectMapper objectMapper;
  private FilterCountResponse filterCountResponse;
  private Page<ReviewProductResponse> productResponses;
  private List<ReviewProductResponse> productResponseList;
  private GdnBaseRestResponse response;
  private GdnRestListResponse<ReviewProductResponse> restListResponse;
  private static final String DEFAULT_STORE_ID = "10001";
  private static final String DEFAULT_CHANNEL_ID = "channel-1";
  private static final String DEFAULT_CLIENT_ID = "client-1";
  private static final String DEFAULT_USERNAME = "com.gdn.mta.developer";
  private static final String DEFAULT_REQUEST_ID = "requestId";
  private static final long TOTAL_RECORDS = 100;
  private static final int PAGE = 0;
  private static final int SIZE = 100;
  private static final String BUSINESS_PARTNER_CODE = "code";
  private static final String BUSINESS_PARTNER_NAME = "name";
  private static final String ASSIGNEE = "assignee";
  private SummaryFilterRequest request;
  private PageMetaData pageMetaData;
  private Pageable pageable;
  private Page<ProductBusinessPartnerMapperResponse> businessPartnerResponsePage;
  private List<AssigneeResponse> assigneeResponses;
  private List<ProductBusinessPartnerMapperResponse> businessPartnerResponseList;
  private GdnRestListResponse<ProductBusinessPartnerMapperResponse> mapperResponseGdnRestListResponse;
  private GdnRestListResponse<AssigneeResponse> assigneeResponseGdnRestListResponse;
  private SummaryFilterServiceRequest summaryFilterServiceRequest;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    this.setMockMvc(MockMvcBuilders.standaloneSetup(this.summaryFilterController)
        .setMessageConverters(new ByteArrayHttpMessageConverter(), new StringHttpMessageConverter(),
            new ResourceHttpMessageConverter(), new FormHttpMessageConverter(),
            new MappingJackson2HttpMessageConverter()).build());
    this.setObjectMapper(new ObjectMapper(new JsonFactory()));
    pageMetaData = new PageMetaData(PAGE, SIZE, TOTAL_RECORDS);
    pageable = PageRequest.of(PAGE, SIZE);
    filterCountResponse = new FilterCountResponse();
    filterCountResponse.setToday(10);
    response = new GdnRestSingleResponse<>(filterCountResponse, DEFAULT_REQUEST_ID);
    request = new SummaryFilterRequest();
    request.setAssignedTo(SolrConstants.ASSIGNED_TO_PREFIX);
    request.setSortColumn(SolrFieldNames.SUBMITTED_DATE);
    request.setSortOrder(SolrConstants.ASC);
    request.setStatusFilter(StatusFilterType.ALL.getStatusFilterType());
    request.setTimeFilter(TimeFilterType.ALL.getTimeFilterType());
    productResponseList = new ArrayList<>();
    ReviewProductResponse reviewProductResponse = new ReviewProductResponse();
    reviewProductResponse.setProductName(Constants.PRODUCT_NAME);
    reviewProductResponse.setCategoryCode(Constants.CATEGORY_CODE);
    productResponseList.add(reviewProductResponse);
    productResponses = new PageImpl<>(productResponseList, pageable, TOTAL_RECORDS);
    restListResponse =
        new GdnRestListResponse<>(productResponses.getContent(), pageMetaData,
            DEFAULT_REQUEST_ID);
    businessPartnerResponseList = new ArrayList<>();
    ProductBusinessPartnerMapperResponse businessPartnerMapperResponse = new ProductBusinessPartnerMapperResponse();
    businessPartnerMapperResponse.setBusinessPartnerCode(BUSINESS_PARTNER_CODE);
    businessPartnerMapperResponse.setBusinessPartnerName(BUSINESS_PARTNER_NAME);
    businessPartnerResponseList.add(businessPartnerMapperResponse);
    businessPartnerResponsePage = new PageImpl<>(businessPartnerResponseList, pageable, TOTAL_RECORDS);
    assigneeResponses = new ArrayList<>();
    AssigneeResponse assigneeResponse = new AssigneeResponse();
    assigneeResponse.setAssignee(ASSIGNEE);
    assigneeResponses.add(assigneeResponse);
    summaryFilterServiceRequest =
        SummaryFilterServiceRequest.builder().statusFilter(StatusFilterType.ALL).timeFilter(TimeFilterType.ALL)
            .searchKeyword(StringUtils.EMPTY).build();
  }

  @Test
  public void getFilterCountsTest() throws Exception {
    Mockito.when(this.summaryFilterService
        .getFilterCountsByStoreIdAndActivatedAndViewable(Constants.DEFAULT_STORE_ID, Boolean.FALSE, Boolean.FALSE))
        .thenReturn(filterCountResponse);
    URI uri = new URIBuilder().setPath(SummaryFilterApiPath.BASE_PATH + SummaryFilterApiPath.COUNTS_FILTER)
        .addParameter("storeId", SummaryFilterControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", SummaryFilterControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", SummaryFilterControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", SummaryFilterControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", SummaryFilterControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("activated", String.valueOf(Boolean.FALSE))
        .addParameter("viewable", String.valueOf(Boolean.FALSE)).build();
    MvcResult result = getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(this.summaryFilterService)
        .getFilterCountsByStoreIdAndActivatedAndViewable(Constants.DEFAULT_STORE_ID, Boolean.FALSE, Boolean.FALSE);
    assertEquals(getObjectMapper().writeValueAsString(response), result.getResponse().getContentAsString());
  }

  @Test
  public void getFilterCountsSolrServerExceptionTest() throws Exception {
    Mockito.when(this.summaryFilterService
        .getFilterCountsByStoreIdAndActivatedAndViewable(Constants.DEFAULT_STORE_ID, Boolean.FALSE, Boolean.FALSE))
        .thenThrow(SolrServerException.class);
    URI uri = new URIBuilder().setPath(SummaryFilterApiPath.BASE_PATH + SummaryFilterApiPath.COUNTS_FILTER)
        .addParameter("storeId", SummaryFilterControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", SummaryFilterControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", SummaryFilterControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", SummaryFilterControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", SummaryFilterControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("activated", String.valueOf(Boolean.FALSE))
        .addParameter("viewable", String.valueOf(Boolean.FALSE)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(this.summaryFilterService)
        .getFilterCountsByStoreIdAndActivatedAndViewable(Constants.DEFAULT_STORE_ID, Boolean.FALSE, Boolean.FALSE);
  }

  @Test
  public void getFilterCountsIOExceptionTest() throws Exception {
    Mockito.when(this.summaryFilterService
        .getFilterCountsByStoreIdAndActivatedAndViewable(Constants.DEFAULT_STORE_ID, Boolean.FALSE, Boolean.FALSE))
        .thenThrow(IOException.class);
    URI uri = new URIBuilder().setPath(SummaryFilterApiPath.BASE_PATH + SummaryFilterApiPath.COUNTS_FILTER)
        .addParameter("storeId", SummaryFilterControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", SummaryFilterControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", SummaryFilterControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", SummaryFilterControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", SummaryFilterControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("activated", String.valueOf(Boolean.FALSE))
        .addParameter("viewable", String.valueOf(Boolean.FALSE)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(this.summaryFilterService)
        .getFilterCountsByStoreIdAndActivatedAndViewable(Constants.DEFAULT_STORE_ID, Boolean.FALSE, Boolean.FALSE);
  }

  @Test
  public void getFilterCountsExceptionTest() throws Exception {
    Mockito.when(this.summaryFilterService
        .getFilterCountsByStoreIdAndActivatedAndViewable(Constants.DEFAULT_STORE_ID, Boolean.FALSE, Boolean.FALSE))
        .thenThrow(RuntimeException.class);
    URI uri = new URIBuilder().setPath(SummaryFilterApiPath.BASE_PATH + SummaryFilterApiPath.COUNTS_FILTER)
        .addParameter("storeId", SummaryFilterControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", SummaryFilterControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", SummaryFilterControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", SummaryFilterControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", SummaryFilterControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("activated", String.valueOf(Boolean.FALSE))
        .addParameter("viewable", String.valueOf(Boolean.FALSE)).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.get(uri).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(this.summaryFilterService)
        .getFilterCountsByStoreIdAndActivatedAndViewable(Constants.DEFAULT_STORE_ID, Boolean.FALSE, Boolean.FALSE);
  }

  @Test
  public void getReviewProductsTest() throws Exception {
    Mockito.doReturn(productResponses).when(this.summaryFilterService)
        .getReviewProductsByFilterRequestAndActivatedAndViewableFlag(Constants.DEFAULT_STORE_ID, request, Boolean.FALSE,
            Boolean.FALSE, PAGE, SIZE);
    URI uri = new URIBuilder().setPath(SummaryFilterApiPath.BASE_PATH + SummaryFilterApiPath.REVIEW_PRODUCTS_FILTER)
        .addParameter("storeId", SummaryFilterControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", SummaryFilterControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", SummaryFilterControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", SummaryFilterControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", SummaryFilterControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("activated", String.valueOf(Boolean.FALSE))
        .addParameter("viewable", String.valueOf(Boolean.FALSE)).addParameter("page", String.valueOf(PAGE))
        .addParameter("size", String.valueOf(SIZE)).build();
    MvcResult result = getMockMvc().perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
        .content(getObjectMapper().writeValueAsString(request)).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(this.summaryFilterService)
        .getReviewProductsByFilterRequestAndActivatedAndViewableFlag(Constants.DEFAULT_STORE_ID, request, Boolean.FALSE,
            Boolean.FALSE, PAGE, SIZE);
    assertEquals(getObjectMapper().writeValueAsString(restListResponse), result.getResponse().getContentAsString());
  }

  @Test
  public void getReviewProductsSolrServerExceptionTest() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(this.summaryFilterService)
        .getReviewProductsByFilterRequestAndActivatedAndViewableFlag(Constants.DEFAULT_STORE_ID, request, Boolean.FALSE,
            Boolean.FALSE, PAGE, SIZE);
    URI uri = new URIBuilder().setPath(SummaryFilterApiPath.BASE_PATH + SummaryFilterApiPath.REVIEW_PRODUCTS_FILTER)
        .addParameter("storeId", SummaryFilterControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", SummaryFilterControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", SummaryFilterControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", SummaryFilterControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", SummaryFilterControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("activated", String.valueOf(Boolean.FALSE))
        .addParameter("viewable", String.valueOf(Boolean.FALSE)).addParameter("page", String.valueOf(PAGE))
        .addParameter("size", String.valueOf(SIZE)).build();
    MvcResult result = getMockMvc().perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
        .content(getObjectMapper().writeValueAsString(request)).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(this.summaryFilterService)
        .getReviewProductsByFilterRequestAndActivatedAndViewableFlag(Constants.DEFAULT_STORE_ID, request, Boolean.FALSE,
            Boolean.FALSE, PAGE, SIZE);
  }

  @Test
  public void getReviewProductsIOExceptionTest() throws Exception {
    Mockito.doThrow(IOException.class).when(this.summaryFilterService)
        .getReviewProductsByFilterRequestAndActivatedAndViewableFlag(Constants.DEFAULT_STORE_ID, request, Boolean.FALSE,
            Boolean.FALSE, PAGE, SIZE);
    URI uri = new URIBuilder().setPath(SummaryFilterApiPath.BASE_PATH + SummaryFilterApiPath.REVIEW_PRODUCTS_FILTER)
        .addParameter("storeId", SummaryFilterControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", SummaryFilterControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", SummaryFilterControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", SummaryFilterControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", SummaryFilterControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("activated", String.valueOf(Boolean.FALSE))
        .addParameter("viewable", String.valueOf(Boolean.FALSE)).addParameter("page", String.valueOf(PAGE))
        .addParameter("size", String.valueOf(SIZE)).build();
    MvcResult result = getMockMvc().perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
        .content(getObjectMapper().writeValueAsString(request)).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(this.summaryFilterService)
        .getReviewProductsByFilterRequestAndActivatedAndViewableFlag(Constants.DEFAULT_STORE_ID, request, Boolean.FALSE,
            Boolean.FALSE, PAGE, SIZE);
  }

  @Test
  public void getReviewProductsExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(this.summaryFilterService)
        .getReviewProductsByFilterRequestAndActivatedAndViewableFlag(Constants.DEFAULT_STORE_ID, request, Boolean.FALSE,
            Boolean.FALSE, PAGE, SIZE);
    URI uri = new URIBuilder().setPath(SummaryFilterApiPath.BASE_PATH + SummaryFilterApiPath.REVIEW_PRODUCTS_FILTER)
        .addParameter("storeId", SummaryFilterControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", SummaryFilterControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", SummaryFilterControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", SummaryFilterControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", SummaryFilterControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("activated", String.valueOf(Boolean.FALSE))
        .addParameter("viewable", String.valueOf(Boolean.FALSE)).addParameter("page", String.valueOf(PAGE))
        .addParameter("size", String.valueOf(SIZE)).build();
    MvcResult result = getMockMvc().perform(MockMvcRequestBuilders.post(uri).accept(MediaType.APPLICATION_JSON)
        .content(getObjectMapper().writeValueAsString(request)).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(this.summaryFilterService)
        .getReviewProductsByFilterRequestAndActivatedAndViewableFlag(Constants.DEFAULT_STORE_ID, request, Boolean.FALSE,
            Boolean.FALSE, PAGE, SIZE);
  }

  @Test
  public void getBusinessPartnersTest() throws Exception {
    Mockito.doReturn(businessPartnerResponsePage).when(this.summaryFilterService)
        .getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(Constants.DEFAULT_STORE_ID, summaryFilterServiceRequest,
            Boolean.FALSE, Boolean.FALSE, PAGE, SIZE);
    URI uri = new URIBuilder().setPath(SummaryFilterApiPath.BASE_PATH + SummaryFilterApiPath.BUSINESS_PARTNER_FILTER)
        .addParameter("storeId", SummaryFilterControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", SummaryFilterControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", SummaryFilterControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", SummaryFilterControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", SummaryFilterControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("activated", String.valueOf(Boolean.FALSE))
        .addParameter("viewable", String.valueOf(Boolean.FALSE)).addParameter("page", String.valueOf(PAGE))
        .addParameter("size", String.valueOf(SIZE)).build();
    MvcResult result = getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).content(getObjectMapper().writeValueAsString(summaryFilterServiceRequest))
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(this.summaryFilterService)
        .getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(Constants.DEFAULT_STORE_ID, summaryFilterServiceRequest,
            Boolean.FALSE, Boolean.FALSE, PAGE, SIZE);
    mapperResponseGdnRestListResponse =
        new GdnRestListResponse<>(businessPartnerResponseList, pageMetaData, DEFAULT_REQUEST_ID);
    assertEquals(getObjectMapper().writeValueAsString(mapperResponseGdnRestListResponse),
        result.getResponse().getContentAsString());
  }

  @Test
  public void getBusinessPartnersSolrServerExceptionTest() throws Exception {
    Mockito.doThrow(SolrServerException.class).when(this.summaryFilterService)
        .getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(Constants.DEFAULT_STORE_ID, summaryFilterServiceRequest,
            Boolean.FALSE, Boolean.FALSE, PAGE, SIZE);
    URI uri = new URIBuilder().setPath(SummaryFilterApiPath.BASE_PATH + SummaryFilterApiPath.BUSINESS_PARTNER_FILTER)
        .addParameter("storeId", SummaryFilterControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", SummaryFilterControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", SummaryFilterControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", SummaryFilterControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", SummaryFilterControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("activated", String.valueOf(Boolean.FALSE))
        .addParameter("viewable", String.valueOf(Boolean.FALSE)).addParameter("page", String.valueOf(PAGE))
        .addParameter("size", String.valueOf(SIZE)).addParameter("timeFilter", SolrConstants.ALL)
        .addParameter("statusFilter", SolrConstants.ALL).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).content(getObjectMapper().writeValueAsString(summaryFilterServiceRequest))
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(this.summaryFilterService)
        .getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(Constants.DEFAULT_STORE_ID, summaryFilterServiceRequest,
            Boolean.FALSE, Boolean.FALSE, PAGE, SIZE);
    mapperResponseGdnRestListResponse =
        new GdnRestListResponse<>(businessPartnerResponseList, pageMetaData, DEFAULT_REQUEST_ID);
  }

  @Test
  public void getBusinessPartnersIOExceptionTest() throws Exception {
    Mockito.doThrow(IOException.class).when(this.summaryFilterService)
        .getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(Constants.DEFAULT_STORE_ID, summaryFilterServiceRequest,
            Boolean.FALSE, Boolean.FALSE, PAGE, SIZE);
    URI uri = new URIBuilder().setPath(SummaryFilterApiPath.BASE_PATH + SummaryFilterApiPath.BUSINESS_PARTNER_FILTER)
        .addParameter("storeId", SummaryFilterControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", SummaryFilterControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", SummaryFilterControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", SummaryFilterControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", SummaryFilterControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("activated", String.valueOf(Boolean.FALSE))
        .addParameter("viewable", String.valueOf(Boolean.FALSE)).addParameter("page", String.valueOf(PAGE))
        .addParameter("size", String.valueOf(SIZE)).addParameter("timeFilter", SolrConstants.ALL)
        .addParameter("statusFilter", SolrConstants.ALL).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).content(getObjectMapper().writeValueAsString(summaryFilterServiceRequest))
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(this.summaryFilterService)
        .getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(Constants.DEFAULT_STORE_ID, summaryFilterServiceRequest,
            Boolean.FALSE, Boolean.FALSE, PAGE, SIZE);
    mapperResponseGdnRestListResponse =
        new GdnRestListResponse<>(businessPartnerResponseList, pageMetaData, DEFAULT_REQUEST_ID);
  }

  @Test
  public void getBusinessPartnersExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(this.summaryFilterService)
        .getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(Constants.DEFAULT_STORE_ID, summaryFilterServiceRequest,
            Boolean.FALSE, Boolean.FALSE, PAGE, SIZE);
    URI uri = new URIBuilder().setPath(SummaryFilterApiPath.BASE_PATH + SummaryFilterApiPath.BUSINESS_PARTNER_FILTER)
        .addParameter("storeId", SummaryFilterControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", SummaryFilterControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", SummaryFilterControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", SummaryFilterControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", SummaryFilterControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("activated", String.valueOf(Boolean.FALSE))
        .addParameter("viewable", String.valueOf(Boolean.FALSE)).addParameter("page", String.valueOf(PAGE))
        .addParameter("size", String.valueOf(SIZE)).addParameter("timeFilter", SolrConstants.ALL)
        .addParameter("statusFilter", SolrConstants.ALL).build();
    getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).content(getObjectMapper().writeValueAsString(summaryFilterServiceRequest))
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(this.summaryFilterService)
        .getBusinessPartnersByFilterRequestAndActivatedAndViewableFlag(Constants.DEFAULT_STORE_ID, summaryFilterServiceRequest,
            Boolean.FALSE, Boolean.FALSE, PAGE, SIZE);
    mapperResponseGdnRestListResponse =
        new GdnRestListResponse<>(businessPartnerResponseList, pageMetaData, DEFAULT_REQUEST_ID);
  }

  @Test
  public void getAssigneeListTest() throws Exception {
    SummaryFilterRequest request = SummaryFilterRequest.builder()
        .statusFilter(SolrConstants.ALL)
        .timeFilter(SolrConstants.ALL)
        .build();
    Mockito.doReturn(assigneeResponses).when(this.summaryFilterService)
        .getAssigneeListByFilterRequestAndActivatedAndViewableFlag(eq(Constants.DEFAULT_STORE_ID), serviceRequest.capture(),
            eq(Boolean.FALSE), eq(Boolean.FALSE));
    URI uri = new URIBuilder().setPath(SummaryFilterApiPath.BASE_PATH + SummaryFilterApiPath.ASSIGNEE_FILTER)
        .addParameter("storeId", SummaryFilterControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", SummaryFilterControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", SummaryFilterControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", SummaryFilterControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", SummaryFilterControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("activated", String.valueOf(Boolean.FALSE))
        .addParameter("viewable", String.valueOf(Boolean.FALSE))
        .addParameter("timeFilter", SolrConstants.ALL)
        .addParameter("statusFilter", SolrConstants.ALL).build();
    MvcResult result = getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).content(getObjectMapper().writeValueAsString(request)).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(this.summaryFilterService)
        .getAssigneeListByFilterRequestAndActivatedAndViewableFlag(eq(Constants.DEFAULT_STORE_ID), serviceRequest.capture(),
            eq(Boolean.FALSE), eq(Boolean.FALSE));
    assigneeResponseGdnRestListResponse =
        new GdnRestListResponse<>(assigneeResponses, new PageMetaData(), DEFAULT_REQUEST_ID);
    assertEquals(getObjectMapper().writeValueAsString(assigneeResponseGdnRestListResponse),
        result.getResponse().getContentAsString());
  }

  @Test
  public void getAssigneeListExceptionTest() throws Exception {
    SummaryFilterRequest request = SummaryFilterRequest.builder()
        .statusFilter(SolrConstants.ALL)
        .timeFilter(SolrConstants.ALL)
        .build();
    Mockito.doThrow(Exception.class).when(this.summaryFilterService)
        .getAssigneeListByFilterRequestAndActivatedAndViewableFlag(eq(Constants.DEFAULT_STORE_ID), serviceRequest.capture(),
            eq(Boolean.FALSE), eq(Boolean.FALSE));
    URI uri = new URIBuilder().setPath(SummaryFilterApiPath.BASE_PATH + SummaryFilterApiPath.ASSIGNEE_FILTER)
        .addParameter("storeId", SummaryFilterControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", SummaryFilterControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", SummaryFilterControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", SummaryFilterControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", SummaryFilterControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("activated", String.valueOf(Boolean.FALSE))
        .addParameter("viewable", String.valueOf(Boolean.FALSE))
        .build();
    MvcResult result = getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).content(getObjectMapper().writeValueAsString(request)).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(this.summaryFilterService)
        .getAssigneeListByFilterRequestAndActivatedAndViewableFlag(eq(Constants.DEFAULT_STORE_ID), serviceRequest.capture(),
            eq(Boolean.FALSE), eq(Boolean.FALSE));
  }

  @Test
  public void getAssigneeListSolrServerExceptionTest() throws Exception {
    SummaryFilterRequest request = SummaryFilterRequest.builder()
        .statusFilter(SolrConstants.ALL)
        .timeFilter(SolrConstants.ALL)
        .build();
    Mockito.doThrow(SolrServerException.class).when(this.summaryFilterService)
        .getAssigneeListByFilterRequestAndActivatedAndViewableFlag(eq(Constants.DEFAULT_STORE_ID), serviceRequest.capture(),
            eq(Boolean.FALSE), eq(Boolean.FALSE));
    URI uri = new URIBuilder().setPath(SummaryFilterApiPath.BASE_PATH + SummaryFilterApiPath.ASSIGNEE_FILTER)
        .addParameter("storeId", SummaryFilterControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", SummaryFilterControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", SummaryFilterControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", SummaryFilterControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", SummaryFilterControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("activated", String.valueOf(Boolean.FALSE))
        .addParameter("viewable", String.valueOf(Boolean.FALSE))
        .build();
    MvcResult result = getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).content(getObjectMapper().writeValueAsString(request)).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(this.summaryFilterService)
        .getAssigneeListByFilterRequestAndActivatedAndViewableFlag(eq(Constants.DEFAULT_STORE_ID), serviceRequest.capture(),
            eq(Boolean.FALSE), eq(Boolean.FALSE));
  }

  @Test
  public void getAssigneeListIOExceptionTest() throws Exception {
    SummaryFilterRequest request = SummaryFilterRequest.builder()
        .statusFilter(SolrConstants.ALL)
        .timeFilter(SolrConstants.ALL)
        .build();
    Mockito.doThrow(IOException.class).when(this.summaryFilterService)
        .getAssigneeListByFilterRequestAndActivatedAndViewableFlag(eq(Constants.DEFAULT_STORE_ID), serviceRequest.capture(),
            eq(Boolean.FALSE), eq(Boolean.FALSE));
    URI uri = new URIBuilder().setPath(SummaryFilterApiPath.BASE_PATH + SummaryFilterApiPath.ASSIGNEE_FILTER)
        .addParameter("storeId", SummaryFilterControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", SummaryFilterControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", SummaryFilterControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", SummaryFilterControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", SummaryFilterControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("activated", String.valueOf(Boolean.FALSE))
        .addParameter("viewable", String.valueOf(Boolean.FALSE))
        .build();
    MvcResult result = getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).content(getObjectMapper().writeValueAsString(request)).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(this.summaryFilterService)
        .getAssigneeListByFilterRequestAndActivatedAndViewableFlag(eq(Constants.DEFAULT_STORE_ID), serviceRequest.capture(),
            eq(Boolean.FALSE), eq(Boolean.FALSE));
  }

  @Test
  public void getVariantHistorySummaryTest() throws Exception {
    HistoryRequest historyRequest = new HistoryRequest(DEFAULT_PRODUCT_SKU, null, DEFAULT_GDN_SKU, new Date(), new Date(), false);
    Page<HistoryResponse> variantHistoryResponses = new PageImpl<>(new ArrayList<>());
    Mockito.when(summaryFilterService
        .getProductHistoryByProductSkuAndKeyword(DEFAULT_STORE_ID, historyRequest, 0, 20))
        .thenReturn(variantHistoryResponses);
    URI uri = new URIBuilder().setPath(SummaryFilterApiPath.BASE_PATH + SummaryFilterApiPath.PRODUCT_EDIT_HISTORY)
        .addParameter("storeId", SummaryFilterControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", SummaryFilterControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", SummaryFilterControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", SummaryFilterControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", SummaryFilterControllerTest.DEFAULT_CLIENT_ID)
        .addParameter("page", String.valueOf(0))
        .addParameter("size", String.valueOf(20))
        .build();
    MvcResult result = getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).content(getObjectMapper().writeValueAsString(historyRequest))
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andReturn();
    Mockito.verify(this.summaryFilterService)
        .getProductHistoryByProductSkuAndKeyword(DEFAULT_STORE_ID, historyRequest, 0, 20);
  }

  @Test
  public void getVariantHistorySummaryExceptionTest() throws Exception {
    HistoryRequest historyRequest = new HistoryRequest(DEFAULT_PRODUCT_SKU, null, DEFAULT_GDN_SKU, new Date(), new Date(), false);
    Page<HistoryResponse> variantHistoryResponses = new PageImpl<>(new ArrayList<>());
    Mockito.when(summaryFilterService
        .getProductHistoryByProductSkuAndKeyword(DEFAULT_STORE_ID, historyRequest, 0, 20))
        .thenThrow(ApplicationRuntimeException.class);
    URI uri = new URIBuilder().setPath(SummaryFilterApiPath.BASE_PATH + SummaryFilterApiPath.PRODUCT_EDIT_HISTORY)
        .addParameter("storeId", SummaryFilterControllerTest.DEFAULT_STORE_ID)
        .addParameter("channelId", SummaryFilterControllerTest.DEFAULT_CHANNEL_ID)
        .addParameter("requestId", SummaryFilterControllerTest.DEFAULT_REQUEST_ID)
        .addParameter("username", SummaryFilterControllerTest.DEFAULT_USERNAME)
        .addParameter("clientId", SummaryFilterControllerTest.DEFAULT_CLIENT_ID).addParameter("page", String.valueOf(0))
        .addParameter("size", String.valueOf(20)).build();
    MvcResult result = getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).content(getObjectMapper().writeValueAsString(historyRequest))
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
        .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(this.summaryFilterService)
        .getProductHistoryByProductSkuAndKeyword(DEFAULT_STORE_ID, historyRequest, 0, 20);
  }

  @Test
  public void getProductHistorySummaryTest() throws Exception {
    HistoryUpdateRequest historyUpdateRequest = new HistoryUpdateRequest();
    Mockito.when(
      summaryFilterService.getProductUpdateHistoryByRequest(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID,
        historyUpdateRequest, 0, 20)).thenReturn(new PageImpl<>(new ArrayList<>()));
    URI uri = new URIBuilder().setPath(SummaryFilterApiPath.BASE_PATH + SummaryFilterApiPath.PRODUCT_UPDATE_HISTORY)
      .addParameter("storeId", DEFAULT_STORE_ID)
      .addParameter("channelId", DEFAULT_CHANNEL_ID)
      .addParameter("requestId", DEFAULT_REQUEST_ID)
      .addParameter("username", DEFAULT_USERNAME)
      .addParameter("clientId", DEFAULT_CLIENT_ID)
      .addParameter("page", String.valueOf(0))
      .addParameter("size", String.valueOf(20))
      .build();
    MvcResult result = getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).content(getObjectMapper().writeValueAsString(historyUpdateRequest))
          .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.status().isOk())
      .andReturn();
    Mockito.verify(this.summaryFilterService)
      .getProductUpdateHistoryByRequest(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID,
        historyUpdateRequest, 0, 20);
  }

  @Test
  public void getProductHistorySummaryExceptionTest() throws Exception {
    HistoryUpdateRequest historyUpdateRequest = new HistoryUpdateRequest();
    Mockito.when(
      summaryFilterService.getProductUpdateHistoryByRequest(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID,
        historyUpdateRequest, 0, 20)).thenThrow(ApplicationRuntimeException.class);
    URI uri = new URIBuilder().setPath(SummaryFilterApiPath.BASE_PATH + SummaryFilterApiPath.PRODUCT_UPDATE_HISTORY)
      .addParameter("storeId", DEFAULT_STORE_ID)
      .addParameter("channelId", DEFAULT_CHANNEL_ID)
      .addParameter("requestId", DEFAULT_REQUEST_ID)
      .addParameter("username", DEFAULT_USERNAME)
      .addParameter("clientId", DEFAULT_CLIENT_ID).addParameter("page", String.valueOf(0))
      .addParameter("size", String.valueOf(20)).build();
    MvcResult result = getMockMvc().perform(
        MockMvcRequestBuilders.post(uri).content(getObjectMapper().writeValueAsString(historyUpdateRequest))
          .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON))
      .andExpect(MockMvcResultMatchers.status().isOk()).andReturn();
    Mockito.verify(this.summaryFilterService)
      .getProductUpdateHistoryByRequest(DEFAULT_STORE_ID, DEFAULT_REQUEST_ID,
        historyUpdateRequest, 0, 20);
  }

  public void setMockMvc(MockMvc mockMvc) {
    this.mockMvc = mockMvc;
  }

  public MockMvc getMockMvc() {
    return this.mockMvc;
  }


  public void setObjectMapper(ObjectMapper objectMapper) {
    this.objectMapper = objectMapper;
  }

  public ObjectMapper getObjectMapper() {
    return objectMapper;
  }
}