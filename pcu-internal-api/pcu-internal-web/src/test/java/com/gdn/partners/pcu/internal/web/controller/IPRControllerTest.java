package com.gdn.partners.pcu.internal.web.controller;

import com.gdn.common.web.wrapper.response.GdnBaseRestResponse;
import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.client.model.request.IPRProductListRequest;
import com.gdn.partners.pcu.internal.client.model.request.IprActionRequest;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductDetailResponse;
import com.gdn.partners.pcu.internal.client.model.request.IPRUpdateAssigneeRequest;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductHistoryResponse;
import com.gdn.partners.pcu.internal.client.model.response.IPRProductListResponse;
import com.gdn.partners.pcu.internal.client.model.response.IprSuspensionInProgressResponse;
import com.gdn.partners.pcu.internal.client.model.response.IprSuspensionInProgressWebResponse;
import com.gdn.partners.pcu.internal.model.AutoApprovedApiPath;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.IPRApiPath;
import com.gdn.partners.pcu.internal.service.IPRService;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;
import com.gdn.partners.pcu.internal.web.model.request.IPRProductsDownloadWebRequest;
import com.gdn.x.mta.distributiontask.rest.model.response.MapResponse;
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
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@ExtendWith(MockitoExtension.class)
class IPRControllerTest extends TestHelper {

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @Mock
  private IPRService iprService;

  @InjectMocks
  private IPRController iprController;

  private static final int PAGE = 0;
  private static final int SIZE = 25;
  private static final String SORT_ORDER = "ASC";
  private static final String PRODUCT_SKU = "productSku";
  private static final String BUSINESS_PARTNER_CODE = "BP-1234";
  private final IPRProductListRequest iprProductListRequest = new IPRProductListRequest();
  private final IPRProductListResponse iprProductListResponse = new IPRProductListResponse();
  private final List<IPRProductListResponse> iprProductListResponseList = new ArrayList<>();
  private final IprSuspensionInProgressResponse iprSuspensionInProgressResponse =
      new IprSuspensionInProgressResponse();
  private final IprSuspensionInProgressWebResponse iprSuspensionInProgressWebResponse =
      new IprSuspensionInProgressWebResponse();
  private final List<IprSuspensionInProgressWebResponse> iprSuspensionInProgressWebResponseList =
      new ArrayList<>();
  private final List<IprSuspensionInProgressResponse> iprSuspensionInProgressResponseList =
      new ArrayList<>();
  private Page<IPRProductListResponse> iprProductPage;
  private Page<IprSuspensionInProgressResponse> iprSuspensionInProgressResponsePage;
  private Page<IPRProductHistoryResponse> iprProductHistoryResponses;
  private Page<IprSuspensionInProgressWebResponse> iprSuspensionInProgressWebResponsePage;

  @BeforeEach
  public void init() throws IOException {
    mockMvc = MockMvcBuilders.standaloneSetup(iprController).build();
    iprProductListResponseList.add(iprProductListResponse);
    iprProductPage = new PageImpl<>(iprProductListResponseList, PageRequest.of(PAGE, SIZE), SIZE);
    iprSuspensionInProgressResponseList.add(iprSuspensionInProgressResponse);
    iprSuspensionInProgressWebResponseList.add(iprSuspensionInProgressWebResponse);
    iprSuspensionInProgressResponsePage =
        new PageImpl<>(iprSuspensionInProgressResponseList, PageRequest.of(PAGE, SIZE), SIZE);
    iprSuspensionInProgressWebResponsePage =
        new PageImpl<>(iprSuspensionInProgressWebResponseList, PageRequest.of(PAGE, SIZE), SIZE);
  }

  @AfterEach
  public void tearDown() throws IOException {
    Mockito.verifyNoMoreInteractions(iprService);
    Mockito.verifyNoMoreInteractions(clientParameterHelper);
  }

  @Test
  void iprProductListTest() throws Exception {
    when(iprService.getIPRProductList(eq(PAGE), eq(SIZE),
        any(IPRProductListRequest.class))).thenReturn(iprProductPage);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(IPRApiPath.BASE_PATH + IPRApiPath.IPR_PRODUCT_LIST).content(
                toJson(iprProductListRequest)).param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(iprService).getIPRProductList(eq(PAGE), eq(SIZE), any(IPRProductListRequest.class));
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  void getIPRReviewerListTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(iprService.getIPRReviewers()).thenReturn(new ArrayList<>());
    MockHttpServletRequestBuilder requestBuilder =
      get(IPRApiPath.BASE_PATH + IPRApiPath.IPR_REVIEWERS).contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(Boolean.TRUE)));
    Mockito.verify(iprService).getIPRReviewers();
    verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  void getIPRProductDetailsTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(iprService.getIPRProductDetailByProductSku(PRODUCT_SKU))
        .thenReturn(new IPRProductDetailResponse());
    MockHttpServletRequestBuilder requestBuilder =
        get(IPRApiPath.BASE_PATH + IPRApiPath.IPR_PRODUCT_DETAIL, PRODUCT_SKU).contentType(
            MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(Boolean.TRUE)));
    Mockito.verify(iprService).getIPRProductDetailByProductSku(PRODUCT_SKU);
    verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  void updateIPRAssigneeTest() throws Exception {
    IPRUpdateAssigneeRequest iprUpdateAssigneeRequest = IPRUpdateAssigneeRequest.builder().build();
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.doNothing().when(iprService).updateAssignee(iprUpdateAssigneeRequest);
    MockHttpServletRequestBuilder requestBuilder =
        post(IPRApiPath.BASE_PATH + IPRApiPath.UPDATE_ASSIGNEE).content(
                toJson(iprUpdateAssigneeRequest)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(Boolean.TRUE)));
    Mockito.verify(iprService).updateAssignee(iprUpdateAssigneeRequest);
    verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  void getProductFilterCountsTest() throws Exception {
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.when(iprService.getPrimaryFilterCounts()).thenReturn(new MapResponse());
    MockHttpServletRequestBuilder requestBuilder =
      get(IPRApiPath.BASE_PATH + IPRApiPath.PRIMARY_FILTER).contentType(MediaType.APPLICATION_JSON)
        .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(iprService).getPrimaryFilterCounts();
    Mockito.verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  void getSuspensionInProgressIprProductsTest() throws Exception {
    when(iprService.getSuspensionInProcessProducts(PAGE, SIZE, BUSINESS_PARTNER_CODE,
      SORT_ORDER)).thenReturn(iprSuspensionInProgressWebResponsePage);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
      get(IPRApiPath.BASE_PATH + IPRApiPath.SUSPENSION_IN_PROGRESS).param("businessPartnerCode",
          BUSINESS_PARTNER_CODE).param("page", String.valueOf(PAGE))
        .param("size", String.valueOf(SIZE)).param("sortOtder", SORT_ORDER)
        .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    verify(iprService).getSuspensionInProcessProducts(PAGE, SIZE, BUSINESS_PARTNER_CODE,
      SORT_ORDER);
    verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  void performIprActionTest() throws Exception {
    IprActionRequest iprActionRequest = new IprActionRequest();
    iprActionRequest.setUpdatedBy(Constants.USER_NAME);
    GdnBaseRestResponse response = new GdnBaseRestResponse();
    response.setSuccess(true);
    Mockito.when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
      Mockito.when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    Mockito.when(iprService.performIprAction(iprActionRequest)).thenReturn(response);
    MockHttpServletRequestBuilder requestBuilder =
        post(IPRApiPath.BASE_PATH + IPRApiPath.PERFORM_IPR_ACTION).content(toJson(iprActionRequest))
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(Boolean.TRUE)));
    Mockito.verify(iprService).performIprAction(iprActionRequest);
    verify(this.clientParameterHelper).getRequestId();
    verify(this.clientParameterHelper).getUsername();
  }

  @Test
  void fetchIprHistoryTest() throws Exception {
    iprProductHistoryResponses = new PageImpl<>(Collections.singletonList(IPRProductHistoryResponse.builder()
        .build()));
    when(iprService.fetchIprProductHistory(PAGE, SIZE, PRODUCT_SKU)).thenReturn(
        iprProductHistoryResponses);
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        get(IPRApiPath.BASE_PATH + IPRApiPath.HISTORY).param("productSku",
                PRODUCT_SKU).param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(iprService).fetchIprProductHistory(PAGE, SIZE, PRODUCT_SKU);
    verify(this.clientParameterHelper).getRequestId();
  }

  @Test
  void downloadIPRProductTest() throws Exception {
    IPRProductsDownloadWebRequest iprProductsDownloadWebRequest =
        new IPRProductsDownloadWebRequest();
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(clientParameterHelper.getUsername()).thenReturn(Constants.USER_NAME);
    MockHttpServletRequestBuilder requestBuilder =
        post(IPRApiPath.BASE_PATH + IPRApiPath.IPR_PRODUCTS_DOWNLOAD).content(
                toJson(iprProductsDownloadWebRequest)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(iprService)
        .downloadIPRProducts(Constants.USER_NAME, iprProductsDownloadWebRequest);
    verify(clientParameterHelper).getRequestId();
    verify(clientParameterHelper).getUsername();
  }
}
