package com.gdn.partners.pcu.internal.web.controller;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Collections;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.gdn.partners.pcu.internal.client.helper.ClientParameterHelper;
import com.gdn.partners.pcu.internal.model.Constants;
import com.gdn.partners.pcu.internal.model.QCApiPath;
import com.gdn.partners.pcu.internal.service.QCTaskService;
import com.gdn.partners.pcu.internal.web.helper.TestHelper;
import com.gdn.partners.pcu.internal.web.model.request.RejectProductWebRequest;
import com.gdn.partners.pcu.internal.web.model.request.SummaryFilterWebRequest;
import com.gdn.partners.pcu.internal.web.model.response.BusinessPartnerWebResponse;
import com.gdn.partners.pcu.internal.web.model.response.DistributionProductWebResponse;

@AutoConfigureMockMvc
@ExtendWith(MockitoExtension.class)
public class QCControllerTest extends TestHelper {

  private static final String PRODUCT_NAME = "product_name";
  private static final String PRODUCT_CODE = "product_code";
  private static final String PRODUCT_ID = "product_id";
  private static final String BUSINESS_PARTNER_CODE = "businessPartnerCode";
  private static final String CATEGORY_CODE = "categoryCode";
  private static final String START_DATE = "startDate";
  private static final String END_DATE = "endDate";
  private static final String STATUS = "ACTIVE";
  private static final String REASON = "reason";
  private static final String REJECTED_TYPE = "rejectedType";
  private static final String KEYWORD = "keyword";
  private static final int PAGE = 0;
  private static final int SIZE = 25;

  private SummaryFilterWebRequest summaryFilterWebRequest;
  private RejectProductWebRequest rejectProductWebRequest;

  @Mock
  private ClientParameterHelper clientParameterHelper;

  @Mock
  private QCTaskService qcTaskService;

  @InjectMocks
  private QCController qcController;

  @BeforeEach
  public void init() {
    mockMvc = MockMvcBuilders.standaloneSetup(qcController).build();

    summaryFilterWebRequest =
        SummaryFilterWebRequest.builder().build().builder().businessPartnerCode(BUSINESS_PARTNER_CODE)
            .categoryCode(CATEGORY_CODE).startDate(START_DATE).endDate(END_DATE).keyword(PRODUCT_NAME).status(STATUS)
            .build();
    rejectProductWebRequest =
        RejectProductWebRequest.builder().productId(PRODUCT_ID).isAssignedToVendor(true).rejectedReason(REASON)
            .rejectedType(REJECTED_TYPE).build();
  }

  @Test
  public void filterProductTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(qcTaskService.filterQCProductList(summaryFilterWebRequest, PAGE, SIZE))
        .thenReturn(new PageImpl<>(Collections.singletonList(new DistributionProductWebResponse())));
    MockHttpServletRequestBuilder requestBuilder =
        post(QCApiPath.BASE_PATH + QCApiPath.FILTER_QC_READY_PRODUCT).param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).content(toJson(summaryFilterWebRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    verify(qcTaskService).filterQCProductList(summaryFilterWebRequest, PAGE, SIZE);
    verify(clientParameterHelper).getRequestId();
  }

  @Test
  public void retryProductActivationTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.doNothing().when(qcTaskService).retryApproveQc(PRODUCT_CODE);
    MockHttpServletRequestBuilder requestBuilder =
        post(QCApiPath.BASE_PATH + QCApiPath.RETRY_PRODUCT_ACTIVATION).param("productCode", PRODUCT_CODE)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(qcTaskService).retryApproveQc(PRODUCT_CODE);
    verify(clientParameterHelper).getRequestId();  }

  @Test
  public void rejectProductTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.doNothing().when(qcTaskService).rejectProduct(rejectProductWebRequest);
    MockHttpServletRequestBuilder requestBuilder =
        post(QCApiPath.BASE_PATH + QCApiPath.REJECT_PRODUCT).param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE)).contentType(MediaType.APPLICATION_JSON)
            .accept(MediaType.APPLICATION_JSON).content(toJson(rejectProductWebRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(qcTaskService).rejectProduct(rejectProductWebRequest);
    verify(clientParameterHelper).getRequestId();  }

  @Test
  public void approveProductTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    Mockito.doNothing().when(qcTaskService).approveProduct(PRODUCT_CODE, PRODUCT_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(QCApiPath.BASE_PATH + QCApiPath.APPROVE_PRODUCT).param("productCode", PRODUCT_CODE)
            .param("productId", PRODUCT_ID).contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(qcTaskService).approveProduct(PRODUCT_CODE, PRODUCT_ID);
    verify(clientParameterHelper).getRequestId();  }

  @Test
  public void getBusinessPartnerListTest() throws Exception {
    when(clientParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(qcTaskService.getBusinessPartnerList(STATUS, KEYWORD, PAGE, SIZE))
        .thenReturn(new PageImpl<>(Collections.singletonList(new BusinessPartnerWebResponse())));
    MockHttpServletRequestBuilder requestBuilder =
        get(QCApiPath.BASE_PATH + QCApiPath.BUSINESS_PARTNER_LIST).param("page", String.valueOf(PAGE))
            .param("size", String.valueOf(SIZE)).param("state", STATUS).param(KEYWORD, KEYWORD)
            .contentType(MediaType.APPLICATION_JSON).accept(MediaType.APPLICATION_JSON);
    mockMvc.perform(requestBuilder).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(true)));

    verify(qcTaskService).getBusinessPartnerList(STATUS, KEYWORD, PAGE, SIZE);
    verify(clientParameterHelper).getRequestId();  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(qcTaskService);
    verifyNoMoreInteractions(clientParameterHelper);
  }
}
