package com.gdn.partners.pcu.external.web.controller;

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.partners.pcu.external.client.helper.MandatoryParameterHelper;
import com.gdn.partners.pcu.external.model.Constants;
import com.gdn.partners.pcu.external.model.ProductAssemblyApiPath;
import com.gdn.partners.pcu.external.service.ProductAssemblyService;
import com.gdn.partners.pcu.external.web.helper.TestHelper;
import com.gdn.partners.pcu.external.web.model.request.SimpleListAssemblyDisassemblyRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.request.MockHttpServletRequestBuilder;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.ArrayList;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;


public class ProductAssemblyControllerTest extends TestHelper {

  private static final int PAGE =0;
  private static final int SIZE =50;
  private static final String LISTING_REQUEST =
      "{\"merchantCode\":\"string\",\"type\":\"string\",\"status\":\"string\",\"startDate\":\"2023-12-11T06:44:59.467Z\","
          + "\"endDate\":\"2023-12-11T06:44:59.467Z\",\"keyword\":\"string\"}";
  private static final String REQUEST_FORM_NUMBER = "requestFormNumber";
  private static final String TYPE = "TRANSFER_REQUEST";
  private static final String RETRY_TYPE = "RETRY";
  private static final String CANCEL_TYPE = "CANCEL";
  private static final String REQUEST_ID = "requestId";
  private static final String SORT_ORDER = "ASC";
  private static final String MERCHANT_CODE = "merchantCode";

  @InjectMocks
  private ProductAssemblyController productAssemblyController;

  @Mock
  private ProductAssemblyService productAssemblyService;

  @Mock
  private MandatoryParameterHelper mandatoryParameterHelper;



  private SimpleListAssemblyDisassemblyRequest simpleListAssemblyDisassemblyRequest;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.initMocks(this);
    mockMvc = MockMvcBuilders.standaloneSetup(productAssemblyController).build();
    simpleListAssemblyDisassemblyRequest = new SimpleListAssemblyDisassemblyRequest();
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(mandatoryParameterHelper);
    verifyNoMoreInteractions(productAssemblyService);
  }


  @Test
  public void getWarehouseCodeAndFulfillmentCenterTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(productAssemblyService.getMasterWarehouseListResponse()).thenReturn(
        new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(1, 0, 1), null));
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductAssemblyApiPath.BASE_PATH + ProductAssemblyApiPath.GET_WAREHOUSE_CODE_AND_NAME_LIST).accept(
                MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
            .param("storeId", Constants.STORE_ID).param("channelId", Constants.CHANNEL_ID)
            .param("clientId", Constants.CLIENT_ID).param("requestId", Constants.REQUEST_ID);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(productAssemblyService).getMasterWarehouseListResponse();
  }

  @Test
  public void getAssemblyDisAssemblyAndTransferRequestListingResponseTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(productAssemblyService.getRequestFormsListingResponse(anyInt(), anyInt(), any(), any()))
        .thenReturn(new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(1, 0, 1), null));
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductAssemblyApiPath.BASE_PATH + ProductAssemblyApiPath.GET_REQUEST_FORMS_LISTING)
            .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
            .param("storeId", Constants.STORE_ID).param("channelId", Constants.CHANNEL_ID)
            .param("clientId", Constants.CLIENT_ID).param("requestId", Constants.REQUEST_ID).param("page", "0")
            .param("size", "50").content(LISTING_REQUEST);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(mandatoryParameterHelper).getRequestId();
    verify(productAssemblyService).getRequestFormsListingResponse(anyInt(), anyInt(), any(), any());
  }

  @Test
  public void getAssemblyDisAssemblyAndTransferRequestListingResponseExceptionTest() throws Exception {
    doThrow(new ApplicationRuntimeException()).when(productAssemblyService)
        .getRequestFormsListingResponse(anyInt(), anyInt(), any(), any());
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductAssemblyApiPath.BASE_PATH + ProductAssemblyApiPath.GET_REQUEST_FORMS_LISTING)
            .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
            .param("storeId", Constants.STORE_ID).param("channelId", Constants.CHANNEL_ID)
            .param("clientId", Constants.CLIENT_ID).param("requestId", Constants.REQUEST_ID).param("page", "0")
            .param("size", "50").content(LISTING_REQUEST);
    try {
      mockMvc.perform(requestBuilder).andExpect(status().isOk());
    } finally {
      verify(productAssemblyService).getRequestFormsListingResponse(anyInt(), anyInt(), any(), any());
      verify(mandatoryParameterHelper).getRequestId();
    }
  }

  @Test
  public void getRequestFormHistoryTest() throws Exception {
    Mockito.when(
        productAssemblyService.getRequestHistory(REQUEST_FORM_NUMBER, PAGE, SIZE, SORT_ORDER, Constants.REQUEST_ID,
            MERCHANT_CODE)).thenReturn(new GdnRestListResponse<>(new ArrayList<>(), new PageMetaData(1, 0, 1), null));
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(MERCHANT_CODE);
    MockHttpServletRequestBuilder requestBuilder =
        get(ProductAssemblyApiPath.BASE_PATH + ProductAssemblyApiPath.GET_HISTORY)
            .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE).param("page", "0")
            .param("size", "50").param("requestFormNumber", REQUEST_FORM_NUMBER).param("sortOrder", SORT_ORDER);
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(productAssemblyService).getRequestHistory(REQUEST_FORM_NUMBER, PAGE, SIZE, SORT_ORDER, Constants.REQUEST_ID, MERCHANT_CODE);
    verify(mandatoryParameterHelper, times(2)).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void createAssemblyDisAssemblyAndTransferRequestsTest() throws Exception {
    when(mandatoryParameterHelper.getRequestId()).thenReturn(Constants.REQUEST_ID);
    when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(Constants.BUSINESS_PARTNER_CODE);
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductAssemblyApiPath.BASE_PATH + ProductAssemblyApiPath.CREATE_REQUEST, TYPE)
            .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
            .content(toJson(simpleListAssemblyDisassemblyRequest));
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    verify(productAssemblyService).createAssemblyDisAssemblyAndTransferRequests(Constants.BUSINESS_PARTNER_CODE,
        TYPE, simpleListAssemblyDisassemblyRequest);
    verify(mandatoryParameterHelper).getRequestId();
    verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void cancelOrRetryRequestTest() throws Exception {
    Mockito.when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    Mockito.when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(MERCHANT_CODE);
    Mockito.when(productAssemblyService.cancelOrRetry(REQUEST_FORM_NUMBER, RETRY_TYPE, "", REQUEST_ID))
        .thenReturn(new GdnRestListResponse<>(null, null, true, REQUEST_ID));
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductAssemblyApiPath.BASE_PATH + ProductAssemblyApiPath.RETRY_OR_CANCEL)
            .accept(MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
            .param("requestFormNumber", REQUEST_FORM_NUMBER).param("type", "RETRY");
    mockMvc.perform(requestBuilder).andExpect(status().isOk());
    Mockito.verify(productAssemblyService).cancelOrRetry(REQUEST_FORM_NUMBER, RETRY_TYPE, MERCHANT_CODE, REQUEST_ID);
    Mockito.verify(mandatoryParameterHelper, times(1)).getRequestId();
    Mockito.verify(mandatoryParameterHelper).getBusinessPartnerCode();
  }

  @Test
  public void cancelOrRetryRequestExceptionTest() throws Exception {
    Mockito.when(mandatoryParameterHelper.getRequestId()).thenReturn(REQUEST_ID);
    Mockito.when(mandatoryParameterHelper.getBusinessPartnerCode()).thenReturn(MERCHANT_CODE);
    Mockito.doThrow(new ApplicationRuntimeException()).when(productAssemblyService)
        .cancelOrRetry(REQUEST_FORM_NUMBER, RETRY_TYPE, MERCHANT_CODE, REQUEST_ID);
    MockHttpServletRequestBuilder requestBuilder =
        post(ProductAssemblyApiPath.BASE_PATH + ProductAssemblyApiPath.RETRY_OR_CANCEL).accept(
                MediaType.APPLICATION_JSON_VALUE).contentType(MediaType.APPLICATION_JSON_VALUE)
            .param("requestFormNumber", REQUEST_FORM_NUMBER).param("type", "RETRY");
    try {
      Assertions.assertThrows(Exception.class,
          () -> mockMvc.perform(requestBuilder).andExpect(status().is5xxServerError()));
    } finally {
      Mockito.verify(productAssemblyService)
          .cancelOrRetry(REQUEST_FORM_NUMBER, RETRY_TYPE, MERCHANT_CODE, REQUEST_ID);
      Mockito.verify(mandatoryParameterHelper, times(1)).getRequestId();
      Mockito.verify(mandatoryParameterHelper).getBusinessPartnerCode();
    }
  }

}
