package com.gdn.x.product.rest.web.controller.api;

import static org.hamcrest.Matchers.containsString;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.model.vo.Off2OnPriceVO;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.Off2OnPriceResponse;
import com.gdn.x.product.service.api.Off2OnService;
import com.gdn.x.product.service.util.ModelConverter;

public class Off2OnControllerTest {

  private static final String MERCHANT_CODE = "merchantCode";

  private static final String ERROR_MESSAGE = "errorMessage";

  private static final String STORE_ID = "storeId";

  private static final String CHANNEL_ID = "channelId";

  private static final String CLIENT_ID = "clientId";

  private static final String REQUEST_ID = "requestId";

  private static final String USERNAME = "username";
  private static final String CHANNEL = "channel";
  private static final String ITEM_SKU = "itemSku";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  @InjectMocks
  private Off2OnController sut;
  @Mock
  private Off2OnService off2OnService;
  @Mock
  private ModelConverter modelConverter;
  private ObjectMapper objectMapper;
  private MockMvc mockMvc;

  @Test
  public void activateOff2OnChannelBulkApplicationExceptionTest() throws Exception {
    List<String> itemSkus = new ArrayList<String>();
    itemSkus.add(Off2OnControllerTest.ITEM_SKU);
    SimpleListStringRequest request = new SimpleListStringRequest(itemSkus);
    String jsonRequest = this.objectMapper.writeValueAsString(request);
    boolean result = false;
    when(this.off2OnService.activateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID, itemSkus))
        .thenThrow(new ApplicationRuntimeException());
    this.mockMvc
        .perform(
            post(ProductApiPath.OFF2ON_ACTIVATE_BULK).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(jsonRequest)
                .param("storeId", Off2OnControllerTest.STORE_ID)
                .param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID)
                .param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(result));
    verify(this.off2OnService).activateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID,
        itemSkus);
  }

  @Test
  public void activateOff2OnChannelBulkExceptionTest() throws Exception {
    List<String> itemSkus = new ArrayList<String>();
    itemSkus.add(Off2OnControllerTest.ITEM_SKU);
    SimpleListStringRequest request = new SimpleListStringRequest(itemSkus);
    String jsonRequest = this.objectMapper.writeValueAsString(request);
    boolean result = false;
    when(this.off2OnService.activateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID, itemSkus))
        .thenThrow(new RuntimeException());
    this.mockMvc
        .perform(
            post(ProductApiPath.OFF2ON_ACTIVATE_BULK).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(jsonRequest)
                .param("storeId", Off2OnControllerTest.STORE_ID)
                .param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID)
                .param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(result));
    verify(this.off2OnService).activateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID,
        itemSkus);
  }

  @Test
  public void activateOff2OnChannelBulkTest() throws Exception {
    List<String> itemSkus = new ArrayList<String>();
    itemSkus.add(Off2OnControllerTest.ITEM_SKU);
    SimpleListStringRequest request = new SimpleListStringRequest(itemSkus);
    String jsonRequest = this.objectMapper.writeValueAsString(request);
    boolean result = true;
    when(this.off2OnService.activateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID, itemSkus))
        .thenReturn(new ArrayList<String>());
    this.mockMvc
        .perform(
            post(ProductApiPath.OFF2ON_ACTIVATE_BULK).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(jsonRequest)
                .param("storeId", Off2OnControllerTest.STORE_ID)
                .param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID)
                .param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(result));
    verify(this.off2OnService).activateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID,
        itemSkus);
  }

  @Test
  public void activateOff2OnChannelTest() throws Exception {
    boolean result = true;
    when(
        this.off2OnService.activateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID,
            Off2OnControllerTest.ITEM_SKU)).thenReturn(result);
    this.mockMvc
        .perform(
            post(ProductApiPath.OFF2ON_ACTIVATE).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", Off2OnControllerTest.STORE_ID)
                .param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID)
                .param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME)
                .param("itemSku", Off2OnControllerTest.ITEM_SKU)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(result));
    verify(this.off2OnService).activateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID,
        Off2OnControllerTest.ITEM_SKU);
  }

  @Test
  public void activateOff2OnChannelWithApplicationExceptionTest() throws Exception {
    when(
        this.off2OnService.activateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID,
            Off2OnControllerTest.ITEM_SKU)).thenThrow(
        new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION,
            Off2OnControllerTest.ERROR_MESSAGE));
    this.mockMvc
        .perform(
            post(ProductApiPath.OFF2ON_ACTIVATE).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", Off2OnControllerTest.STORE_ID)
                .param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID)
                .param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME)
                .param("itemSku", Off2OnControllerTest.ITEM_SKU))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(false))
        .andExpect(
            jsonPath("$.errorMessage").value(containsString(Off2OnControllerTest.ERROR_MESSAGE)))
        .andExpect(
            jsonPath("$.errorCode").value(
                ProductErrorCodesEnum.CHANGE_OFF2ON_CHANNEL_ACTIVE_FAILED.getCode()));
    verify(this.off2OnService).activateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID,
        Off2OnControllerTest.ITEM_SKU);
  }

  @Test
  public void activateOff2OnChannelWithExceptionTest() throws Exception {
    when(
        this.off2OnService.activateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID,
            Off2OnControllerTest.ITEM_SKU)).thenThrow(
        new RuntimeException(Off2OnControllerTest.ERROR_MESSAGE));
    this.mockMvc
        .perform(
            post(ProductApiPath.OFF2ON_ACTIVATE).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", Off2OnControllerTest.STORE_ID)
                .param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID)
                .param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME)
                .param("itemSku", Off2OnControllerTest.ITEM_SKU))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(false))
        .andExpect(jsonPath("$.errorMessage").value(Off2OnControllerTest.ERROR_MESSAGE))
        .andExpect(
            jsonPath("$.errorCode").value(
                ProductErrorCodesEnum.CHANGE_OFF2ON_CHANNEL_ACTIVE_FAILED.getCode()));
    verify(this.off2OnService).activateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID,
        Off2OnControllerTest.ITEM_SKU);
  }

  @Test
  public void activateOff2OnChannelWithMerchantCodeApplicationExceptionTest() throws Exception {
    boolean result = false;
    when(
        this.off2OnService.activateOff2OnChannelByMerchantCode(Off2OnControllerTest.STORE_ID,
            Off2OnControllerTest.MERCHANT_CODE)).thenThrow(new ApplicationRuntimeException());
    this.mockMvc
        .perform(
            post(ProductApiPath.OFF2ON_ACTIVATE_BY_MERCHANT_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", Off2OnControllerTest.STORE_ID)
                .param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID)
                .param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME)
                .param("merchantCode", Off2OnControllerTest.MERCHANT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(result));
    verify(this.off2OnService).activateOff2OnChannelByMerchantCode(Off2OnControllerTest.STORE_ID,
        Off2OnControllerTest.MERCHANT_CODE);
  }

  @Test
  public void activateOff2OnChannelWithMerchantCodeExceptionTest() throws Exception {
    boolean result = false;
    when(
        this.off2OnService.activateOff2OnChannelByMerchantCode(Off2OnControllerTest.STORE_ID,
            Off2OnControllerTest.MERCHANT_CODE)).thenThrow(new RuntimeException());
    this.mockMvc
        .perform(
            post(ProductApiPath.OFF2ON_ACTIVATE_BY_MERCHANT_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", Off2OnControllerTest.STORE_ID)
                .param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID)
                .param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME)
                .param("merchantCode", Off2OnControllerTest.MERCHANT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(result));
    verify(this.off2OnService).activateOff2OnChannelByMerchantCode(Off2OnControllerTest.STORE_ID,
        Off2OnControllerTest.MERCHANT_CODE);
  }

  @Test
  public void activateOff2OnChannelWithMerchantCodeTest() throws Exception {
    boolean result = true;
    ArrayList<String> expectedResult = new ArrayList<String>();
    when(
        this.off2OnService.activateOff2OnChannelByMerchantCode(Off2OnControllerTest.STORE_ID,
            Off2OnControllerTest.MERCHANT_CODE)).thenReturn(expectedResult);
    this.mockMvc
        .perform(
            post(ProductApiPath.OFF2ON_ACTIVATE_BY_MERCHANT_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", Off2OnControllerTest.STORE_ID)
                .param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID)
                .param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME)
                .param("merchantCode", Off2OnControllerTest.MERCHANT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(result));
    verify(this.off2OnService).activateOff2OnChannelByMerchantCode(Off2OnControllerTest.STORE_ID,
        Off2OnControllerTest.MERCHANT_CODE);
  }

  @Test
  public void deactivateOff2OnChannelBulkApplicationExceptionTest() throws Exception {
    List<String> itemSkus = new ArrayList<String>();
    itemSkus.add(Off2OnControllerTest.ITEM_SKU);
    SimpleListStringRequest request = new SimpleListStringRequest(itemSkus);
    String jsonRequest = this.objectMapper.writeValueAsString(request);
    boolean result = false;
    when(
        this.off2OnService
            .deactivateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID, itemSkus)).thenThrow(
        new ApplicationRuntimeException());
    this.mockMvc
        .perform(
            post(ProductApiPath.OFF2ON_DEACTIVATE_BULK).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(jsonRequest)
                .param("storeId", Off2OnControllerTest.STORE_ID)
                .param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID)
                .param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(result));
    verify(this.off2OnService).deactivateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID,
        itemSkus);
  }

  @Test
  public void deactivateOff2OnChannelBulkExceptionTest() throws Exception {
    List<String> itemSkus = new ArrayList<String>();
    itemSkus.add(Off2OnControllerTest.ITEM_SKU);
    SimpleListStringRequest request = new SimpleListStringRequest(itemSkus);
    String jsonRequest = this.objectMapper.writeValueAsString(request);
    boolean result = false;
    when(
        this.off2OnService
            .deactivateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID, itemSkus)).thenThrow(
        new RuntimeException());
    this.mockMvc
        .perform(
            post(ProductApiPath.OFF2ON_DEACTIVATE_BULK).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(jsonRequest)
                .param("storeId", Off2OnControllerTest.STORE_ID)
                .param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID)
                .param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(result));
    verify(this.off2OnService).deactivateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID,
        itemSkus);
  }

  @Test
  public void deactivateOff2OnChannelBulkTest() throws Exception {
    List<String> itemSkus = new ArrayList<String>();
    itemSkus.add(Off2OnControllerTest.ITEM_SKU);
    SimpleListStringRequest request = new SimpleListStringRequest(itemSkus);
    String jsonRequest = this.objectMapper.writeValueAsString(request);
    boolean result = true;
    when(
        this.off2OnService
            .deactivateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID, itemSkus)).thenReturn(
        new ArrayList<String>());
    this.mockMvc
        .perform(
            post(ProductApiPath.OFF2ON_DEACTIVATE_BULK).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(jsonRequest)
                .param("storeId", Off2OnControllerTest.STORE_ID)
                .param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID)
                .param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(result));
    verify(this.off2OnService).deactivateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID,
        itemSkus);
  }

  @Test
  public void deactivateOff2OnChannelTest() throws Exception {
    boolean result = true;
    when(
        this.off2OnService.deactivateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID,
            Off2OnControllerTest.ITEM_SKU)).thenReturn(result);
    this.mockMvc
        .perform(
            post(ProductApiPath.OFF2ON_DEACTIVATE).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", Off2OnControllerTest.STORE_ID)
                .param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID)
                .param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME)
                .param("itemSku", Off2OnControllerTest.ITEM_SKU)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(true));
    verify(this.off2OnService).deactivateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID,
        Off2OnControllerTest.ITEM_SKU);
  }

  @Test
  public void deactivateOff2OnChannelWithApplicationExceptionTest() throws Exception {
    when(
        this.off2OnService.deactivateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID,
            Off2OnControllerTest.ITEM_SKU)).thenThrow(
        new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION,
            Off2OnControllerTest.ERROR_MESSAGE));
    this.mockMvc
        .perform(
            post(ProductApiPath.OFF2ON_DEACTIVATE).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", Off2OnControllerTest.STORE_ID)
                .param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID)
                .param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME)
                .param("itemSku", Off2OnControllerTest.ITEM_SKU))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(false))
        .andExpect(
            jsonPath("$.errorMessage").value(containsString(Off2OnControllerTest.ERROR_MESSAGE)))
        .andExpect(
            jsonPath("$.errorCode").value(
                ProductErrorCodesEnum.CHANGE_OFF2ON_CHANNEL_ACTIVE_FAILED.getCode()));
    verify(this.off2OnService).deactivateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID,
        Off2OnControllerTest.ITEM_SKU);
  }

  @Test
  public void deactivateOff2OnChannelWithExceptionTest() throws Exception {
    when(
        this.off2OnService.deactivateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID,
            Off2OnControllerTest.ITEM_SKU)).thenThrow(
        new RuntimeException(Off2OnControllerTest.ERROR_MESSAGE));
    this.mockMvc
        .perform(
            post(ProductApiPath.OFF2ON_DEACTIVATE).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", Off2OnControllerTest.STORE_ID)
                .param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID)
                .param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME)
                .param("itemSku", Off2OnControllerTest.ITEM_SKU))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(false))
        .andExpect(jsonPath("$.errorMessage").value(Off2OnControllerTest.ERROR_MESSAGE))
        .andExpect(
            jsonPath("$.errorCode").value(
                ProductErrorCodesEnum.CHANGE_OFF2ON_CHANNEL_ACTIVE_FAILED.getCode()));
    verify(this.off2OnService).deactivateOff2OnChannelByItemSku(Off2OnControllerTest.STORE_ID,
        Off2OnControllerTest.ITEM_SKU);
  }

  @Test
  public void deactivateOff2OnChannelWithMerchantCodeApplicationExceptionTest() throws Exception {
    boolean result = false;
    when(
        this.off2OnService.deactivateOff2OnChannelByMerchantCode(Off2OnControllerTest.STORE_ID,
            Off2OnControllerTest.MERCHANT_CODE)).thenThrow(new ApplicationRuntimeException());
    this.mockMvc
        .perform(
            post(ProductApiPath.OFF2ON_DEACTIVATE_BY_MERCHANT_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", Off2OnControllerTest.STORE_ID)
                .param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID)
                .param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME)
                .param("merchantCode", Off2OnControllerTest.MERCHANT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(result));
    verify(this.off2OnService).deactivateOff2OnChannelByMerchantCode(Off2OnControllerTest.STORE_ID,
        Off2OnControllerTest.MERCHANT_CODE);
  }

  @Test
  public void deactivateOff2OnChannelWithMerchantCodeExceptionTest() throws Exception {
    boolean result = false;
    when(
        this.off2OnService.deactivateOff2OnChannelByMerchantCode(Off2OnControllerTest.STORE_ID,
            Off2OnControllerTest.MERCHANT_CODE)).thenThrow(new RuntimeException());
    this.mockMvc
        .perform(
            post(ProductApiPath.OFF2ON_DEACTIVATE_BY_MERCHANT_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", Off2OnControllerTest.STORE_ID)
                .param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID)
                .param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME)
                .param("merchantCode", Off2OnControllerTest.MERCHANT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(result));
    verify(this.off2OnService).deactivateOff2OnChannelByMerchantCode(Off2OnControllerTest.STORE_ID,
        Off2OnControllerTest.MERCHANT_CODE);
  }

  @Test
  public void deactivateOff2OnChannelWithMerchantCodeTest() throws Exception {
    boolean result = true;
    ArrayList<String> expectedResult = new ArrayList<String>();
    when(
        this.off2OnService.deactivateOff2OnChannelByMerchantCode(Off2OnControllerTest.STORE_ID,
            Off2OnControllerTest.MERCHANT_CODE)).thenReturn(expectedResult);
    this.mockMvc
        .perform(
            post(ProductApiPath.OFF2ON_DEACTIVATE_BY_MERCHANT_CODE)
                .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", Off2OnControllerTest.STORE_ID)
                .param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID)
                .param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME)
                .param("merchantCode", Off2OnControllerTest.MERCHANT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(result));
    verify(this.off2OnService).deactivateOff2OnChannelByMerchantCode(Off2OnControllerTest.STORE_ID,
        Off2OnControllerTest.MERCHANT_CODE);
  }

  @Test
  public void getPriceAndOff2OnChannelActiveByItemSkuTest() throws Exception {
    List<String> itemSkus = new ArrayList<String>();
    itemSkus.add(Off2OnControllerTest.ITEM_SKU);
    SimpleListStringRequest bodyObject = new SimpleListStringRequest(itemSkus);
    String bodyJson = new ObjectMapper().writeValueAsString(bodyObject);
    List<Off2OnPriceVO> expectedServiceResponse = new ArrayList<Off2OnPriceVO>();
    expectedServiceResponse.add(new Off2OnPriceVO(Off2OnControllerTest.ITEM_SKU, 0, 0, false));
    List<Off2OnPriceResponse> expectedControllerResponse = new ArrayList<Off2OnPriceResponse>();
    expectedControllerResponse.add(new Off2OnPriceResponse(Off2OnControllerTest.ITEM_SKU, 0, 0,
        false));

    when(
        this.off2OnService.getProductPriceForOff2On(Off2OnControllerTest.STORE_ID, itemSkus,
            Off2OnControllerTest.CHANNEL)).thenReturn(expectedServiceResponse);
    when(
        this.modelConverter.convertListToResponse(expectedServiceResponse,
            Off2OnPriceResponse.class)).thenReturn(expectedControllerResponse);

    this.mockMvc
        .perform(
            post(ProductApiPath.OFF2ON_GET_OFF2ON_PRICE).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", Off2OnControllerTest.STORE_ID)
                .param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID)
                .param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME)
                .param("channel", Off2OnControllerTest.CHANNEL).content(bodyJson))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true));

    verify(this.off2OnService).getProductPriceForOff2On(Off2OnControllerTest.STORE_ID, itemSkus,
        Off2OnControllerTest.CHANNEL);
    verify(this.modelConverter).convertListToResponse(expectedServiceResponse,
        Off2OnPriceResponse.class);
  }

  @Test
  public void getPriceAndOff2OnChannelActiveByItemSkuWithApplicationExceptionTest()
      throws Exception {
    List<String> itemSkus = new ArrayList<String>();
    itemSkus.add(Off2OnControllerTest.ITEM_SKU);
    SimpleListStringRequest bodyObject = new SimpleListStringRequest(itemSkus);
    String bodyJson = new ObjectMapper().writeValueAsString(bodyObject);

    when(
        this.off2OnService.getProductPriceForOff2On(Off2OnControllerTest.STORE_ID, itemSkus,
            Off2OnControllerTest.CHANNEL)).thenThrow(
        new ApplicationRuntimeException(ErrorCategory.AUTHORIZATION,
            Off2OnControllerTest.ERROR_MESSAGE));

    this.mockMvc
        .perform(
            post(ProductApiPath.OFF2ON_GET_OFF2ON_PRICE).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", Off2OnControllerTest.STORE_ID)
                .param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID)
                .param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME)
                .param("channel", Off2OnControllerTest.CHANNEL).content(bodyJson))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(false))
        .andExpect(
            jsonPath("$.errorMessage").value(containsString(Off2OnControllerTest.ERROR_MESSAGE)))
        .andExpect(jsonPath("$.errorCode").value(ProductErrorCodesEnum.GET_PRICE_OFF2ON.getCode()));

    verify(this.off2OnService).getProductPriceForOff2On(Off2OnControllerTest.STORE_ID, itemSkus,
        Off2OnControllerTest.CHANNEL);
  }

  @Test
  public void getPriceAndOff2OnChannelActiveByItemSkuWithExceptionTest() throws Exception {
    List<String> itemSkus = new ArrayList<String>();
    itemSkus.add(Off2OnControllerTest.ITEM_SKU);
    SimpleListStringRequest bodyObject = new SimpleListStringRequest(itemSkus);
    String bodyJson = new ObjectMapper().writeValueAsString(bodyObject);
    List<Off2OnPriceVO> expectedServiceResponse = new ArrayList<Off2OnPriceVO>();
    expectedServiceResponse.add(new Off2OnPriceVO(Off2OnControllerTest.ITEM_SKU, 0, 0, false));

    when(
        this.off2OnService.getProductPriceForOff2On(Off2OnControllerTest.STORE_ID, itemSkus,
            Off2OnControllerTest.CHANNEL)).thenReturn(expectedServiceResponse);
    when(
        this.modelConverter.convertListToResponse(expectedServiceResponse,
            Off2OnPriceResponse.class)).thenThrow(
        new RuntimeException(Off2OnControllerTest.ERROR_MESSAGE));

    this.mockMvc
        .perform(
            post(ProductApiPath.OFF2ON_GET_OFF2ON_PRICE).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON)
                .param("storeId", Off2OnControllerTest.STORE_ID)
                .param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID)
                .param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME)
                .param("channel", Off2OnControllerTest.CHANNEL).content(bodyJson))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(false))
        .andExpect(jsonPath("$.errorMessage").value(Off2OnControllerTest.ERROR_MESSAGE))
        .andExpect(jsonPath("$.errorCode").value(ProductErrorCodesEnum.GET_PRICE_OFF2ON.getCode()));

    verify(this.off2OnService).getProductPriceForOff2On(Off2OnControllerTest.STORE_ID, itemSkus,
        Off2OnControllerTest.CHANNEL);
    verify(this.modelConverter).convertListToResponse(expectedServiceResponse,
        Off2OnPriceResponse.class);
  }

  @Test
  public void getPriceAndOff2OnChannelActiveByItemSkuAndPickupPointCodeTest() throws Exception {
    List<ItemPickupPointRequest> itemPickupPointRequestList =
        Arrays.asList(new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE));

    when(off2OnService.getProductPriceForOff2OnV2(STORE_ID, itemPickupPointRequestList, CHANNEL, false)).thenReturn(
        new ArrayList<>());

    this.mockMvc.perform(
            post(ProductApiPath.OFF2ON + ProductApiPath.GET_OFF2ON_PRICE_BY_ITEM_SKU_AND_PICKUP_POINT_CODE).accept(
                    MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", Off2OnControllerTest.STORE_ID).param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID).param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME).param("channel", Off2OnControllerTest.CHANNEL)
                .content(objectMapper.writeValueAsString(itemPickupPointRequestList))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(true));

    verify(off2OnService).getProductPriceForOff2OnV2(STORE_ID, itemPickupPointRequestList, CHANNEL, false);
  }

  @Test
  public void getPriceAndOff2OnChannelActiveByItemSkuAndPickupPointCodeExceptionTest() throws Exception {
    List<ItemPickupPointRequest> itemPickupPointRequestList =
        Arrays.asList(new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE));

    when(off2OnService.getProductPriceForOff2OnV2(STORE_ID, itemPickupPointRequestList, CHANNEL, false)).thenThrow(
        ApplicationRuntimeException.class);

    this.mockMvc.perform(
            post(ProductApiPath.OFF2ON + ProductApiPath.GET_OFF2ON_PRICE_BY_ITEM_SKU_AND_PICKUP_POINT_CODE).accept(
                    MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
                .param("storeId", Off2OnControllerTest.STORE_ID).param("channelId", Off2OnControllerTest.CHANNEL_ID)
                .param("clientId", Off2OnControllerTest.CLIENT_ID).param("requestId", Off2OnControllerTest.REQUEST_ID)
                .param("username", Off2OnControllerTest.USERNAME).param("channel", Off2OnControllerTest.CHANNEL)
                .content(objectMapper.writeValueAsString(itemPickupPointRequestList))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(false));

    verify(off2OnService).getProductPriceForOff2OnV2(STORE_ID, itemPickupPointRequestList, CHANNEL, false);
  }

  @BeforeEach
  public void setUp() throws Exception {
    openMocks(this);
    this.objectMapper = new ObjectMapper();
    this.mockMvc = standaloneSetup(this.sut).build();
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.off2OnService);
    verifyNoMoreInteractions(this.modelConverter);
  }
}
