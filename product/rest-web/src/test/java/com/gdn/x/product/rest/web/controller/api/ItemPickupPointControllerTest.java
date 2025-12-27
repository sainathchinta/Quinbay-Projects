package com.gdn.x.product.rest.web.controller.api;

import static com.gdn.common.web.param.MandatoryRequestParam.generateMandatoryRequestParam;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.MockitoAnnotations.openMocks;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.List;

import com.gdn.common.exception.ApplicationException;
import com.gdn.x.product.rest.web.model.dto.PickupPointDetailsDTO;
import com.gdn.x.product.rest.web.model.request.EanUpcPickupPointCodeRequest;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponseV2;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.springframework.data.domain.PageImpl;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.enums.Constants;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.rest.web.model.EditItemResponse;
import com.gdn.x.product.rest.web.model.enums.ApiErrorCode;
import com.gdn.x.product.rest.web.model.request.AutoCreatePickupPointRequestList;
import com.gdn.x.product.rest.web.model.request.CreateFbbPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.DeleteItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointUpdateRequest;
import com.gdn.x.product.rest.web.model.request.ItemPickupPointViewConfigBaseRequest;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.request.SimpleSetStringRequest;
import com.gdn.x.product.rest.web.model.response.AutoCreatePickupPointListResponse;
import com.gdn.x.product.rest.web.model.response.CreateFbbPickupPointResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointBasicResponse;
import com.gdn.x.product.rest.web.model.response.ItemPickupPointPriceResponse;
import com.gdn.x.product.rest.web.model.response.ItemSkuAndPickupPointCodeResponse;
import com.gdn.x.product.rest.web.model.response.MinMaxItemPriceResponse;
import com.gdn.x.product.rest.web.model.response.PriceUpdatedInTimeRangeL5Response;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponse;
import com.gdn.x.product.rest.web.model.util.ItemPickupPointApiPath;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ItemPickupPointWrapperService;
import com.gdn.x.product.service.api.ItemService;
import com.gdn.x.product.service.util.ModelConverter;

public class ItemPickupPointControllerTest {

  private static final String STORE_ID = "storeId";
  private static final String USERNAME = "username";
  private static final String REQUEST_ID = "requestId";
  private static final String CHANNEL_ID = "channelId";
  private static final String CLIENT_ID = "clientId";
  private static final String ITEM_SKU = "itemSku";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String PRODUCT_SKU = "productSku";
  private static final String EAN_UPC = "eanUpc";
  private ItemPickupPointViewConfigBaseRequest itemPickupPointViewConfigBaseRequest;
  private static final double OFFER_PRICE = 11;
  private static final double NORMAL_PRICE = 111;
  private static final String ITEM_CODE = "itemCode";
  private static final String PRISTINE_ID = "pristineId";
  private static final Long COUNT = 1L;
  private static final String BP_CODE = "bpCode";
  private static final Date DATE = new Date();
  private static final int PAGE = 0;
  private static final int SIZE = 10;

  @InjectMocks
  private ItemPickupPointController itemPickupPointController;

  @Mock
  private ItemPickupPointWrapperService itemPickupPointWrapperService;

  @Mock
  private ItemService itemService;

  @Mock
  private ModelConverter modelConverter;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  private MockMvc mockMvc;
  private ObjectMapper objectMapper;
  private ItemPickupPointRequest itemPickupPointRequest;
  private ItemPickupPointUpdateRequest itemPickupPointUpdateRequest = new ItemPickupPointUpdateRequest();
  private ProductSkuPickupPointResponse productSkuPickupPointResponse =
    new ProductSkuPickupPointResponse();
  private ProductSkuPickupPointResponseV2 productSkuPickupPointResponseV2 =
    new ProductSkuPickupPointResponseV2();
  private ItemSkuAndPickupPointCodeResponse itemSkuAndPickupPointCodeResponse = new ItemSkuAndPickupPointCodeResponse();
  private List<ItemSkuAndPickupPointCodeResponse> itemSkuAndPickupPointCodeResponseList = new ArrayList<>();
  private List<PriceUpdatedInTimeRangeL5Response> priceUpdatedInTimeRangeL5ResponseList = new ArrayList<>();
  private PriceUpdatedInTimeRangeL5Response priceUpdatedInTimeRangeL5Response;
  private SimpleDateFormat format = new SimpleDateFormat(Constants.DATE_TIME_FORMAT);

  @BeforeEach
  public void setUp() {
    openMocks(this);
    this.mockMvc = standaloneSetup(this.itemPickupPointController).build();
    objectMapper = new ObjectMapper();
    itemPickupPointRequest = new ItemPickupPointRequest(ITEM_SKU, PICKUP_POINT_CODE);
    itemPickupPointViewConfigBaseRequest = new ItemPickupPointViewConfigBaseRequest(false, false, false);
    itemSkuAndPickupPointCodeResponseList.add(itemSkuAndPickupPointCodeResponse);

    priceUpdatedInTimeRangeL5Response =
        PriceUpdatedInTimeRangeL5Response.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE)
            .priceUpdatedDate(DATE).build();
    priceUpdatedInTimeRangeL5ResponseList.add(priceUpdatedInTimeRangeL5Response);
  }

  @AfterEach
  public void tearDown() {
    Mockito.verifyNoMoreInteractions(itemPickupPointWrapperService, itemService, modelConverter);
  }

  @Test
  public void findProductAndItemByItemSkuAndPickupPointCodeTest() throws Exception {
    Mockito.when(itemPickupPointWrapperService.findProductForTransactionByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID,
        USERNAME, Arrays.asList(itemPickupPointRequest))).thenReturn(new ArrayList<>());
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.GET_PRODUCT_AND_ITEM_BY_ITEM_SKU_AND_PICKUP_POINT_CODE).contentType(
                MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(Arrays.asList(itemPickupPointRequest)))
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(itemPickupPointWrapperService)
        .findProductForTransactionByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME,
            Arrays.asList(itemPickupPointRequest));
  }

  @Test
  public void findProductAndItemByItemSkuAndPickupPointCodeExceptionTest() throws Exception {
    Mockito.when(itemPickupPointWrapperService.findProductForTransactionByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID,
        USERNAME, Arrays.asList(itemPickupPointRequest))).thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH +ItemPickupPointApiPath.GET_PRODUCT_AND_ITEM_BY_ITEM_SKU_AND_PICKUP_POINT_CODE).contentType(
                MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(Arrays.asList(itemPickupPointRequest)))
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(itemPickupPointWrapperService)
        .findProductForTransactionByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME,
            Arrays.asList(itemPickupPointRequest));
  }

  @Test
  public void findItemSummaryByItemSkuAndPickupPointCodeTest() throws Exception {
    Mockito.when(itemPickupPointWrapperService.fetchItemSummaryByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID,
        USERNAME, false, Arrays.asList(itemPickupPointRequest), null, CLIENT_ID, Constants.ALL, false)).thenReturn(new ArrayList<>());
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH +ItemPickupPointApiPath.GET_ITEM_SUMMARY_BY_ITEM_SKU_AND_PICKUP_POINT_CODE).contentType(
                MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(Arrays.asList(itemPickupPointRequest)))
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(itemPickupPointWrapperService)
        .fetchItemSummaryByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME, false,
            Arrays.asList(itemPickupPointRequest), null, CLIENT_ID, Constants.ALL, false);
  }

  @Test
  public void findItemSummaryByItemSkuAndPickupPointCodeExceptionTest() throws Exception {
    Mockito.when(itemPickupPointWrapperService.fetchItemSummaryByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID,
        USERNAME, false, Arrays.asList(itemPickupPointRequest), null, CLIENT_ID, Constants.ALL, false)).thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH +ItemPickupPointApiPath.GET_ITEM_SUMMARY_BY_ITEM_SKU_AND_PICKUP_POINT_CODE).contentType(
                MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(Arrays.asList(itemPickupPointRequest)))
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(itemPickupPointWrapperService)
        .fetchItemSummaryByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME, false,
            Arrays.asList(itemPickupPointRequest), null, CLIENT_ID, Constants.ALL, false);
  }


  @Test
  public void findItemSummaryByItemSkuAndPickupPointCodeGenericExceptionTest() throws Exception {
    Mockito.when(
      itemPickupPointWrapperService.fetchItemSummaryByItemSkusAndPickupPointCode(STORE_ID,
        REQUEST_ID, USERNAME, false, Arrays.asList(itemPickupPointRequest), null, CLIENT_ID,
        Constants.ALL, false)).thenThrow(ApplicationException.class);
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH
        + ItemPickupPointApiPath.GET_ITEM_SUMMARY_BY_ITEM_SKU_AND_PICKUP_POINT_CODE).contentType(
          MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(Arrays.asList(itemPickupPointRequest)))
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(itemPickupPointWrapperService)
      .fetchItemSummaryByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME, false,
        Arrays.asList(itemPickupPointRequest), null, CLIENT_ID, Constants.ALL, false);
  }

  @Test
  public void findItemSummaryByItemSkuAndPickupPointCodeApiIncorrectinputDataExceptionTest() throws Exception {
    Mockito.when(itemPickupPointWrapperService.fetchItemSummaryByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID,
        USERNAME, false, Arrays.asList(itemPickupPointRequest), null, CLIENT_ID, Constants.ALL, false)).thenThrow(ApiIncorrectInputDataException.class);
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH +ItemPickupPointApiPath.GET_ITEM_SUMMARY_BY_ITEM_SKU_AND_PICKUP_POINT_CODE).contentType(
                MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(Arrays.asList(itemPickupPointRequest)))
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(itemPickupPointWrapperService)
        .fetchItemSummaryByItemSkusAndPickupPointCode(STORE_ID, REQUEST_ID, USERNAME, false,
            Arrays.asList(itemPickupPointRequest), null, CLIENT_ID, Constants.ALL, false);
  }

  @Test
  public void updateItemPickupPointViewConfigWithProductStatusTest() throws Exception {
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH
            + ItemPickupPointApiPath.UPDATE_VIEW_CONFIG_WITH_PRODUCT_STATUS_IN_ITEMPICKUPPOINT, PRODUCT_SKU).contentType(
                MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(itemPickupPointViewConfigBaseRequest))
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(itemPickupPointWrapperService)
        .updateItemPickupPointViewConfigWithProductStatus(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU,
            itemPickupPointViewConfigBaseRequest);
  }

  @Test
  public void updateItemPickupPointViewConfigWithProductStatusExceptionTest() throws Exception {
    doThrow(new ApplicationRuntimeException()).when(itemPickupPointWrapperService)
        .updateItemPickupPointViewConfigWithProductStatus(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU,
            itemPickupPointViewConfigBaseRequest);
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH
            + ItemPickupPointApiPath.UPDATE_VIEW_CONFIG_WITH_PRODUCT_STATUS_IN_ITEMPICKUPPOINT, PRODUCT_SKU).contentType(
                MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(itemPickupPointViewConfigBaseRequest))
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(itemPickupPointWrapperService)
        .updateItemPickupPointViewConfigWithProductStatus(STORE_ID, USERNAME, REQUEST_ID, PRODUCT_SKU,
            itemPickupPointViewConfigBaseRequest);
  }

  @Test
  public void getItemSkusByItemCodeAndPickupPointCodeTest() throws Exception {
    when(itemService.getAllItemSkuByItemCodeAndPickupPointCode(eq(STORE_ID), eq(ITEM_CODE), eq(PICKUP_POINT_CODE),
        eq(false))).thenReturn(new ArrayList<>());
    this.mockMvc.perform(
            get(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.GET_ITEM_SKUS_BY_ITEM_CODE_AND_PICKUPPOINT_CODE,
                ITEM_CODE, PICKUP_POINT_CODE).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
                .param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemService, times(1)).getAllItemSkuByItemCodeAndPickupPointCode(eq(STORE_ID), eq(ITEM_CODE),
        eq(PICKUP_POINT_CODE), eq(false));
  }

  @Test
  public void getItemSkusByItemCodeAndPickupPointCodeExceptionTest() throws Exception {
    when(itemService.getAllItemSkuByItemCodeAndPickupPointCode(eq(STORE_ID), eq(ITEM_CODE), eq(PICKUP_POINT_CODE),
        eq(false))).thenThrow(new RuntimeException());
    this.mockMvc.perform(
            get(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.GET_ITEM_SKUS_BY_ITEM_CODE_AND_PICKUPPOINT_CODE,
                ITEM_CODE, PICKUP_POINT_CODE).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
                .param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemService, times(1)).getAllItemSkuByItemCodeAndPickupPointCode(eq(STORE_ID), eq(ITEM_CODE),
        eq(PICKUP_POINT_CODE), eq(false));
  }

  @Test
  public void getItemSkuAndPickupPointCreatedInTimeRangeTest() throws Exception {
    String date = format.format(DATE);
    GdnRestListResponse gdnRestListResponse =
        new GdnRestListResponse(null, null, true, itemSkuAndPickupPointCodeResponseList, null, REQUEST_ID);
    when(itemPickupPointService.getL5sCreatedInTimeRange(STORE_ID, format.parse(date), format.parse(date), PAGE, SIZE,
        REQUEST_ID)).thenReturn(gdnRestListResponse);
    this.mockMvc.perform(
            get(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.GET_L5S_CREATED_IN_TIME_RANGE).contentType(
                    MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
                .param("startDate", format.format(DATE)).param("endDate", format.format(DATE)).param("page", "0")
                .param("size", "10")).andExpect(status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(true)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointService).getL5sCreatedInTimeRange(STORE_ID, format.parse(date), format.parse(date), PAGE,
        SIZE, REQUEST_ID);
  }

  @Test
  public void getItemSkuAndPickupPointCreatedInTimeRangeApplicationRuntimeExceptionTest() throws Exception {
    String date = format.format(DATE);
    when(itemPickupPointService.getL5sCreatedInTimeRange(STORE_ID, format.parse(date), format.parse(date), PAGE,
        SIZE, REQUEST_ID)).thenThrow(new ApplicationRuntimeException());
    this.mockMvc.perform(
            get(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.GET_L5S_CREATED_IN_TIME_RANGE).contentType(
                    MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
                .param("startDate", format.format(DATE)).param("endDate", format.format(DATE)).param("page", "0")
                .param("size", "10")).andExpect(status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointService).getL5sCreatedInTimeRange(STORE_ID, format.parse(date), format.parse(date), PAGE,
        SIZE, REQUEST_ID);
  }

  @Test
  public void getItemSkuAndPickupPointCreatedInTimeRangeExceptionTest() throws Exception {
    String date = format.format(DATE);
    when(itemPickupPointService.getL5sCreatedInTimeRange(STORE_ID, format.parse(date), format.parse(date), PAGE,
        SIZE, REQUEST_ID)).thenThrow(new NullPointerException());
    this.mockMvc.perform(
            get(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.GET_L5S_CREATED_IN_TIME_RANGE).contentType(
                    MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
                .param("startDate", format.format(DATE)).param("endDate", format.format(DATE)).param("page", "0")
                .param("size", "10")).andExpect(status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointService).getL5sCreatedInTimeRange(STORE_ID, format.parse(date), format.parse(date), PAGE,
        SIZE, REQUEST_ID);
  }

  @Test
  public void getL5sPriceUpdatedInTimeRangeTest() throws Exception {
    String date = format.format(DATE);
    GdnRestListResponse gdnRestListResponse =
        new GdnRestListResponse(null, null, true, priceUpdatedInTimeRangeL5ResponseList, null, REQUEST_ID);
    when(itemPickupPointService.getL5sPriceUpdatedInTimeRange(STORE_ID, format.parse(date), format.parse(date), PAGE,
        SIZE, REQUEST_ID)).thenReturn(gdnRestListResponse);
    this.mockMvc.perform(
            get(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.GET_L5S_PRICE_UPDATED_IN_TIME_RANGE).contentType(
                    MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
                .param("startDate", format.format(DATE)).param("endDate", format.format(DATE)).param("page", "0")
                .param("size", "10")).andExpect(status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(true)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointService).getL5sPriceUpdatedInTimeRange(STORE_ID, format.parse(date), format.parse(date), PAGE,
        SIZE, REQUEST_ID);
  }

  @Test
  public void getL5sPriceUpdatedInTimeRangeApplicationRuntimeExceptionTest() throws Exception {
    String date = format.format(DATE);
    when(itemPickupPointService.getL5sPriceUpdatedInTimeRange(STORE_ID, format.parse(date), format.parse(date), PAGE,
        SIZE, REQUEST_ID)).thenThrow(new ApplicationRuntimeException());
    this.mockMvc.perform(
            get(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.GET_L5S_PRICE_UPDATED_IN_TIME_RANGE).contentType(
                    MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
                .param("startDate", format.format(DATE)).param("endDate", format.format(DATE)).param("page", "0")
                .param("size", "10")).andExpect(status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointService).getL5sPriceUpdatedInTimeRange(STORE_ID, format.parse(date), format.parse(date), PAGE,
        SIZE, REQUEST_ID);
  }

  @Test
  public void getL5sPriceUpdatedInTimeRangeExceptionTest() throws Exception {
    String date = format.format(DATE);
    when(itemPickupPointService.getL5sPriceUpdatedInTimeRange(STORE_ID, format.parse(date), format.parse(date), PAGE,
        SIZE, REQUEST_ID)).thenThrow(new NullPointerException());
    this.mockMvc.perform(
            get(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.GET_L5S_PRICE_UPDATED_IN_TIME_RANGE).contentType(
                    MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
                .param("startDate", format.format(DATE)).param("endDate", format.format(DATE)).param("page", "0")
                .param("size", "10")).andExpect(status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointService).getL5sPriceUpdatedInTimeRange(STORE_ID, format.parse(date), format.parse(date), PAGE,
        SIZE, REQUEST_ID);
  }

  @Test
  public void getItemSkusByPristineIdAndPickupPointCodeTest() throws Exception {
    when(itemService.getAllItemSkuByPristineIdAndPickupPointCode(eq(STORE_ID), eq(PRISTINE_ID),
        eq(PICKUP_POINT_CODE))).thenReturn(new ArrayList<>());
    this.mockMvc.perform(
            get(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.GET_ITEM_SKUS_BY_PRISTINE_ID_AND_PICKUPPOINT_CODE,
                PRISTINE_ID, PICKUP_POINT_CODE).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
                .param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemService, times(1)).getAllItemSkuByPristineIdAndPickupPointCode(eq(STORE_ID), eq(PRISTINE_ID),
        eq(PICKUP_POINT_CODE));
  }

  @Test
  public void getItemSkusByPristineIdAndPickupPointCodeExceptionTest() throws Exception {
    when(itemService.getAllItemSkuByPristineIdAndPickupPointCode(eq(STORE_ID), eq(PRISTINE_ID),
        eq(PICKUP_POINT_CODE))).thenThrow(new RuntimeException());
    this.mockMvc.perform(
            get(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.GET_ITEM_SKUS_BY_PRISTINE_ID_AND_PICKUPPOINT_CODE,
                PRISTINE_ID, PICKUP_POINT_CODE).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
                .param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemService, times(1)).getAllItemSkuByPristineIdAndPickupPointCode(eq(STORE_ID), eq(PRISTINE_ID),
        eq(PICKUP_POINT_CODE));
  }

  @Test
  public void updateItemPickupPointsTest() throws Exception {
    Mockito.when(itemPickupPointWrapperService.updateItemPickupPoint(
        generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, USERNAME), null, null)).thenReturn(new EditItemResponse());
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.UPDATE_ITEM_PICKUP_POINTS)
        .contentType(MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(itemPickupPointUpdateRequest))
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    verify(modelConverter).covertToItemPickupPointUpdateRequestVo(itemPickupPointUpdateRequest);
    verify(itemPickupPointWrapperService).updateItemPickupPoint(
        generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, USERNAME), null, null);
  }

  @Test
  public void updateItemPickupPointsExceptionTest() throws Exception {
    EditItemResponse editItemResponse = new EditItemResponse();
    editItemResponse.setApiErrorCode(ApiErrorCode.L5_NOT_PRESENT);
    Mockito.when(itemPickupPointWrapperService.updateItemPickupPoint(
        generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, USERNAME), null, null)).thenReturn(editItemResponse);
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.UPDATE_ITEM_PICKUP_POINTS)
        .contentType(MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(itemPickupPointUpdateRequest))
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(modelConverter).covertToItemPickupPointUpdateRequestVo(itemPickupPointUpdateRequest);
    verify(itemPickupPointWrapperService).updateItemPickupPoint(
        generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, USERNAME), null, null);
  }

  @Test
  public void updateItemPickupPointsException1Test() throws Exception {
    EditItemResponse editItemResponse = new EditItemResponse();
    editItemResponse.setApiErrorCode(ApiErrorCode.L5_NOT_PRESENT);
    doThrow(Exception.class).when(itemPickupPointWrapperService).updateItemPickupPoint(
        generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, USERNAME), null, null);
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.UPDATE_ITEM_PICKUP_POINTS)
        .contentType(MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(itemPickupPointUpdateRequest))
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(modelConverter).covertToItemPickupPointUpdateRequestVo(itemPickupPointUpdateRequest);
    verify(itemPickupPointWrapperService).updateItemPickupPoint(
        generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, USERNAME), null, null);
  }

  @Test
  public void updateItemPickupPointsExceptionApplicationRuntimeExceptionTest() throws Exception {
    EditItemResponse editItemResponse = new EditItemResponse();
    editItemResponse.setApiErrorCode(ApiErrorCode.L5_NOT_PRESENT);
    when(itemPickupPointWrapperService.updateItemPickupPoint(
        generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, USERNAME), null, null)).thenThrow(
        new ApplicationRuntimeException(ErrorCategory.VALIDATION, ErrorMessages.PRICE_CANNOT_BE_EDITED_FOR_ITEM));
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.UPDATE_ITEM_PICKUP_POINTS)
        .contentType(MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(itemPickupPointUpdateRequest))
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    verify(modelConverter).covertToItemPickupPointUpdateRequestVo(itemPickupPointUpdateRequest);
    verify(itemPickupPointWrapperService).updateItemPickupPoint(
        generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME, USERNAME), null, null);
  }

  @Test
  public void getL5CountByItemSkuTest() throws Exception {
    when(itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU)).thenReturn(COUNT);
    this.mockMvc.perform(
        get(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.GET_L5_COUNT_BY_ITEM_SKU,
            ITEM_SKU).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointService, times(1)).findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(
        STORE_ID, ITEM_SKU);
  }

  @Test
  public void getL5CountByItemSkuExceptionTest() throws Exception {
    when(itemPickupPointService.findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(STORE_ID, ITEM_SKU))
        .thenThrow(new RuntimeException());
    this.mockMvc.perform(
        get(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.GET_L5_COUNT_BY_ITEM_SKU,
            ITEM_SKU).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointService, times(1)).findCountByStoreIdAndItemSkuAndMarkForDeleteFalse(
        STORE_ID, ITEM_SKU);
  }

  @Test
  public void autoCreatePickupPointTest() throws Exception {
    AutoCreatePickupPointRequestList autoCreatePickupPointRequestList = new AutoCreatePickupPointRequestList();
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
            USERNAME);
    when(itemPickupPointWrapperService.autoCreatePickupPoint(autoCreatePickupPointRequestList,
        mandatoryRequestParam)).thenReturn(new AutoCreatePickupPointListResponse());
    this.mockMvc.perform(post(
            ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.AUTO_CREATE_L5_BY_ITEM_SKU_AND_PP_CODE).contentType(
                MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(autoCreatePickupPointRequestList))
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointWrapperService).autoCreatePickupPoint(autoCreatePickupPointRequestList,
        mandatoryRequestParam);
  }

  @Test
  public void autoCreatePickupPointApplicationRuntimeExceptionTest() throws Exception {
    AutoCreatePickupPointRequestList autoCreatePickupPointRequestList = new AutoCreatePickupPointRequestList();
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
            USERNAME);
    when(itemPickupPointWrapperService.autoCreatePickupPoint(autoCreatePickupPointRequestList,
        mandatoryRequestParam)).thenThrow(ApplicationRuntimeException.class);
    try {
      this.mockMvc.perform(post(
              ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.AUTO_CREATE_L5_BY_ITEM_SKU_AND_PP_CODE).contentType(
                  MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(autoCreatePickupPointRequestList))
              .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
              .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
          .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    } finally {
      verify(itemPickupPointWrapperService).autoCreatePickupPoint(autoCreatePickupPointRequestList,
          mandatoryRequestParam);
    }
  }

  @Test
  public void autoCreatePickupPointExceptionTest() throws Exception {
    AutoCreatePickupPointRequestList autoCreatePickupPointRequestList = new AutoCreatePickupPointRequestList();
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, CHANNEL_ID, CLIENT_ID, REQUEST_ID, USERNAME,
            USERNAME);
    when(itemPickupPointWrapperService.autoCreatePickupPoint(autoCreatePickupPointRequestList,
        mandatoryRequestParam)).thenThrow(ApplicationRuntimeException.class);
    try {
      this.mockMvc.perform(post(
              ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.AUTO_CREATE_L5_BY_ITEM_SKU_AND_PP_CODE).contentType(
                  MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(autoCreatePickupPointRequestList))
              .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
              .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
          .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
          .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    } finally {
      verify(itemPickupPointWrapperService).autoCreatePickupPoint(autoCreatePickupPointRequestList,
          mandatoryRequestParam);
    }
  }

  @Test
  public void createFbbPickupPointTest() throws Exception {
    CreateFbbPickupPointRequest createFbbPickupPointRequest = new CreateFbbPickupPointRequest();
    createFbbPickupPointRequest.setItemSku(ITEM_SKU);
    createFbbPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    createFbbPickupPointRequest.setBusinessPartnerCode(BP_CODE);
    when(itemPickupPointWrapperService.createFbbPickupPoint(STORE_ID, createFbbPickupPointRequest)).thenReturn(
        new CreateFbbPickupPointResponse());
    this.mockMvc.perform(
            post(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.CREATE_FBB_PICKUP_POINT).contentType(
                    MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(createFbbPickupPointRequest))
                .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointWrapperService).createFbbPickupPoint(STORE_ID, createFbbPickupPointRequest);
  }

  @Test
  public void createFbbPickupPointApplicationExceptionTest() throws Exception {
    CreateFbbPickupPointRequest createFbbPickupPointRequest = new CreateFbbPickupPointRequest();
    createFbbPickupPointRequest.setItemSku(ITEM_SKU);
    createFbbPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    createFbbPickupPointRequest.setBusinessPartnerCode(BP_CODE);
    when(itemPickupPointWrapperService.createFbbPickupPoint(STORE_ID, createFbbPickupPointRequest)).thenThrow(
        ApplicationRuntimeException.class);
    this.mockMvc.perform(
            post(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.CREATE_FBB_PICKUP_POINT).contentType(
                    MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(createFbbPickupPointRequest))
                .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointWrapperService).createFbbPickupPoint(STORE_ID, createFbbPickupPointRequest);
  }

  @Test
  public void createFbbPickupPointExceptionTest() throws Exception {
    CreateFbbPickupPointRequest createFbbPickupPointRequest = new CreateFbbPickupPointRequest();
    createFbbPickupPointRequest.setItemSku(ITEM_SKU);
    createFbbPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    createFbbPickupPointRequest.setBusinessPartnerCode(BP_CODE);
    when(itemPickupPointWrapperService.createFbbPickupPoint(STORE_ID, createFbbPickupPointRequest)).thenThrow(
        ApplicationRuntimeException.class);
    this.mockMvc.perform(
            post(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.CREATE_FBB_PICKUP_POINT).contentType(
                    MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(createFbbPickupPointRequest))
                .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointWrapperService).createFbbPickupPoint(STORE_ID, createFbbPickupPointRequest);
  }

  @Test
  public void deleteItemPickupPointByPickupPointCodeTest() throws Exception {
    DeleteItemPickupPointRequest deleteItemPickupPointRequest = new DeleteItemPickupPointRequest();
    deleteItemPickupPointRequest.setProductSku(PRODUCT_SKU);
    deleteItemPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    deleteItemPickupPointRequest.setBusinessPartnerCode(BP_CODE);
    when(itemPickupPointWrapperService.deleteItemPickupPointsByPickupPointCode(STORE_ID,deleteItemPickupPointRequest))
        .thenReturn(new ArrayList<>());
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH
                + ItemPickupPointApiPath.DELETE_ITEM_PICKUP_POINT_BY_PICKUPPOINT_CODE).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(deleteItemPickupPointRequest))
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointWrapperService).deleteItemPickupPointsByPickupPointCode(STORE_ID,
        deleteItemPickupPointRequest);
  }

  @Test
  public void deleteItemPickupPointByPickupPointCodeForApplicationExceptionTest()
      throws Exception {
    DeleteItemPickupPointRequest deleteItemPickupPointRequest = new DeleteItemPickupPointRequest();
    deleteItemPickupPointRequest.setProductSku(PRODUCT_SKU);
    deleteItemPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    deleteItemPickupPointRequest.setBusinessPartnerCode(BP_CODE);
    doThrow(ApplicationRuntimeException.class).when(itemPickupPointWrapperService)
        .deleteItemPickupPointsByPickupPointCode(STORE_ID, deleteItemPickupPointRequest);
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH
            + ItemPickupPointApiPath.DELETE_ITEM_PICKUP_POINT_BY_PICKUPPOINT_CODE).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(deleteItemPickupPointRequest))
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointWrapperService).deleteItemPickupPointsByPickupPointCode(STORE_ID,
        deleteItemPickupPointRequest);
  }

  @Test
  public void deleteItemPickupPointByPickupPointCodeForExceptionTest()
      throws Exception {
    DeleteItemPickupPointRequest deleteItemPickupPointRequest = new DeleteItemPickupPointRequest();
    deleteItemPickupPointRequest.setProductSku(PRODUCT_SKU);
    deleteItemPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    deleteItemPickupPointRequest.setBusinessPartnerCode(BP_CODE);
    doThrow(Exception.class).when(itemPickupPointWrapperService)
        .deleteItemPickupPointsByPickupPointCode(STORE_ID, deleteItemPickupPointRequest);
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH
            + ItemPickupPointApiPath.DELETE_ITEM_PICKUP_POINT_BY_PICKUPPOINT_CODE).contentType(MediaType.APPLICATION_JSON)
            .content(objectMapper.writeValueAsString(deleteItemPickupPointRequest))
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointWrapperService).deleteItemPickupPointsByPickupPointCode(STORE_ID,
        deleteItemPickupPointRequest);
  }
  @Test
  public void testFindPriceDetailsByItemSkuAndPickupPointCode() throws Exception {
    when(itemPickupPointWrapperService.findPriceDetailsByItemSkuAndPickupPointCode(STORE_ID,
      Collections.singletonList(itemPickupPointRequest)))
      .thenReturn(Collections.singletonList(new ItemPickupPointPriceResponse()));
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH
          + ItemPickupPointApiPath.GET_PRICE_DETAIL_BY_ITEM_SKU_AND_PICKUP_POINT_CODE,
        ITEM_SKU).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(Collections.singletonList(itemPickupPointRequest)))
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", Matchers.equalTo(true)))
      .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointWrapperService).findPriceDetailsByItemSkuAndPickupPointCode(STORE_ID,
      Collections.singletonList(itemPickupPointRequest));
  }

  @Test
  public void testFindPriceDetailsByItemSkuAndPickupPointCodeForApplicationException()
    throws Exception {
    doThrow(ApplicationRuntimeException.class).when(itemPickupPointWrapperService)
      .findPriceDetailsByItemSkuAndPickupPointCode(STORE_ID,
        Collections.singletonList(itemPickupPointRequest));
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH
          + ItemPickupPointApiPath.GET_PRICE_DETAIL_BY_ITEM_SKU_AND_PICKUP_POINT_CODE,
        ITEM_SKU).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(Collections.singletonList(itemPickupPointRequest)))
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
      .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointWrapperService).findPriceDetailsByItemSkuAndPickupPointCode(STORE_ID,
      Collections.singletonList(itemPickupPointRequest));
  }

  @Test
  public void testFindPriceDetailsByItemSkuAndPickupPointCodeForException()
    throws Exception {
    doThrow(ApplicationRuntimeException.class).when(itemPickupPointWrapperService)
      .findPriceDetailsByItemSkuAndPickupPointCode(STORE_ID,
        Collections.singletonList(itemPickupPointRequest));
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH
          + ItemPickupPointApiPath.GET_PRICE_DETAIL_BY_ITEM_SKU_AND_PICKUP_POINT_CODE,
        ITEM_SKU).contentType(MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(Collections.singletonList(itemPickupPointRequest)))
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
      .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointWrapperService).findPriceDetailsByItemSkuAndPickupPointCode(STORE_ID,
      Collections.singletonList(itemPickupPointRequest));
  }

  @Test
  public void testFindProductSkuListByBusinessPartnerAndPickupPointCode() throws Exception {
    when(itemPickupPointService.getProductSkuListByBusinessPartnerAndPickupPointCode(STORE_ID,
      BP_CODE,PICKUP_POINT_CODE,0,1))
      .thenReturn(new PageImpl<>(new ArrayList<>(Collections.singletonList(productSkuPickupPointResponseV2))));
    this.mockMvc.perform(get(ItemPickupPointApiPath.BASE_PATH
      + ItemPickupPointApiPath.GET_PRODUCT_SKU_LIST_BY_BP_AND_PP_CODE)
      .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
      .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
      .param("page", "0").param("size", "1").param("businessPartnerCode", BP_CODE).param(
        "pickupPointCode", PICKUP_POINT_CODE)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", Matchers.equalTo(true)))
      .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointService).getProductSkuListByBusinessPartnerAndPickupPointCode(STORE_ID,
      BP_CODE,PICKUP_POINT_CODE,0,1);
  }

  @Test
  public void testFindProductSkuListByBusinessPartnerAndPickupPointCodeException() throws Exception {
    when(itemPickupPointService.getProductSkuListByBusinessPartnerAndPickupPointCode(STORE_ID,
      BP_CODE,PICKUP_POINT_CODE,0,1))
      .thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(get(ItemPickupPointApiPath.BASE_PATH
      + ItemPickupPointApiPath.GET_PRODUCT_SKU_LIST_BY_BP_AND_PP_CODE)
      .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
      .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
      .param("page", "0").param("size", "1").param("businessPartnerCode", BP_CODE).param(
        "pickupPointCode", PICKUP_POINT_CODE)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
      .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointService).getProductSkuListByBusinessPartnerAndPickupPointCode(STORE_ID,
      BP_CODE,PICKUP_POINT_CODE,0,1);
  }

  @Test
  public void getMinAndMaxOfferPriceTest() throws Exception {
    when(itemPickupPointService.getMinAndMaxOfferPrice(Constants.DEFAULT_STORE_ID,
      Constants.PRODUCT_CODE)).thenReturn(
      MinMaxItemPriceResponse.builder().productSku(PRODUCT_SKU).maxPrice(OFFER_PRICE)
        .minPrice(OFFER_PRICE).build());
    this.mockMvc.perform(get(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.GET_MIN_AND_MAX_OFFER_PRICE,
            Constants.PRODUCT_CODE).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("productCode", Constants.PRODUCT_CODE).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointService).getMinAndMaxOfferPrice(Constants.STORE_ID,
      Constants.PRODUCT_CODE);
  }

  @Test
  public void getMinAndMaxOfferPriceExceptionTest() throws Exception {
    when(itemPickupPointService.getMinAndMaxOfferPrice(Constants.DEFAULT_STORE_ID,
      Constants.PRODUCT_CODE)).thenReturn(
      MinMaxItemPriceResponse.builder().productSku(PRODUCT_SKU).maxPrice(OFFER_PRICE)
        .minPrice(OFFER_PRICE).build());
    doThrow(ApplicationRuntimeException.class).when(itemPickupPointService).getMinAndMaxOfferPrice(anyString(),
      anyString());
    this.mockMvc.perform(get(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.GET_MIN_AND_MAX_OFFER_PRICE,
            Constants.PRODUCT_CODE).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("productCode", Constants.PRODUCT_CODE).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointService).getMinAndMaxOfferPrice(Constants.STORE_ID,
      Constants.PRODUCT_CODE);
  }

  @Test
  public void findProductL5DetailItemSkuAndPickupPointCodeTest() throws Exception {
    Mockito.when(
      itemPickupPointWrapperService.findProductL5DetailByItemSkusAndPickupPointCode(STORE_ID,
        Collections.singletonList(itemPickupPointRequest), false, null)).thenReturn(new ArrayList<>());
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH
        + ItemPickupPointApiPath.GET_PRODUCT_L5_DETAIL_BY_ITEM_SKU_AND_PICKUP_POINT_CODE).contentType(
          MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(Arrays.asList(itemPickupPointRequest)))
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(itemPickupPointWrapperService)
      .findProductL5DetailByItemSkusAndPickupPointCode(STORE_ID,
        Collections.singletonList(itemPickupPointRequest), false, null);
  }

  @Test
  public void findProductL5DetailItemSkuAndPickupPointCodeTestExceptionTest() throws Exception {
    Mockito.when(
        itemPickupPointWrapperService.findProductL5DetailByItemSkusAndPickupPointCode(STORE_ID,
          Collections.singletonList(itemPickupPointRequest), false, null))
      .thenThrow(ApplicationRuntimeException.class);
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH
        + ItemPickupPointApiPath.GET_PRODUCT_L5_DETAIL_BY_ITEM_SKU_AND_PICKUP_POINT_CODE).contentType(
          MediaType.APPLICATION_JSON)
        .content(objectMapper.writeValueAsString(Arrays.asList(itemPickupPointRequest)))
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
      .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(itemPickupPointWrapperService)
      .findProductL5DetailByItemSkusAndPickupPointCode(STORE_ID,
        Collections.singletonList(itemPickupPointRequest), false, null);
  }

  @Test
  public void findFbbTrueAndOnlineItemPickupPointsTest() throws Exception {
    when(itemPickupPointWrapperService.findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID,
        new ArrayList<>())).thenReturn(new ArrayList<>());
    this.mockMvc.perform(post(
            ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.FIND_FBB_TRUE_ONLINE_L5_BY_ITEM_SKUS).contentType(
                MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(new SimpleSetStringRequest()))
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointWrapperService).findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID, new ArrayList<>());
  }

  @Test
  public void findFbbTrueAndOnlineItemPickupPointsExceptionTest() throws Exception {
    doThrow(ApplicationRuntimeException.class).when(itemPickupPointWrapperService)
        .findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID, new ArrayList<>());
    this.mockMvc.perform(post(
            ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.FIND_FBB_TRUE_ONLINE_L5_BY_ITEM_SKUS).contentType(
                MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(new SimpleSetStringRequest()))
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointWrapperService).findFbbTrueOnlinePickupPointsAndItemSkusIn(STORE_ID, new ArrayList<>());
  }

  @Test
  public void findL5sByItemSkuListTest() throws Exception {
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
    simpleListStringRequest.setValue(Collections.singletonList(ITEM_SKU));
    when(
        itemPickupPointWrapperService.findItemPickupPointsByItemSkus(STORE_ID, simpleListStringRequest, PAGE,
            SIZE, null)).thenReturn(new PageImpl<>(new ArrayList<>()));
    this.mockMvc.perform(
            post(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.FIND_BY_ITEM_SKU_LIST).contentType(
                    MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(simpleListStringRequest))
                .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME).param("size", String.valueOf(SIZE))
                .param("page", String.valueOf(PAGE))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointWrapperService).findItemPickupPointsByItemSkus(STORE_ID, simpleListStringRequest,
        PAGE, SIZE, null);
  }

  @Test
  public void findL5sByItemSkuListApplicationRuntimeExceptionTest() throws Exception {
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
    simpleListStringRequest.setValue(Collections.singletonList(ITEM_SKU));
    doThrow(ApplicationRuntimeException.class).when(itemPickupPointWrapperService)
        .findItemPickupPointsByItemSkus(STORE_ID, simpleListStringRequest, PAGE, SIZE, null);
    this.mockMvc.perform(
            post(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.FIND_BY_ITEM_SKU_LIST).contentType(
                    MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(simpleListStringRequest))
                .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME).param("size", String.valueOf(SIZE))
                .param("page", String.valueOf(PAGE))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointWrapperService).findItemPickupPointsByItemSkus(STORE_ID, simpleListStringRequest,
        PAGE, SIZE, null);
  }

  @Test
  public void findL5sByItemSkuListExceptionTest() throws Exception {
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
    simpleListStringRequest.setValue(Collections.singletonList(ITEM_SKU));
    doThrow(Exception.class).when(itemPickupPointWrapperService)
        .findItemPickupPointsByItemSkus(STORE_ID, simpleListStringRequest, PAGE, SIZE, null);
    this.mockMvc.perform(
            post(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.FIND_BY_ITEM_SKU_LIST).contentType(
                    MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(simpleListStringRequest))
                .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME).param("size", String.valueOf(SIZE))
                .param("page", String.valueOf(PAGE))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointWrapperService).findItemPickupPointsByItemSkus(STORE_ID, simpleListStringRequest,
        PAGE, SIZE, null);
  }

  @Test
  public void fetchViewConfigsByItemSkuAndPickupPointCodeListTest() throws Exception {
    ItemPickupPointBasicResponse itemPickupPointBasicResponse = new ItemPickupPointBasicResponse();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    when(itemPickupPointWrapperService.fetchBasicDetailsByItemSkuAndPickupPointCodeList(STORE_ID,
        Collections.singletonList(itemPickupPointRequest))).thenReturn(new ArrayList<>());
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH
            + ItemPickupPointApiPath.FETCH_BASIC_DETAILS_BY_BY_ITEM_SKU_AND_PICKUP_POINT_CODE).contentType(
                MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(Collections.singleton(itemPickupPointRequest)))
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)
            .param("size", String.valueOf(SIZE)).param("page", String.valueOf(PAGE)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(true)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointWrapperService).fetchBasicDetailsByItemSkuAndPickupPointCodeList(STORE_ID,
        Collections.singletonList(itemPickupPointRequest));
  }

  @Test
  public void fetchViewConfigsByItemSkuAndPickupPointCodeListExceptionTest() throws Exception {
    ItemPickupPointBasicResponse itemPickupPointBasicResponse = new ItemPickupPointBasicResponse();
    ItemPickupPointRequest itemPickupPointRequest = new ItemPickupPointRequest();
    itemPickupPointRequest.setItemSku(ITEM_SKU);
    itemPickupPointRequest.setPickupPointCode(PICKUP_POINT_CODE);
    Mockito.doThrow(new ApplicationRuntimeException()).when(itemPickupPointWrapperService)
        .fetchBasicDetailsByItemSkuAndPickupPointCodeList(STORE_ID,
            Collections.singletonList(itemPickupPointRequest));
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH
            + ItemPickupPointApiPath.FETCH_BASIC_DETAILS_BY_BY_ITEM_SKU_AND_PICKUP_POINT_CODE).contentType(
                MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(Collections.singleton(itemPickupPointRequest)))
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)
            .param("size", String.valueOf(SIZE)).param("page", String.valueOf(PAGE)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(true)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointWrapperService).fetchBasicDetailsByItemSkuAndPickupPointCodeList(STORE_ID,
        Collections.singletonList(itemPickupPointRequest));
  }

  @Test
  public void fetchByEanCodeAndPickupPointCodesExceptionTest() throws Exception {
    EanUpcPickupPointCodeRequest request = new EanUpcPickupPointCodeRequest();
    request.setEanUpcCode(EAN_UPC);
    PickupPointDetailsDTO pickupPointDetailsDTO = new PickupPointDetailsDTO();
    pickupPointDetailsDTO.setPickupPointCode(PICKUP_POINT_CODE);
    pickupPointDetailsDTO.setBusinessPartnerCode(BP_CODE);
    request.setPickupPointDetails(Collections.singletonList(pickupPointDetailsDTO));
    Mockito.doThrow(new ApplicationRuntimeException()).when(itemPickupPointWrapperService)
        .fetchItemDetailsByEanUpcCode(STORE_ID, request, Constants.CNC);
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH
            + ItemPickupPointApiPath.FETCH_BY_EAN_CODE_AND_PICKUP_POINT_CODE).contentType(
                MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(request))
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointWrapperService).fetchItemDetailsByEanUpcCode(STORE_ID, request, Constants.CNC);
  }

  @Test
  public void fetchByEanCodeAndPickupPointCodesTest() throws Exception {
    EanUpcPickupPointCodeRequest request = new EanUpcPickupPointCodeRequest();
    request.setEanUpcCode(EAN_UPC);
    PickupPointDetailsDTO pickupPointDetailsDTO = new PickupPointDetailsDTO();
    pickupPointDetailsDTO.setPickupPointCode(PICKUP_POINT_CODE);
    pickupPointDetailsDTO.setBusinessPartnerCode(BP_CODE);
    request.setPickupPointDetails(Collections.singletonList(pickupPointDetailsDTO));
    when(itemPickupPointWrapperService.fetchItemDetailsByEanUpcCode(STORE_ID, request, Constants.CNC)).thenReturn(
        new ArrayList<>());
    this.mockMvc.perform(post(ItemPickupPointApiPath.BASE_PATH
            + ItemPickupPointApiPath.FETCH_BY_EAN_CODE_AND_PICKUP_POINT_CODE).contentType(
                MediaType.APPLICATION_JSON).content(objectMapper.writeValueAsString(request))
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointWrapperService).fetchItemDetailsByEanUpcCode(STORE_ID, request, Constants.CNC);
  }

  @Test
  public void getCncAtL5ByProductSkuTest() throws Exception {
    when(itemPickupPointWrapperService.getCncAtL5ByProductSku(eq(STORE_ID), eq(PRODUCT_SKU))).thenReturn(true);
    this.mockMvc.perform(
            get(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.GET_CNC_AT_L5_BY_PRODUCT_SKU).contentType(
                    MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
                .param("productSku", PRODUCT_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", Matchers.equalTo(true)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointWrapperService, times(1)).getCncAtL5ByProductSku(eq(STORE_ID), eq(PRODUCT_SKU));
  }

  @Test
  public void getCncAtL5ByProductSkuExceptionTest() throws Exception {
    when(itemPickupPointWrapperService.getCncAtL5ByProductSku(eq(STORE_ID), eq(PRODUCT_SKU))).thenThrow(
        new RuntimeException());
    this.mockMvc.perform(
            get(ItemPickupPointApiPath.BASE_PATH + ItemPickupPointApiPath.GET_CNC_AT_L5_BY_PRODUCT_SKU,
                ITEM_CODE, PICKUP_POINT_CODE).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
                .param("username", USERNAME).param("productSku", PRODUCT_SKU)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)))
        .andExpect(jsonPath("$.requestId", Matchers.equalTo(REQUEST_ID)));
    verify(itemPickupPointWrapperService, times(1)).getCncAtL5ByProductSku(eq(STORE_ID), eq(PRODUCT_SKU));
  }
}
