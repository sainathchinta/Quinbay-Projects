package com.gdn.x.product.rest.web.controller.api;

import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.util.Arrays;
import java.util.Collections;

import org.hamcrest.CoreMatchers;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.vo.SimpleMasterDataDetailWithProductAndItemsV2ResponseVo;
import com.gdn.x.product.rest.web.model.ProductReindexV2ApiPath;
import com.gdn.x.product.rest.web.model.dto.SimpleItemPickupPointResponseDTO;
import com.gdn.x.product.rest.web.model.dto.SimpleItemV2ResponseDTO;
import com.gdn.x.product.rest.web.model.dto.SimpleProductAndItemAndItemPickupPointDTO;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.ProductAndItemSkusRequest;
import com.gdn.x.product.rest.web.model.response.SimpleProductAndItemsMasterDataDetailV2Response;
import com.gdn.x.product.service.api.ProductReindexingService;
import com.gdn.x.product.service.util.ModelConverter;

public class ProductReindexV2ControllerTest {
  private static final String REQUEST_ID = "request-id";
  private static final String STORE_ID = "store-id";
  private static final String CHANNEL_ID = "channel";
  private static final String CLIENT_ID = "client";
  private static final String USERNAME = "username";
  private static final String PICKUP_POINT_CODE = "code";
  private static final String ITEM_SKU = "item-sku";
  private static final String PRODUCT_SKU = "product-sku";
  private static final String PAGE = "0";
  private static final String SIZE = "10";
  private static final int PAGE_INT = 0;
  private static final int SIZE_INT = 10;

  private MockMvc mockMvc;

  @InjectMocks
  private ProductReindexV2Controller productReindexV2Controller;

  @Mock
  private ModelConverter modelConverter;

  @Mock
  private ProductReindexingService productReindexingService;

  private SimpleProductAndItemAndItemPickupPointDTO simpleProductAndItemsDTO;
  private SimpleProductAndItemsMasterDataDetailV2Response simpleProductAndItemsMasterDataDetailResponse;
  private ProductAndItemSkusRequest productAndItemSkusRequest;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);
    this.mockMvc = standaloneSetup(this.productReindexV2Controller).build();
    simpleProductAndItemsDTO = new SimpleProductAndItemAndItemPickupPointDTO();
    SimpleItemV2ResponseDTO simpleItemResponseDTO = new SimpleItemV2ResponseDTO();
    simpleItemResponseDTO.setItemSku(ITEM_SKU);
    SimpleItemPickupPointResponseDTO simpleItemPickupPointResponseDTO = new SimpleItemPickupPointResponseDTO();
    simpleItemPickupPointResponseDTO.setItemSku(ITEM_SKU);
    simpleProductAndItemsDTO.setSimpleItems(Collections.singletonList(simpleItemResponseDTO));
    simpleProductAndItemsDTO.setItemPickupPoints(Collections.singletonList(simpleItemPickupPointResponseDTO));
    simpleProductAndItemsMasterDataDetailResponse =
        new SimpleProductAndItemsMasterDataDetailV2Response();
    simpleProductAndItemsMasterDataDetailResponse.setProductAndItems(Arrays.asList(simpleProductAndItemsDTO));
    productAndItemSkusRequest = new ProductAndItemSkusRequest();
    productAndItemSkusRequest.setProductSkus(Arrays.asList(PRODUCT_SKU));
    productAndItemSkusRequest.setItemSkus(Arrays.asList(ITEM_SKU));
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.productReindexingService);
    verifyNoMoreInteractions(this.modelConverter);
  }

  @Test
  public void fullReindexByItemSkuAndPickupPointCodeTest() throws Exception {
    SimpleMasterDataDetailWithProductAndItemsV2ResponseVo resultVo =
        new SimpleMasterDataDetailWithProductAndItemsV2ResponseVo();
    Mockito.when(this.productReindexingService.getMasterDataProductDetailV2ResponseByItemSkuAndPickupPointCode(STORE_ID,
            USERNAME, REQUEST_ID, ITEM_SKU, PICKUP_POINT_CODE, null)).thenReturn(resultVo);
    Mockito.when(modelConverter.toSimpleProductAndItemsMasterDataDetailV2Response(resultVo))
        .thenReturn(simpleProductAndItemsMasterDataDetailResponse);
    this.mockMvc.perform(MockMvcRequestBuilders
        .get(ProductReindexV2ApiPath.BASE_PATH + ProductReindexV2ApiPath.FULL_REINDEX_BY_ITEM_SKU_AND_PICKUP_POINT)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID)
        .param("username", USERNAME)
        .param("itemSku", ITEM_SKU)
        .param("pickupPointCode", PICKUP_POINT_CODE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorMessage", CoreMatchers.equalTo(null)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorCode", CoreMatchers.equalTo(null)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.value", Matchers.notNullValue())).andExpect(
        MockMvcResultMatchers.jsonPath("$.requestId",
            CoreMatchers.equalTo(REQUEST_ID)));
    Mockito.verify(this.productReindexingService).getMasterDataProductDetailV2ResponseByItemSkuAndPickupPointCode(STORE_ID,
            USERNAME, REQUEST_ID, ITEM_SKU, PICKUP_POINT_CODE, null);
    Mockito.verify(modelConverter).toSimpleProductAndItemsMasterDataDetailV2Response(resultVo);
  }

  @Test
  public void fullReindexByItemSkuAndPickupPointCodeLoggerExceptionTest() throws Exception {
    SimpleMasterDataDetailWithProductAndItemsV2ResponseVo resultVo =
        new SimpleMasterDataDetailWithProductAndItemsV2ResponseVo();
    Mockito.when(this.productReindexingService.getMasterDataProductDetailV2ResponseByItemSkuAndPickupPointCode(STORE_ID,
        USERNAME, REQUEST_ID, ITEM_SKU, PICKUP_POINT_CODE, null)).thenReturn(resultVo);
    Mockito.when(modelConverter.toSimpleProductAndItemsMasterDataDetailV2Response(resultVo))
        .thenReturn(null);
    this.mockMvc.perform(MockMvcRequestBuilders
        .get(ProductReindexV2ApiPath.BASE_PATH + ProductReindexV2ApiPath.FULL_REINDEX_BY_ITEM_SKU_AND_PICKUP_POINT)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID)
        .param("username", USERNAME)
        .param("itemSku", ITEM_SKU)
        .param("pickupPointCode", PICKUP_POINT_CODE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorMessage", CoreMatchers.equalTo(null)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorCode", CoreMatchers.equalTo(null)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.value", Matchers.nullValue())).andExpect(
        MockMvcResultMatchers.jsonPath("$.requestId",
            CoreMatchers.equalTo(REQUEST_ID)));
    Mockito.verify(this.productReindexingService).getMasterDataProductDetailV2ResponseByItemSkuAndPickupPointCode(STORE_ID,
        USERNAME, REQUEST_ID, ITEM_SKU, PICKUP_POINT_CODE, null);
    Mockito.verify(modelConverter).toSimpleProductAndItemsMasterDataDetailV2Response(resultVo);
  }

  @Test
  public void fullReindexByItemSkuWithApplicationRuntimeExceptionTest() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(this.productReindexingService)
        .getMasterDataProductDetailV2ResponseByItemSkuAndPickupPointCode(STORE_ID,
            USERNAME, REQUEST_ID, ITEM_SKU, PICKUP_POINT_CODE, null);
    this.mockMvc.perform(MockMvcRequestBuilders
        .get(ProductReindexV2ApiPath.BASE_PATH + ProductReindexV2ApiPath.FULL_REINDEX_BY_ITEM_SKU_AND_PICKUP_POINT)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID)
        .param("username", USERNAME)
        .param("itemSku", ITEM_SKU).param("instantPickup", "false")
        .param("pickupPointCode", PICKUP_POINT_CODE))
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorMessage", Matchers.notNullValue()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorCode",
            CoreMatchers.equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.value", Matchers.nullValue())).andExpect(
        MockMvcResultMatchers.jsonPath("$.requestId", CoreMatchers.equalTo(REQUEST_ID)));
    Mockito.verify(this.productReindexingService).getMasterDataProductDetailV2ResponseByItemSkuAndPickupPointCode(STORE_ID,
        USERNAME, REQUEST_ID, ITEM_SKU, PICKUP_POINT_CODE, null);
  }

  @Test
  public void fullReindexByItemSkuWithExceptionTest() throws Exception {
    Mockito.doThrow(new RuntimeException()).when(this.productReindexingService)
        .getMasterDataProductDetailV2ResponseByItemSkuAndPickupPointCode(STORE_ID,
            USERNAME, REQUEST_ID, ITEM_SKU, PICKUP_POINT_CODE, null);
    this.mockMvc.perform(MockMvcRequestBuilders
        .get(ProductReindexV2ApiPath.BASE_PATH + ProductReindexV2ApiPath.FULL_REINDEX_BY_ITEM_SKU_AND_PICKUP_POINT)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID)
        .param("username", USERNAME)
        .param("itemSku", ITEM_SKU).param("instantPickup", "false")
        .param("pickupPointCode", PICKUP_POINT_CODE))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(MockMvcResultMatchers
        .jsonPath("$.errorCode", CoreMatchers.equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.value", Matchers.nullValue())).andExpect(
        MockMvcResultMatchers.jsonPath("$.requestId", CoreMatchers.equalTo(REQUEST_ID)));
    Mockito.verify(this.productReindexingService).getMasterDataProductDetailV2ResponseByItemSkuAndPickupPointCode(STORE_ID,
        USERNAME, REQUEST_ID, ITEM_SKU, PICKUP_POINT_CODE, null);
  }

  @Test
  public void fullReindexByProductOrItemSkusTest() throws Exception {
    String bodyJson = new ObjectMapper().writeValueAsString(productAndItemSkusRequest);
    SimpleMasterDataDetailWithProductAndItemsV2ResponseVo resultVo =
        new SimpleMasterDataDetailWithProductAndItemsV2ResponseVo();
    Mockito.when(this.productReindexingService.getMasterDataProductDetailV2ResponseByProductSkusOrItemSku(STORE_ID,
        USERNAME, REQUEST_ID, productAndItemSkusRequest.getProductSkus(),
        productAndItemSkusRequest.getItemSkus(), false, PAGE_INT, SIZE_INT, null)).thenReturn(resultVo);
    Mockito.when(modelConverter.toSimpleProductAndItemsMasterDataDetailV2Response(resultVo))
        .thenReturn(simpleProductAndItemsMasterDataDetailResponse);
    this.mockMvc.perform(MockMvcRequestBuilders
        .post(ProductReindexV2ApiPath.BASE_PATH + ProductReindexV2ApiPath.ITEM_PICKUP_POINT)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID)
        .param("username", USERNAME)
        .param("page", PAGE)
        .param("size", SIZE).content(bodyJson))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorMessage", CoreMatchers.equalTo(null)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorCode", CoreMatchers.equalTo(null)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.value", Matchers.notNullValue())).andExpect(
        MockMvcResultMatchers.jsonPath("$.requestId",
            CoreMatchers.equalTo(REQUEST_ID)));
    Mockito.verify(this.productReindexingService).getMasterDataProductDetailV2ResponseByProductSkusOrItemSku(STORE_ID,
        USERNAME, REQUEST_ID, productAndItemSkusRequest.getProductSkus(),
        productAndItemSkusRequest.getItemSkus(), false, PAGE_INT, SIZE_INT, null);
    Mockito.verify(modelConverter).toSimpleProductAndItemsMasterDataDetailV2Response(resultVo);
  }

  @Test
  public void fullReindexByProductOrItemSkusWithIsArchivedTest() throws Exception {
    Mockito.doThrow(new ApiIncorrectInputDataException("errorMessage", "errorCode")).when(this.productReindexingService)
        .getMasterDataProductDetailV2ResponseByItemSkuAndPickupPointCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU,
            PICKUP_POINT_CODE, null);
    this.mockMvc.perform(MockMvcRequestBuilders
        .get(ProductReindexV2ApiPath.BASE_PATH + ProductReindexV2ApiPath.FULL_REINDEX_BY_ITEM_SKU_AND_PICKUP_POINT)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("username", USERNAME)
        .param("itemSku", ITEM_SKU).param("instantPickup", "false").param("pickupPointCode", PICKUP_POINT_CODE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorMessage", CoreMatchers.equalTo("errorMessage")))
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorCode", CoreMatchers.equalTo("errorCode")))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.value", Matchers.nullValue()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.requestId", CoreMatchers.equalTo(REQUEST_ID)));
    Mockito.verify(this.productReindexingService)
        .getMasterDataProductDetailV2ResponseByItemSkuAndPickupPointCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU,
            PICKUP_POINT_CODE, null);
  }

  @Test
  public void fullReindexByProductOrItemSkusWithExceptionTest() throws Exception {
    String bodyJson = new ObjectMapper().writeValueAsString(productAndItemSkusRequest);
    SimpleMasterDataDetailWithProductAndItemsV2ResponseVo resultVo =
        new SimpleMasterDataDetailWithProductAndItemsV2ResponseVo();
    Mockito.doThrow(new RuntimeException()).when(this.productReindexingService)
        .getMasterDataProductDetailV2ResponseByProductSkusOrItemSku(STORE_ID,
            USERNAME, REQUEST_ID, productAndItemSkusRequest.getProductSkus(),
            productAndItemSkusRequest.getItemSkus(), false, PAGE_INT, SIZE_INT, null);
    Mockito.when(modelConverter.toSimpleProductAndItemsMasterDataDetailV2Response(resultVo))
        .thenReturn(simpleProductAndItemsMasterDataDetailResponse);
    this.mockMvc.perform(MockMvcRequestBuilders
        .post(ProductReindexV2ApiPath.BASE_PATH + ProductReindexV2ApiPath.ITEM_PICKUP_POINT)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID)
        .param("username", USERNAME)
        .param("page", PAGE)
        .param("size", SIZE).content(bodyJson))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(MockMvcResultMatchers
        .jsonPath("$.errorCode", CoreMatchers.equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.value", Matchers.nullValue())).andExpect(
        MockMvcResultMatchers.jsonPath("$.requestId", CoreMatchers.equalTo(REQUEST_ID)));
    Mockito.verify(this.productReindexingService).getMasterDataProductDetailV2ResponseByProductSkusOrItemSku(STORE_ID,
        USERNAME, REQUEST_ID, productAndItemSkusRequest.getProductSkus(),
        productAndItemSkusRequest.getItemSkus(), false, PAGE_INT, SIZE_INT, null);
  }

  @Test
  public void fullReindexByProductOrItemSkusWithApplicationRuntimeExceptionTest() throws Exception {
    String bodyJson = new ObjectMapper().writeValueAsString(productAndItemSkusRequest);
    SimpleMasterDataDetailWithProductAndItemsV2ResponseVo resultVo =
        new SimpleMasterDataDetailWithProductAndItemsV2ResponseVo();
    Mockito.doThrow(new ApplicationRuntimeException()).when(this.productReindexingService)
        .getMasterDataProductDetailV2ResponseByProductSkusOrItemSku(STORE_ID,
            USERNAME, REQUEST_ID, productAndItemSkusRequest.getProductSkus(),
            productAndItemSkusRequest.getItemSkus(), false, PAGE_INT, SIZE_INT, null);
    Mockito.when(modelConverter.toSimpleProductAndItemsMasterDataDetailV2Response(resultVo))
        .thenReturn(simpleProductAndItemsMasterDataDetailResponse);
    this.mockMvc.perform(MockMvcRequestBuilders
        .post(ProductReindexV2ApiPath.BASE_PATH + ProductReindexV2ApiPath.ITEM_PICKUP_POINT)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID)
        .param("username", USERNAME)
        .param("page", PAGE)
        .param("size", SIZE).content(bodyJson))
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorMessage", Matchers.notNullValue()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorCode",
            CoreMatchers.equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.value", Matchers.nullValue())).andExpect(
        MockMvcResultMatchers.jsonPath("$.requestId", CoreMatchers.equalTo(REQUEST_ID)));
    Mockito.verify(this.productReindexingService).getMasterDataProductDetailV2ResponseByProductSkusOrItemSku(STORE_ID,
        USERNAME, REQUEST_ID, productAndItemSkusRequest.getProductSkus(),
        productAndItemSkusRequest.getItemSkus(), false, PAGE_INT, SIZE_INT, null);
  }

}
