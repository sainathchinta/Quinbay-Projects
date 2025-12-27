package com.gdn.x.product.rest.web.controller.api;

import static org.mockito.Mockito.verifyNoMoreInteractions;

import java.io.File;
import java.util.Collections;

import org.apache.commons.io.FileUtils;
import org.hamcrest.CoreMatchers;
import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.Mockito;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.model.vo.OfflineItemDetailVo;
import com.gdn.x.product.model.vo.SimpleMasterDataDetailWithProductAndItemsResponseVo;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.dto.SimpleItemResponseDTO;
import com.gdn.x.product.rest.web.model.dto.SimpleProductAndItemsDTO;
import com.gdn.x.product.rest.web.model.enums.ProductErrorCodesEnum;
import com.gdn.x.product.rest.web.model.request.SimpleSetStringRequest;
import com.gdn.x.product.rest.web.model.response.OfflineItemDetailResponse;
import com.gdn.x.product.rest.web.model.response.SimpleProductAndItemsMasterDataDetailResponse;
import com.gdn.x.product.service.api.ProductReindexingService;
import com.gdn.x.product.service.util.ModelConverter;

/**
 * Created by govind on 08/08/2018 AD.
 */
public class ProductReindexControllerTest {

  private static final String STORE_ID = "store-id";

  private static final String CHANNEL_ID = "channel-id";

  private static final String CLIENT_ID = "client-id";

  private static final String REQUEST_ID = "request-id";

  private static final String USERNAME = "username";

  private static final String ITEM_SKU = "item-sku";

  private static final String PICKUP_POINT_CODE = "pickup-point-code";
  private static final String PAGE = "0";
  private static final String SIZE = "100";

  private String simpleSetRequestJson;

  @InjectMocks
  private ProductReindexController productReindexController;

  @Mock
  private ProductReindexingService productReindexingService;

  @Mock
  private ModelConverter modelConverter;

  private MockMvc mockMvc;

  private ObjectMapper objectMapper;

  private SimpleSetStringRequest simpleSetRequest;

  private SimpleProductAndItemsMasterDataDetailResponse simpleProductAndItemsMasterDataDetailResponse;
  private SimpleProductAndItemsDTO simpleProductAndItemsDTO;
  private OfflineItemDetailVo offlineItemDetailVo;
  private Page<OfflineItemDetailVo> offlineItemDetailVoPage;
  private OfflineItemDetailResponse offlineItemDetailResponse;

  @BeforeEach
  public void setUp() throws Exception {
    MockitoAnnotations.openMocks(this);
    this.mockMvc = MockMvcBuilders.standaloneSetup(this.productReindexController).build();
    objectMapper = new ObjectMapper();
    this.simpleSetRequestJson =
        FileUtils.readFileToString(new File("src/test/resources/itemSkuList.json"));
    this.simpleSetRequest =
        objectMapper.readValue(this.simpleSetRequestJson,
            objectMapper.getTypeFactory()
                .constructType(SimpleSetStringRequest.class));
    simpleProductAndItemsDTO = new SimpleProductAndItemsDTO();
    SimpleItemResponseDTO simpleItemResponseDTO = new SimpleItemResponseDTO();
    simpleItemResponseDTO.setActivePromoBundlings(Collections.singleton(ITEM_SKU));
    simpleItemResponseDTO.setItemSku(ITEM_SKU);
    simpleProductAndItemsDTO.setSimpleItems(Collections.singletonList(simpleItemResponseDTO));
    simpleProductAndItemsMasterDataDetailResponse =
        new SimpleProductAndItemsMasterDataDetailResponse();
    simpleProductAndItemsMasterDataDetailResponse
        .setProductAndItems(Collections.singletonList(simpleProductAndItemsDTO));

    offlineItemDetailVo = new OfflineItemDetailVo();
    offlineItemDetailVoPage = new PageImpl<>(Collections.singletonList(offlineItemDetailVo));

    offlineItemDetailResponse = new OfflineItemDetailResponse();
    offlineItemDetailResponse.setUniqueId(ITEM_SKU);
  }

  @AfterEach
  public void tearDown() {
    verifyNoMoreInteractions(this.productReindexingService, this.modelConverter);
  }

  @Test
  public void fullReindexByProductCodesTest() throws Exception {
    SimpleMasterDataDetailWithProductAndItemsResponseVo resultVo =
        new SimpleMasterDataDetailWithProductAndItemsResponseVo();
    Mockito.when(this.productReindexingService
        .getMasterDataProductDetailResponseByProductCodes(ProductReindexControllerTest.STORE_ID,
            ProductReindexControllerTest.USERNAME, ProductReindexControllerTest.REQUEST_ID,
            this.simpleSetRequest.getValue())).thenReturn(resultVo);
    Mockito.when(modelConverter.toSimpleProductAndItemsMasterDataDetailResponse(resultVo))
        .thenReturn(new SimpleProductAndItemsMasterDataDetailResponse());
    this.mockMvc.perform(MockMvcRequestBuilders
        .post(ProductApiPath.PRODUCT_REINDEX + ProductApiPath.FULL_REINDEX_BY_PRODUCT_CODES)
        .content(this.simpleSetRequestJson).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductReindexControllerTest.STORE_ID)
        .param("channelId", ProductReindexControllerTest.CHANNEL_ID)
        .param("clientId", ProductReindexControllerTest.CLIENT_ID)
        .param("requestId", ProductReindexControllerTest.REQUEST_ID)
        .param("username", ProductReindexControllerTest.USERNAME))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorMessage", CoreMatchers.equalTo(null)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorCode", CoreMatchers.equalTo(null)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.value", Matchers.notNullValue())).andExpect(
        MockMvcResultMatchers.jsonPath("$.requestId",
            CoreMatchers.equalTo(ProductReindexControllerTest.REQUEST_ID)));
    Mockito.verify(this.productReindexingService)
        .getMasterDataProductDetailResponseByProductCodes(ProductReindexControllerTest.STORE_ID,
            ProductReindexControllerTest.USERNAME, ProductReindexControllerTest.REQUEST_ID,
            this.simpleSetRequest.getValue());
    Mockito.verify(modelConverter).toSimpleProductAndItemsMasterDataDetailResponse(resultVo);
  }

  @Test
  public void fullReindexByProductCodesLoggerTest() throws Exception {
    SimpleMasterDataDetailWithProductAndItemsResponseVo resultVo =
        new SimpleMasterDataDetailWithProductAndItemsResponseVo();
    Mockito.when(this.productReindexingService
        .getMasterDataProductDetailResponseByProductCodes(ProductReindexControllerTest.STORE_ID,
            ProductReindexControllerTest.USERNAME, ProductReindexControllerTest.REQUEST_ID,
            this.simpleSetRequest.getValue())).thenReturn(resultVo);
    Mockito.when(modelConverter.toSimpleProductAndItemsMasterDataDetailResponse(resultVo))
        .thenReturn(simpleProductAndItemsMasterDataDetailResponse);
    this.mockMvc.perform(
        MockMvcRequestBuilders.post(ProductApiPath.PRODUCT_REINDEX + ProductApiPath.FULL_REINDEX_BY_PRODUCT_CODES)
            .content(this.simpleSetRequestJson).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", ProductReindexControllerTest.STORE_ID)
            .param("channelId", ProductReindexControllerTest.CHANNEL_ID)
            .param("clientId", ProductReindexControllerTest.CLIENT_ID)
            .param("requestId", ProductReindexControllerTest.REQUEST_ID)
            .param("username", ProductReindexControllerTest.USERNAME)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorMessage", CoreMatchers.equalTo(null)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorCode", CoreMatchers.equalTo(null)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.value", Matchers.notNullValue())).andExpect(
        MockMvcResultMatchers.jsonPath("$.requestId", CoreMatchers.equalTo(ProductReindexControllerTest.REQUEST_ID)));
    Mockito.verify(this.productReindexingService)
        .getMasterDataProductDetailResponseByProductCodes(ProductReindexControllerTest.STORE_ID,
            ProductReindexControllerTest.USERNAME, ProductReindexControllerTest.REQUEST_ID,
            this.simpleSetRequest.getValue());
    Mockito.verify(modelConverter).toSimpleProductAndItemsMasterDataDetailResponse(resultVo);
  }

  @Test
  public void fullReindexByProductCodesWithApplicationRuntimeExceptionTest()
      throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(this.productReindexingService)
        .getMasterDataProductDetailResponseByProductCodes(ProductReindexControllerTest.STORE_ID,
            ProductReindexControllerTest.USERNAME, ProductReindexControllerTest.REQUEST_ID,
            this.simpleSetRequest.getValue());
    this.mockMvc.perform(MockMvcRequestBuilders
        .post(ProductApiPath.PRODUCT_REINDEX + ProductApiPath.FULL_REINDEX_BY_PRODUCT_CODES)
        .contentType(MediaType.APPLICATION_JSON).content(this.simpleSetRequestJson)
        .param("storeId", ProductReindexControllerTest.STORE_ID)
        .param("channelId", ProductReindexControllerTest.CHANNEL_ID)
        .param("clientId", ProductReindexControllerTest.CLIENT_ID)
        .param("requestId", ProductReindexControllerTest.REQUEST_ID)
        .param("username", ProductReindexControllerTest.USERNAME))
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorMessage", Matchers.notNullValue()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorCode",
            CoreMatchers.equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.value", Matchers.nullValue())).andExpect(
        MockMvcResultMatchers.jsonPath("$.requestId",
            CoreMatchers.equalTo(ProductReindexControllerTest.REQUEST_ID)));

    Mockito.verify(this.productReindexingService)
        .getMasterDataProductDetailResponseByProductCodes(ProductReindexControllerTest.STORE_ID,
            ProductReindexControllerTest.USERNAME, ProductReindexControllerTest.REQUEST_ID,
            this.simpleSetRequest.getValue());
  }

  @Test
  public void fullReindexByProductCodesLoggerExceptionTest() throws Exception {
    Mockito.when(modelConverter.toSimpleProductAndItemsMasterDataDetailResponse(null))
        .thenReturn(null);
    Mockito.when(productReindexingService
        .getMasterDataProductDetailResponseByProductCodes(ProductReindexControllerTest.STORE_ID,
            ProductReindexControllerTest.USERNAME, ProductReindexControllerTest.REQUEST_ID,
            this.simpleSetRequest.getValue())).thenReturn(null);
    this.mockMvc.perform(
        MockMvcRequestBuilders.post(ProductApiPath.PRODUCT_REINDEX + ProductApiPath.FULL_REINDEX_BY_PRODUCT_CODES)
            .contentType(MediaType.APPLICATION_JSON).content(this.simpleSetRequestJson)
            .param("storeId", ProductReindexControllerTest.STORE_ID)
            .param("channelId", ProductReindexControllerTest.CHANNEL_ID)
            .param("clientId", ProductReindexControllerTest.CLIENT_ID)
            .param("requestId", ProductReindexControllerTest.REQUEST_ID)
            .param("username", ProductReindexControllerTest.USERNAME))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true))).andExpect(
        MockMvcResultMatchers.jsonPath("$.requestId", CoreMatchers.equalTo(ProductReindexControllerTest.REQUEST_ID)));

    Mockito.verify(this.productReindexingService)
        .getMasterDataProductDetailResponseByProductCodes(ProductReindexControllerTest.STORE_ID,
            ProductReindexControllerTest.USERNAME, ProductReindexControllerTest.REQUEST_ID,
            this.simpleSetRequest.getValue());
    Mockito.verify(modelConverter).toSimpleProductAndItemsMasterDataDetailResponse(null);
  }

  @Test
  public void fullReindexByProductCodesWithExceptionTest() throws Exception {
    Mockito.doThrow(new RuntimeException()).when(this.productReindexingService)
        .getMasterDataProductDetailResponseByProductCodes(ProductReindexControllerTest.STORE_ID,
            ProductReindexControllerTest.USERNAME, ProductReindexControllerTest.REQUEST_ID,
            this.simpleSetRequest.getValue());
    this.mockMvc.perform(MockMvcRequestBuilders
        .post(ProductApiPath.PRODUCT_REINDEX + ProductApiPath.FULL_REINDEX_BY_PRODUCT_CODES)
        .contentType(MediaType.APPLICATION_JSON).content(this.simpleSetRequestJson)
        .param("storeId", ProductReindexControllerTest.STORE_ID)
        .param("channelId", ProductReindexControllerTest.CHANNEL_ID)
        .param("clientId", ProductReindexControllerTest.CLIENT_ID)
        .param("requestId", ProductReindexControllerTest.REQUEST_ID)
        .param("username", ProductReindexControllerTest.USERNAME)).andExpect(MockMvcResultMatchers
        .jsonPath("$.errorCode",
            CoreMatchers.equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.value", Matchers.nullValue())).andExpect(
        MockMvcResultMatchers.jsonPath("$.requestId",
            CoreMatchers.equalTo(ProductReindexControllerTest.REQUEST_ID)));

    Mockito.verify(this.productReindexingService)
        .getMasterDataProductDetailResponseByProductCodes(ProductReindexControllerTest.STORE_ID,
            ProductReindexControllerTest.USERNAME, ProductReindexControllerTest.REQUEST_ID,
            this.simpleSetRequest.getValue());
  }

  @Test
  public void fullReindexByProductSkusTest() throws Exception {
    SimpleMasterDataDetailWithProductAndItemsResponseVo resultVo =
        new SimpleMasterDataDetailWithProductAndItemsResponseVo();
    Mockito.when(this.productReindexingService
        .getMasterDataProductDetailResponseByProductSkus(ProductReindexControllerTest.STORE_ID,
            ProductReindexControllerTest.USERNAME, ProductReindexControllerTest.REQUEST_ID,
            this.simpleSetRequest.getValue())).thenReturn(resultVo);
    Mockito.when(modelConverter.toSimpleProductAndItemsMasterDataDetailResponse(resultVo))
        .thenReturn(new SimpleProductAndItemsMasterDataDetailResponse());
    this.mockMvc.perform(MockMvcRequestBuilders
        .post(ProductApiPath.PRODUCT_REINDEX + ProductApiPath.FULL_REINDEX_BY_PRODUCT_SKUS)
        .content(this.simpleSetRequestJson).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductReindexControllerTest.STORE_ID)
        .param("channelId", ProductReindexControllerTest.CHANNEL_ID)
        .param("clientId", ProductReindexControllerTest.CLIENT_ID)
        .param("requestId", ProductReindexControllerTest.REQUEST_ID)
        .param("username", ProductReindexControllerTest.USERNAME))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorMessage", CoreMatchers.equalTo(null)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorCode", CoreMatchers.equalTo(null)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.value", Matchers.notNullValue())).andExpect(
        MockMvcResultMatchers.jsonPath("$.requestId",
            CoreMatchers.equalTo(ProductReindexControllerTest.REQUEST_ID)));
    Mockito.verify(this.productReindexingService)
        .getMasterDataProductDetailResponseByProductSkus(ProductReindexControllerTest.STORE_ID,
            ProductReindexControllerTest.USERNAME, ProductReindexControllerTest.REQUEST_ID,
            this.simpleSetRequest.getValue());
    Mockito.verify(modelConverter).toSimpleProductAndItemsMasterDataDetailResponse(resultVo);

  }

  @Test
  public void fullReindexByProductSkusWithApplicationRuntimeExceptionTest() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(this.productReindexingService)
        .getMasterDataProductDetailResponseByProductSkus(ProductReindexControllerTest.STORE_ID,
            ProductReindexControllerTest.USERNAME, ProductReindexControllerTest.REQUEST_ID,
            this.simpleSetRequest.getValue());
    this.mockMvc.perform(MockMvcRequestBuilders
        .post(ProductApiPath.PRODUCT_REINDEX + ProductApiPath.FULL_REINDEX_BY_PRODUCT_SKUS)
        .contentType(MediaType.APPLICATION_JSON).content(this.simpleSetRequestJson)
        .param("storeId", ProductReindexControllerTest.STORE_ID)
        .param("channelId", ProductReindexControllerTest.CHANNEL_ID)
        .param("clientId", ProductReindexControllerTest.CLIENT_ID)
        .param("requestId", ProductReindexControllerTest.REQUEST_ID)
        .param("username", ProductReindexControllerTest.USERNAME))
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorMessage", Matchers.notNullValue()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorCode",
            CoreMatchers.equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.value", Matchers.nullValue())).andExpect(
        MockMvcResultMatchers.jsonPath("$.requestId",
            CoreMatchers.equalTo(ProductReindexControllerTest.REQUEST_ID)));

    Mockito.verify(this.productReindexingService)
        .getMasterDataProductDetailResponseByProductSkus(ProductReindexControllerTest.STORE_ID,
            ProductReindexControllerTest.USERNAME, ProductReindexControllerTest.REQUEST_ID,
            this.simpleSetRequest.getValue());
  }

  @Test
  public void fullReindexByProductSkusWithExceptionTest() throws Exception {
    Mockito.doThrow(new RuntimeException()).when(this.productReindexingService)
        .getMasterDataProductDetailResponseByProductSkus(ProductReindexControllerTest.STORE_ID,
            ProductReindexControllerTest.USERNAME, ProductReindexControllerTest.REQUEST_ID,
            this.simpleSetRequest.getValue());
    this.mockMvc.perform(MockMvcRequestBuilders
        .post(ProductApiPath.PRODUCT_REINDEX + ProductApiPath.FULL_REINDEX_BY_PRODUCT_SKUS)
        .contentType(MediaType.APPLICATION_JSON).content(this.simpleSetRequestJson)
        .param("storeId", ProductReindexControllerTest.STORE_ID)
        .param("channelId", ProductReindexControllerTest.CHANNEL_ID)
        .param("clientId", ProductReindexControllerTest.CLIENT_ID)
        .param("requestId", ProductReindexControllerTest.REQUEST_ID)
        .param("username", ProductReindexControllerTest.USERNAME)).andExpect(MockMvcResultMatchers
        .jsonPath("$.errorCode",
            CoreMatchers.equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.value", Matchers.nullValue())).andExpect(
        MockMvcResultMatchers.jsonPath("$.requestId",
            CoreMatchers.equalTo(ProductReindexControllerTest.REQUEST_ID)));

    Mockito.verify(this.productReindexingService)
        .getMasterDataProductDetailResponseByProductSkus(ProductReindexControllerTest.STORE_ID,
            ProductReindexControllerTest.USERNAME, ProductReindexControllerTest.REQUEST_ID,
            this.simpleSetRequest.getValue());
  }

  @Test
  public void fullReindexByItemSkuTest() throws Exception {
    SimpleMasterDataDetailWithProductAndItemsResponseVo resultVo =
        new SimpleMasterDataDetailWithProductAndItemsResponseVo();
    Mockito.when(this.productReindexingService
        .getMasterDataProductDetailResponseByItemSku(ProductReindexControllerTest.STORE_ID,
            ProductReindexControllerTest.USERNAME, ProductReindexControllerTest.REQUEST_ID,
            ITEM_SKU, false, PICKUP_POINT_CODE)).thenReturn(resultVo);
    Mockito.when(modelConverter.toSimpleProductAndItemsMasterDataDetailResponse(resultVo))
        .thenReturn(new SimpleProductAndItemsMasterDataDetailResponse());
    this.mockMvc.perform(MockMvcRequestBuilders
        .get(ProductApiPath.PRODUCT_REINDEX + ProductApiPath.FULL_REINDEX_BY_ITEM_SKU)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductReindexControllerTest.STORE_ID)
        .param("channelId", ProductReindexControllerTest.CHANNEL_ID)
        .param("clientId", ProductReindexControllerTest.CLIENT_ID)
        .param("requestId", ProductReindexControllerTest.REQUEST_ID)
        .param("username", ProductReindexControllerTest.USERNAME)
        .param("itemSku", ProductReindexControllerTest.ITEM_SKU).param("instantPickup", "false")
        .param("pickupPointCode", PICKUP_POINT_CODE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorMessage", CoreMatchers.equalTo(null)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorCode", CoreMatchers.equalTo(null)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.value", Matchers.notNullValue())).andExpect(
        MockMvcResultMatchers.jsonPath("$.requestId",
            CoreMatchers.equalTo(ProductReindexControllerTest.REQUEST_ID)));
    Mockito.verify(this.productReindexingService)
        .getMasterDataProductDetailResponseByItemSku(ProductReindexControllerTest.STORE_ID,
            ProductReindexControllerTest.USERNAME, ProductReindexControllerTest.REQUEST_ID,
            ITEM_SKU, false, PICKUP_POINT_CODE);
    Mockito.verify(modelConverter).toSimpleProductAndItemsMasterDataDetailResponse(resultVo);

  }

  @Test
  public void fullReindexByItemSkuWithApplicationRuntimeExceptionTest() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(this.productReindexingService)
        .getMasterDataProductDetailResponseByItemSku(ProductReindexControllerTest.STORE_ID,
            ProductReindexControllerTest.USERNAME, ProductReindexControllerTest.REQUEST_ID,
            ITEM_SKU, false, PICKUP_POINT_CODE);
    this.mockMvc.perform(MockMvcRequestBuilders
        .get(ProductApiPath.PRODUCT_REINDEX + ProductApiPath.FULL_REINDEX_BY_ITEM_SKU)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductReindexControllerTest.STORE_ID)
        .param("channelId", ProductReindexControllerTest.CHANNEL_ID)
        .param("clientId", ProductReindexControllerTest.CLIENT_ID)
        .param("requestId", ProductReindexControllerTest.REQUEST_ID)
        .param("username", ProductReindexControllerTest.USERNAME)
        .param("itemSku", ProductReindexControllerTest.ITEM_SKU).param("instantPickup", "false")
        .param("pickupPointCode", PICKUP_POINT_CODE))
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorMessage", Matchers.notNullValue()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorCode",
            CoreMatchers.equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.value", Matchers.nullValue())).andExpect(
        MockMvcResultMatchers.jsonPath("$.requestId",
            CoreMatchers.equalTo(ProductReindexControllerTest.REQUEST_ID)));

    Mockito.verify(this.productReindexingService)
        .getMasterDataProductDetailResponseByItemSku(ProductReindexControllerTest.STORE_ID,
            ProductReindexControllerTest.USERNAME, ProductReindexControllerTest.REQUEST_ID,
            ITEM_SKU, false, PICKUP_POINT_CODE);
  }

  @Test
  public void fullReindexByItemSkuWithExceptionTest() throws Exception {
    Mockito.doThrow(new RuntimeException()).when(this.productReindexingService)
        .getMasterDataProductDetailResponseByItemSku(ProductReindexControllerTest.STORE_ID,
            ProductReindexControllerTest.USERNAME, ProductReindexControllerTest.REQUEST_ID,
            ITEM_SKU, false, PICKUP_POINT_CODE);
    this.mockMvc.perform(MockMvcRequestBuilders
        .get(ProductApiPath.PRODUCT_REINDEX + ProductApiPath.FULL_REINDEX_BY_ITEM_SKU)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductReindexControllerTest.STORE_ID)
        .param("channelId", ProductReindexControllerTest.CHANNEL_ID)
        .param("clientId", ProductReindexControllerTest.CLIENT_ID)
        .param("requestId", ProductReindexControllerTest.REQUEST_ID)
        .param("username", ProductReindexControllerTest.USERNAME)
        .param("itemSku", ProductReindexControllerTest.ITEM_SKU).param("instantPickup", "false")
        .param("pickupPointCode", PICKUP_POINT_CODE))
        .andExpect(MockMvcResultMatchers.status().isOk()).andExpect(MockMvcResultMatchers
        .jsonPath("$.errorCode",
            CoreMatchers.equalTo(ProductErrorCodesEnum.GET_PRODUCT_AND_ITEMS.getCode())))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.value", Matchers.nullValue())).andExpect(
        MockMvcResultMatchers.jsonPath("$.requestId",
            CoreMatchers.equalTo(ProductReindexControllerTest.REQUEST_ID)));

    Mockito.verify(this.productReindexingService)
        .getMasterDataProductDetailResponseByItemSku(ProductReindexControllerTest.STORE_ID,
            ProductReindexControllerTest.USERNAME, ProductReindexControllerTest.REQUEST_ID,
            ITEM_SKU, false, PICKUP_POINT_CODE);
  }

  @Test
  public void getOfflineItemsByItemSkuTest() throws Exception {
    Mockito.when(productReindexingService.getOfflineItemsByItemSku(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU,
        PageRequest.of(0, 100))).thenReturn(offlineItemDetailVoPage);
    this.mockMvc.perform(MockMvcRequestBuilders
        .get(ProductApiPath.PRODUCT_REINDEX + ProductApiPath.GET_OFFLINE_ITEMS_BY_ITEM_SKU, ITEM_SKU)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductReindexControllerTest.STORE_ID)
        .param("channelId", ProductReindexControllerTest.CHANNEL_ID)
        .param("clientId", ProductReindexControllerTest.CLIENT_ID)
        .param("requestId", ProductReindexControllerTest.REQUEST_ID)
        .param("username", ProductReindexControllerTest.USERNAME)
        .param("page", ProductReindexControllerTest.PAGE)
        .param("size", ProductReindexControllerTest.SIZE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorMessage", CoreMatchers.equalTo(null)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorCode", CoreMatchers.equalTo(null)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.content", Matchers.notNullValue())).andExpect(
        MockMvcResultMatchers.jsonPath("$.requestId",
            CoreMatchers.equalTo(ProductReindexControllerTest.REQUEST_ID)));
    Mockito.verify(this.productReindexingService).getOfflineItemsByItemSku(STORE_ID, USERNAME, REQUEST_ID,
        ITEM_SKU, PageRequest.of(0, 100));
    Mockito.verify(modelConverter).toOfflineItemDetailResponse(Collections.singletonList(offlineItemDetailVo));
  }

  @Test
  public void getOfflineItemsByItemSkuApplicationRuntimeExceptionTest() throws Exception {
    Mockito.doThrow(new ApplicationRuntimeException()).when(this.productReindexingService).getOfflineItemsByItemSku(
        STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, PageRequest.of(0, 100));
    this.mockMvc.perform(MockMvcRequestBuilders
        .get(ProductApiPath.PRODUCT_REINDEX + ProductApiPath.GET_OFFLINE_ITEMS_BY_ITEM_SKU, ITEM_SKU)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductReindexControllerTest.STORE_ID)
        .param("channelId", ProductReindexControllerTest.CHANNEL_ID)
        .param("clientId", ProductReindexControllerTest.CLIENT_ID)
        .param("requestId", ProductReindexControllerTest.REQUEST_ID)
        .param("username", ProductReindexControllerTest.USERNAME)
        .param("page", ProductReindexControllerTest.PAGE)
        .param("size", ProductReindexControllerTest.SIZE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorMessage", Matchers.notNullValue()))
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorCode",
            CoreMatchers.equalTo(ProductErrorCodesEnum.GET_OFFLINE_ITEMS_BY_ITEM_SKU.getCode())))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.content", Matchers.empty())).andExpect(
            MockMvcResultMatchers.jsonPath("$.requestId",
                CoreMatchers.equalTo(ProductReindexControllerTest.REQUEST_ID)));
    Mockito.verify(this.productReindexingService).getOfflineItemsByItemSku(STORE_ID, USERNAME, REQUEST_ID,
        ITEM_SKU, PageRequest.of(0, 100));
  }

  @Test
  public void getOfflineItemsByItemSkuExceptionTest() throws Exception {
    Mockito.doThrow(new RuntimeException()).when(this.productReindexingService).getOfflineItemsByItemSku(
        STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, PageRequest.of(0, 100));
    this.mockMvc.perform(MockMvcRequestBuilders
        .get(ProductApiPath.PRODUCT_REINDEX + ProductApiPath.GET_OFFLINE_ITEMS_BY_ITEM_SKU, ITEM_SKU)
        .contentType(MediaType.APPLICATION_JSON)
        .param("storeId", ProductReindexControllerTest.STORE_ID)
        .param("channelId", ProductReindexControllerTest.CHANNEL_ID)
        .param("clientId", ProductReindexControllerTest.CLIENT_ID)
        .param("requestId", ProductReindexControllerTest.REQUEST_ID)
        .param("username", ProductReindexControllerTest.USERNAME)
        .param("page", ProductReindexControllerTest.PAGE)
        .param("size", ProductReindexControllerTest.SIZE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorMessage",
            CoreMatchers.equalTo(ProductErrorCodesEnum.GET_OFFLINE_ITEMS_BY_ITEM_SKU.getMessage())))
        .andExpect(MockMvcResultMatchers.jsonPath("$.errorCode",
            CoreMatchers.equalTo(ProductErrorCodesEnum.GET_OFFLINE_ITEMS_BY_ITEM_SKU.getCode())))
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(false)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.content", Matchers.empty())).andExpect(
        MockMvcResultMatchers.jsonPath("$.requestId",
            CoreMatchers.equalTo(ProductReindexControllerTest.REQUEST_ID)));
    Mockito.verify(this.productReindexingService).getOfflineItemsByItemSku(STORE_ID, USERNAME, REQUEST_ID,
        ITEM_SKU, PageRequest.of(0, 100));
  }

  @Test
  public void getOfflineItemsByItemSkuLoggerTest() throws Exception {
    Mockito.when(modelConverter.toOfflineItemDetailResponse(Collections.singletonList(offlineItemDetailVo)))
        .thenReturn(Collections.singletonList(offlineItemDetailResponse));
    Mockito.when(productReindexingService
        .getOfflineItemsByItemSku(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, PageRequest.of(0, 100)))
        .thenReturn(offlineItemDetailVoPage);
    this.mockMvc.perform(MockMvcRequestBuilders
        .get(ProductApiPath.PRODUCT_REINDEX + ProductApiPath.GET_OFFLINE_ITEMS_BY_ITEM_SKU, ITEM_SKU)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", ProductReindexControllerTest.STORE_ID)
        .param("channelId", ProductReindexControllerTest.CHANNEL_ID)
        .param("clientId", ProductReindexControllerTest.CLIENT_ID)
        .param("requestId", ProductReindexControllerTest.REQUEST_ID)
        .param("username", ProductReindexControllerTest.USERNAME).param("page", ProductReindexControllerTest.PAGE)
        .param("size", ProductReindexControllerTest.SIZE)).andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true))).andExpect(
        MockMvcResultMatchers.jsonPath("$.requestId", CoreMatchers.equalTo(ProductReindexControllerTest.REQUEST_ID)));
    Mockito.verify(this.productReindexingService)
        .getOfflineItemsByItemSku(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, PageRequest.of(0, 100));
    Mockito.verify(modelConverter).toOfflineItemDetailResponse(Collections.singletonList(offlineItemDetailVo));
  }

  @Test()
  public void getOfflineItemsByItemSkuLoggerExceptionTest() throws Exception {
    Mockito.when(modelConverter.toOfflineItemDetailResponse(Collections.singletonList(offlineItemDetailVo)))
        .thenReturn(null);
    Mockito.when(productReindexingService
        .getOfflineItemsByItemSku(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, PageRequest.of(0, 100)))
        .thenReturn(offlineItemDetailVoPage);
    try {
      this.mockMvc.perform(MockMvcRequestBuilders.get(ProductApiPath.PRODUCT_REINDEX + ProductApiPath.GET_OFFLINE_ITEMS_BY_ITEM_SKU, ITEM_SKU)
          .contentType(MediaType.APPLICATION_JSON).param("storeId", ProductReindexControllerTest.STORE_ID)
          .param("channelId", ProductReindexControllerTest.CHANNEL_ID)
          .param("clientId", ProductReindexControllerTest.CLIENT_ID)
          .param("requestId", ProductReindexControllerTest.REQUEST_ID)
          .param("username", ProductReindexControllerTest.USERNAME)
          .param("page", ProductReindexControllerTest.PAGE).param("size", ProductReindexControllerTest.SIZE))
        .andExpect(MockMvcResultMatchers.status().isOk())
        .andExpect(MockMvcResultMatchers.jsonPath("$.success", CoreMatchers.equalTo(true)))
        .andExpect(MockMvcResultMatchers.jsonPath("$.content", Matchers.nullValue())).andExpect(
          MockMvcResultMatchers.jsonPath("$.requestId", CoreMatchers.equalTo(ProductReindexControllerTest.REQUEST_ID)));
    }
    finally {
      Mockito.verify(this.productReindexingService)
        .getOfflineItemsByItemSku(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, PageRequest.of(0, 100));
      Mockito.verify(modelConverter).toOfflineItemDetailResponse(Collections.singletonList(offlineItemDetailVo));
    }

  }
}
