package com.gdn.x.product.rest.web.controller.api;

import static java.util.stream.Collectors.toList;
import static java.util.stream.Collectors.toSet;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.hamcrest.Matchers.notNullValue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.doThrow;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.verifyNoMoreInteractions;
import static org.mockito.Mockito.when;
import com.fasterxml.jackson.databind.ObjectMapper;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.put;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.setup.MockMvcBuilders.standaloneSetup;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Stream;

import com.gdn.common.enums.ErrorCategory;
import com.gdn.common.web.wrapper.response.GdnRestListResponse;
import com.gdn.common.web.wrapper.response.PageMetaData;
import com.gdn.x.product.enums.CurationStatus;
import com.gdn.common.web.param.MandatoryRequestParam;
import com.gdn.x.product.exception.ApiIncorrectInputDataException;
import com.gdn.x.product.model.vo.BulkDownloadProductBasicInfoResponse;
import com.gdn.x.product.model.vo.ProductSkuSizeChartResponse;
import com.gdn.x.product.rest.web.model.CombinedEditItemResponse;
import com.gdn.x.product.rest.web.model.ProductApiPath;
import com.gdn.x.product.rest.web.model.enums.ApiErrorCode;
import com.gdn.x.product.rest.web.model.request.DefaultPickupPointRequest;
import com.gdn.x.product.rest.web.model.request.DistributionInfoByOmniChannelSkusRequest;
import com.gdn.x.product.rest.web.model.request.ProductAndL5MigrationRequest;
import com.gdn.x.product.rest.web.model.request.ProductBasicMasterFieldsRequest;
import com.gdn.x.product.rest.web.model.request.ProductDetailPageEditRequest;
import com.gdn.x.product.rest.web.model.request.ProductSkuAndProductCodeRequest;
import com.gdn.x.product.rest.web.model.response.BasicProductAndItemDTO;
import com.gdn.x.product.rest.web.model.response.BasicProductResponse;
import com.gdn.x.product.rest.web.model.response.CogsResponse;
import com.gdn.x.product.rest.web.model.response.DistributionInfoByOmniChannelSkusResponse;
import com.gdn.x.product.rest.web.model.response.PrdProductResponse;
import com.gdn.x.product.rest.web.model.response.PriceRangeResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsSummaryResponseV2;

import org.hamcrest.Matchers;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
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

import com.gdn.common.exception.ApplicationRuntimeException;
import com.gdn.x.product.constants.ErrorMessages;
import com.gdn.x.product.model.vo.ProductItemsVo;
import com.gdn.x.product.rest.web.model.ProductV2ApiPath;
import com.gdn.x.product.rest.web.model.request.GetProductInfoRequestV2;
import com.gdn.x.product.rest.web.model.request.SimpleListStringRequest;
import com.gdn.x.product.rest.web.model.response.DuplicateProductDetailsResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemDataResponse;
import com.gdn.x.product.rest.web.model.response.ProductAndItemInfoResponseV2;
import com.gdn.x.product.rest.web.model.response.ProductAndItemsResponse;
import com.gdn.x.product.rest.web.model.request.ProductLevel3SummaryRequest;
import com.gdn.x.product.rest.web.model.request.CogsUpdateListRequest;
import com.gdn.x.product.rest.web.model.request.CogsUpdateRequest;
import com.gdn.x.product.rest.web.model.response.ProductSkuPickupPointResponse;
import com.gdn.x.product.service.api.ItemPickupPointService;
import com.gdn.x.product.service.api.ProductService;
import com.gdn.x.product.service.api.ProductServiceV2;
import com.gdn.x.product.service.api.ProductWrapperService;
import com.gdn.x.product.service.util.ModelConverter;

public class ProductV2ControllerTest {
  private static final String REQUEST_ID = "request-id";
  private static final String STORE_ID = "store-id";
  private static final String CHANNEL_ID = "channel";
  private static final String CLIENT_ID = "client";
  private static final String USERNAME = "username";
  private static final String PICKUP_POINT = "code";
  private static final String PRODUCT_SKU = "product-sku";
  private static final String ITEM_SKU = "itemSku";
  private static final String PICKUP_POINT_CODE = "pickupPointCode";
  private static final String MERCHANT_SKU = "merchantSku";
  private static final String MERCHANT_CODE = "merchantCode";
  private static final String PRODUCT_NOT_FOUND = "Product not found";

  private MockMvc mockMvc;

  @InjectMocks
  private ProductV2Controller productV2Controller;

  @Mock
  private ProductService productService;

  @Mock
  private ProductServiceV2 productServiceV2;

  @Mock
  private ModelConverter modelConverter;

  @Mock
  private ItemPickupPointService itemPickupPointService;

  @Mock
  private ProductWrapperService productWrapperService;

  private ProductItemsVo productItemsVo;
  private ProductAndItemsResponse productAndItemsResponse;
  private DefaultPickupPointRequest defaultPickupPointRequest;

  private ProductAndItemsSummaryResponseV2 productAndItemsSummaryResponseV2;

  @BeforeEach
  public void setUp() {
    MockitoAnnotations.openMocks(this);
    this.mockMvc = standaloneSetup(this.productV2Controller).build();
    productItemsVo = new ProductItemsVo();
    productAndItemsResponse = new ProductAndItemsResponse();
    this.productAndItemsSummaryResponseV2 = new ProductAndItemsSummaryResponseV2();
    defaultPickupPointRequest = new DefaultPickupPointRequest();
    defaultPickupPointRequest.setRepublish(false);
    defaultPickupPointRequest.setSkus(Arrays.asList(ITEM_SKU));
  }

  @AfterEach
  public void tearDown() throws Exception {
    verifyNoMoreInteractions(this.productService);
    verifyNoMoreInteractions(this.modelConverter);
    verifyNoMoreInteractions(this.productWrapperService);
  }

  @Test
  public void getProductAndItemsTest() throws Exception {
    Mockito.when(this.productService.getProductAndItemDetails(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, PICKUP_POINT, true, true, true, true, true, null)).thenReturn(productItemsVo);
    Mockito.when(this.modelConverter.convertToProductAndItemsResponse(productItemsVo, null, true)).thenReturn(productAndItemsResponse);
    this.mockMvc
        .perform(MockMvcRequestBuilders.get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_AND_ITEMS)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", USERNAME)
            .param("productSku", PRODUCT_SKU)
            .param("pickupPointCode", PICKUP_POINT)
            .param("showDeleted", Boolean.toString(true))
            .param("combineOthersBundlings", Boolean.toString(true))
            .param("off2On", Boolean.toString(true))
            .param("includeForceReview", Boolean.toString(true))
            .param("needProductData", Boolean.toString(true)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true));
    verify(this.productService).getProductAndItemDetails(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, PICKUP_POINT, true, true, true, true, true, null);
    verify(this.modelConverter).convertToProductAndItemsResponse(productItemsVo, null, true);
  }

  @Test
  public void getProductAndItemsApiIncorrectInputDataExceptionTest() throws Exception {
    Mockito.when(this.productService.getProductAndItemDetails(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, PICKUP_POINT, true, true, true, true, true,null)).thenReturn(productItemsVo);
    Mockito.doThrow(ApiIncorrectInputDataException.class).when(this.modelConverter).convertToProductAndItemsResponse(productItemsVo, null, true);
    this.mockMvc
        .perform(MockMvcRequestBuilders.get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_AND_ITEMS)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", USERNAME)
            .param("productSku", PRODUCT_SKU)
            .param("pickupPointCode", PICKUP_POINT)
            .param("showDeleted", Boolean.toString(true))
            .param("combineOthersBundlings", Boolean.toString(true))
            .param("off2On", Boolean.toString(true))
            .param("includeForceReview", Boolean.toString(true))
            .param("needProductData", Boolean.toString(true)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(false));
    verify(this.productService).getProductAndItemDetails(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, PICKUP_POINT, true, true, true, true, true,null);
    verify(this.modelConverter).convertToProductAndItemsResponse(productItemsVo, null, true);
  }

  @Test
  public void getProductAndItemsApplicationRuntimeExceptionTest() throws Exception {
    Mockito.when(this.productService.getProductAndItemDetails(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, PICKUP_POINT, true, true, true, true, true, null)).thenReturn(productItemsVo);
    Mockito.doThrow(ApplicationRuntimeException.class).when(this.modelConverter).convertToProductAndItemsResponse(productItemsVo, null, true);
    this.mockMvc
        .perform(MockMvcRequestBuilders.get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_AND_ITEMS)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", USERNAME)
            .param("productSku", PRODUCT_SKU)
            .param("pickupPointCode", PICKUP_POINT)
            .param("showDeleted", Boolean.toString(true))
            .param("combineOthersBundlings", Boolean.toString(true))
            .param("off2On", Boolean.toString(true))
            .param("includeForceReview", Boolean.toString(true))
            .param("needProductData", Boolean.toString(true)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(false));
    verify(this.productService).getProductAndItemDetails(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, PICKUP_POINT, true, true, true, true, true, null);
    verify(this.modelConverter).convertToProductAndItemsResponse(productItemsVo, null, true);
  }

  @Test
  public void getProductAndItemsExceptionTest() throws Exception {
    Mockito.when(this.productService.getProductAndItemDetails(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, PICKUP_POINT, true, true, true, true, true, null)).thenReturn(productItemsVo);
    Mockito.doThrow(RuntimeException.class).when(this.modelConverter).convertToProductAndItemsResponse(productItemsVo, null, true);
    this.mockMvc
        .perform(MockMvcRequestBuilders.get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_AND_ITEMS)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID)
            .param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID)
            .param("username", USERNAME)
            .param("productSku", PRODUCT_SKU)
            .param("pickupPointCode", PICKUP_POINT)
            .param("showDeleted", Boolean.toString(true))
            .param("combineOthersBundlings", Boolean.toString(true))
            .param("off2On", Boolean.toString(true))
            .param("includeForceReview", Boolean.toString(true))
            .param("needProductData", Boolean.toString(true)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(false));
    verify(this.productService).getProductAndItemDetails(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, PICKUP_POINT, true, true, true, true, true, null);
    verify(this.modelConverter).convertToProductAndItemsResponse(productItemsVo, null, true);
  }

  @Test
  public void getProductAndSingleItemByItemSkuAndPickupPointCodeTest() throws Exception {
    Mockito.when(
        productServiceV2.getProductAndSingleItemByItemSkuAndPickupPointCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU,
            PICKUP_POINT_CODE, true)).thenReturn(new ProductItemsVo());
    Mockito.when(
        modelConverter.convertToProductAndItemsResponseWithConvertPreOrderDetails(any(ProductItemsVo.class),
            eq(true), eq(null), eq(true))).thenReturn(new ProductAndItemsResponse());

    this.mockMvc.perform(get(ProductV2ApiPath.BASE_PATH
        + ProductV2ApiPath.GET_PRODUCT_AND_SINGLE_ITEM_BY_ITEM_SKU_AND_PICKUP_POINT_CODE).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("username", USERNAME).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("itemSku", ITEM_SKU).param("pickupPointCode", PICKUP_POINT_CODE)
        .param("includeMarkForDelete", "true")).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null))).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true))).andExpect(jsonPath("$.value", notNullValue()))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));

    Mockito.verify(productServiceV2)
        .getProductAndSingleItemByItemSkuAndPickupPointCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, PICKUP_POINT_CODE,
            true);
    Mockito.verify(modelConverter)
        .convertToProductAndItemsResponseWithConvertPreOrderDetails(any(ProductItemsVo.class),
            eq(true), eq(null), eq(true));
  }

  @Test
  public void getProductAndSingleItemByItemSkuAndPickupPointCodeExceptionTest() throws Exception {
    Mockito.when(
        productServiceV2.getProductAndSingleItemByItemSkuAndPickupPointCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU,
            PICKUP_POINT_CODE, true)).thenThrow(ApplicationRuntimeException.class);

    this.mockMvc.perform(get(ProductV2ApiPath.BASE_PATH
        + ProductV2ApiPath.GET_PRODUCT_AND_SINGLE_ITEM_BY_ITEM_SKU_AND_PICKUP_POINT_CODE).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("username", USERNAME).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("itemSku", ITEM_SKU).param("pickupPointCode", PICKUP_POINT_CODE)
        .param("includeMarkForDelete", "true")).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));

    Mockito.verify(productServiceV2)
        .getProductAndSingleItemByItemSkuAndPickupPointCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, PICKUP_POINT_CODE,
            true);
  }

  @Test
  public void getProductAndSingleItemByItemSkuAndPickupPointCodeApiIncorrectInputDataExceptionTest() throws Exception {
    Mockito.when(
        productServiceV2.getProductAndSingleItemByItemSkuAndPickupPointCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU,
            PICKUP_POINT_CODE, true)).thenThrow(ApiIncorrectInputDataException.class);

    this.mockMvc.perform(get(ProductV2ApiPath.BASE_PATH
        + ProductV2ApiPath.GET_PRODUCT_AND_SINGLE_ITEM_BY_ITEM_SKU_AND_PICKUP_POINT_CODE).param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID).param("username", USERNAME).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("itemSku", ITEM_SKU).param("pickupPointCode", PICKUP_POINT_CODE)
        .param("includeMarkForDelete", "true")).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));

    Mockito.verify(productServiceV2)
        .getProductAndSingleItemByItemSkuAndPickupPointCode(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, PICKUP_POINT_CODE,
            true);
  }

  @Test
  public void getProductAndSingleItemByItemSkuTest() throws Exception {
    Mockito.when(productServiceV2.getProductAndSingleItemByItemSku(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, true))
        .thenReturn(new ProductItemsVo());
    Mockito.when(modelConverter.convertToProductAndItemDataResponse(any(ProductItemsVo.class)))
        .thenReturn(new ProductAndItemDataResponse());

    this.mockMvc.perform(
        get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_AND_SINGLE_ITEM_BY_ITEM_SKU).param("storeId",
            STORE_ID).param("channelId", CHANNEL_ID).param("username", USERNAME).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("itemSku", ITEM_SKU).param("includeMarkForDelete", "true"))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", notNullValue())).andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));

    Mockito.verify(productServiceV2).getProductAndSingleItemByItemSku(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, true);
    Mockito.verify(modelConverter).convertToProductAndItemDataResponse(any(ProductItemsVo.class));
  }

  @Test
  public void getProductAndSingleItemByItemSkuExceptionTest() throws Exception {
    Mockito.when(productServiceV2.getProductAndSingleItemByItemSku(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, true))
        .thenThrow(ApplicationRuntimeException.class);

    this.mockMvc.perform(
        get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_AND_SINGLE_ITEM_BY_ITEM_SKU).param("storeId",
            STORE_ID).param("channelId", CHANNEL_ID).param("username", USERNAME).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("itemSku", ITEM_SKU).param("includeMarkForDelete", "true"))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));

    Mockito.verify(productServiceV2).getProductAndSingleItemByItemSku(STORE_ID, USERNAME, REQUEST_ID, ITEM_SKU, true);
  }

  @Test
  public void getProductInfoByItemSkuTest() throws Exception {
    Set<String> itemSkus = Stream.of(ITEM_SKU).collect(toSet());
    GetProductInfoRequestV2 request = new GetProductInfoRequestV2();
    request.setItemSkus(itemSkus);
    request.setOff2On(true);
    request.setPristine(true);
    ObjectMapper mapper = new ObjectMapper();
    String requestBody = mapper.writeValueAsString(request);
    ProductAndItemInfoResponseV2 expectedResult = new ProductAndItemInfoResponseV2();
    when(productServiceV2.getProductAndItemsByItemSkus(STORE_ID, USERNAME, REQUEST_ID, new HashSet<>(itemSkus), true,
        true)).thenReturn(Stream.of(productItemsVo).collect(toList()));
    when(modelConverter.convertToProductAndItemInfoResponseV2(productItemsVo)).thenReturn(expectedResult);

    this.mockMvc.perform(post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_INFO_BY_ITEM_SKUS).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null))).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true))).andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(productServiceV2).getProductAndItemsByItemSkus(STORE_ID, USERNAME, REQUEST_ID, new HashSet<>(itemSkus), true,
        true);
    verify(modelConverter).convertToProductAndItemInfoResponseV2(productItemsVo);
  }

  @Test
  public void getProductInfoByItemSkuExceptionTest() throws Exception {
    Set<String> itemSkus = Stream.of(ITEM_SKU).collect(toSet());
    GetProductInfoRequestV2 request = new GetProductInfoRequestV2();
    request.setItemSkus(itemSkus);
    request.setPristine(true);
    request.setOff2On(true);
    ObjectMapper mapper = new ObjectMapper();
    String requestBody = mapper.writeValueAsString(request);
    when(productServiceV2.getProductAndItemsByItemSkus(STORE_ID, USERNAME, REQUEST_ID, new HashSet<>(itemSkus), true,
        true)).thenThrow(new RuntimeException("Unspecified error :"));

    this.mockMvc.perform(post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_INFO_BY_ITEM_SKUS).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo("Unspecified error :")))
        .andExpect(jsonPath("$.errorCode", equalTo("GET_PRODUCT_INFO_FAILED")))
        .andExpect(jsonPath("$.success", equalTo(false))).andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(productServiceV2).getProductAndItemsByItemSkus(STORE_ID, USERNAME, REQUEST_ID, new HashSet<>(itemSkus), true,
        true);
  }

  @Test
  public void updateupdateHalalConfigOfProductTest() throws Exception {
    this.mockMvc.perform(
            put(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.UPDATE_PRODUCT_HALAL_CONFIG, PRODUCT_SKU).accept(
                    MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
                .param("username", USERNAME).param("curationStatus", CurationStatus.APPROVED.name()))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(productServiceV2).updateHalalConfigOfProduct(STORE_ID, PRODUCT_SKU, CurationStatus.APPROVED.name(),
        USERNAME);
  }

  @Test
  public void updateupdateHalalConfigOfProductExceptionTest() throws Exception {
    Mockito.doThrow(RuntimeException.class).when(productServiceV2)
        .updateHalalConfigOfProduct(STORE_ID, PRODUCT_SKU, CurationStatus.APPROVED.name(), USERNAME);
    try {
      this.mockMvc.perform(
              put(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.UPDATE_PRODUCT_HALAL_CONFIG, PRODUCT_SKU).accept(
                      MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
                  .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
                  .param("username", USERNAME).param("curationStatus", CurationStatus.APPROVED.name()))
          .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
          .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.success", equalTo(false)))
          .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    } finally {
      verify(productServiceV2).updateHalalConfigOfProduct(STORE_ID, PRODUCT_SKU, CurationStatus.APPROVED.name(),
          USERNAME);
    }
  }

  @Test
  public void updateCncActivationFlagTest() throws Exception {
    SimpleListStringRequest request = new SimpleListStringRequest();
    request.setValue(Arrays.asList(ITEM_SKU));
    ObjectMapper mapper = new ObjectMapper();
    String requestBody = mapper.writeValueAsString(request);

    this.mockMvc.perform(post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.UPDATE_CNC_ACTIVATED_FLAG).accept(
        MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null))).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true))).andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(productServiceV2).updateCncFlagAtProductAndItemLevel(STORE_ID, USERNAME,  Arrays.asList(ITEM_SKU));
  }

  @Test
  public void updateCncActivationFlagExceptionTest() throws Exception {
    SimpleListStringRequest request = new SimpleListStringRequest();
    request.setValue(Arrays.asList(ITEM_SKU));
    ObjectMapper mapper = new ObjectMapper();
    String requestBody = mapper.writeValueAsString(request);

    doThrow(Exception.class).when(productServiceV2).updateCncFlagAtProductAndItemLevel(STORE_ID, USERNAME,  Arrays.asList(ITEM_SKU));
    this.mockMvc.perform(post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.UPDATE_CNC_ACTIVATED_FLAG).accept(
        MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null))).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(false))).andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    verify(productServiceV2).updateCncFlagAtProductAndItemLevel(STORE_ID, USERNAME,  Arrays.asList(ITEM_SKU));
  }

  @Test
  public void validateDuplicateProductBySellerSkuTest() throws Exception {
    Mockito.when(productServiceV2.validateDuplicateProductBySellerSku(STORE_ID, MERCHANT_CODE, MERCHANT_SKU)).thenReturn(null);
    this.mockMvc.perform(
        get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.VALIDATE_DUPLICATE_PRODUCT_BY_SELLER_SKU, MERCHANT_CODE).param("storeId",
            STORE_ID).param("channelId", CHANNEL_ID).param("username", USERNAME).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("sellerSku", MERCHANT_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", equalTo(null))).andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    Mockito.verify(productServiceV2).validateDuplicateProductBySellerSku(STORE_ID, MERCHANT_CODE, MERCHANT_SKU);
  }

  @Test
  public void validateDuplicateProductBySellerSkuDuplicateProductTest() throws Exception {
    Mockito.when(productServiceV2.validateDuplicateProductBySellerSku(STORE_ID, MERCHANT_CODE, MERCHANT_SKU)).thenReturn(new DuplicateProductDetailsResponse());
    this.mockMvc.perform(
        get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.VALIDATE_DUPLICATE_PRODUCT_BY_SELLER_SKU, MERCHANT_CODE).param("storeId",
            STORE_ID).param("channelId", CHANNEL_ID).param("username", USERNAME).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("sellerSku", MERCHANT_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(ErrorMessages.PRODUCT_ALREADY_EXIST_WITH_THE_SELLER_SKU)))
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", notNullValue())).andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    Mockito.verify(productServiceV2).validateDuplicateProductBySellerSku(STORE_ID, MERCHANT_CODE, MERCHANT_SKU);
  }

  @Test
  public void validateDuplicateProductBySellerSkuExceptionTest() throws Exception {
    Mockito.when(productServiceV2.validateDuplicateProductBySellerSku(STORE_ID, MERCHANT_CODE, MERCHANT_SKU)).thenThrow(RuntimeException.class);
    this.mockMvc.perform(
        get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.VALIDATE_DUPLICATE_PRODUCT_BY_SELLER_SKU, MERCHANT_CODE).param("storeId",
            STORE_ID).param("channelId", CHANNEL_ID).param("username", USERNAME).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("sellerSku", MERCHANT_SKU))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.value", equalTo(null))).andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    Mockito.verify(productServiceV2).validateDuplicateProductBySellerSku(STORE_ID, MERCHANT_CODE, MERCHANT_SKU);
  }

  @Test
  public void getPrdProductDetailByProductSkuOrProductCodeTest() throws Exception {
    ProductSkuAndProductCodeRequest productSkuAndProductCodeRequest = new ProductSkuAndProductCodeRequest();
    productSkuAndProductCodeRequest.setProductCode(PRODUCT_SKU);
    ObjectMapper mapper = new ObjectMapper();
    String requestBody = mapper.writeValueAsString(productSkuAndProductCodeRequest);
    List<PrdProductResponse> prdProductResponseList = new ArrayList<>();
    Mockito.when(
            this.productServiceV2.getPrdProductDetailByProductSkuOrProductCode(STORE_ID, productSkuAndProductCodeRequest))
        .thenReturn(prdProductResponseList);
    this.mockMvc.perform(
            post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_L3_DETAIL_BY_PRODUCT_SKU_OR_PRODUCT_CODE).accept(
                    MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
                .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(true)));
    Mockito.verify(this.productServiceV2)
        .getPrdProductDetailByProductSkuOrProductCode(STORE_ID, productSkuAndProductCodeRequest);
  }

  @Test
  public void getPrdProductDetailByProductSkuOrProductCodeExceptionTest() throws Exception {
    ProductSkuAndProductCodeRequest productSkuAndProductCodeRequest = new ProductSkuAndProductCodeRequest();
    productSkuAndProductCodeRequest.setProductCode(PRODUCT_SKU);
    ObjectMapper mapper = new ObjectMapper();
    String requestBody = mapper.writeValueAsString(productSkuAndProductCodeRequest);
    Mockito.when(
            this.productServiceV2.getPrdProductDetailByProductSkuOrProductCode(STORE_ID, productSkuAndProductCodeRequest))
        .thenThrow(new RuntimeException());
    this.mockMvc.perform(
            post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_L3_DETAIL_BY_PRODUCT_SKU_OR_PRODUCT_CODE).accept(
                    MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
                .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false)));
    Mockito.verify(this.productServiceV2)
        .getPrdProductDetailByProductSkuOrProductCode(STORE_ID, productSkuAndProductCodeRequest);
  }

  public void getProductAndItemsForViewTest() throws Exception {
    productAndItemsSummaryResponseV2.setStoreId(STORE_ID);
    Mockito.when(
      this.productServiceV2.getProductAndItemsForView(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU,
        PICKUP_POINT, true, true, true, true, true)).thenReturn(productAndItemsSummaryResponseV2);
    this.mockMvc.perform(MockMvcRequestBuilders.get(
          ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_AND_ITEMS_FOR_VIEW)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME).param("productSku", PRODUCT_SKU)
        .param("pickupPointCode", PICKUP_POINT).param("showDeleted", Boolean.toString(true))
        .param("combineOthersBundlings", Boolean.toString(true))
        .param("off2On", Boolean.toString(true)).param("includeForceReview", Boolean.toString(true))
        .param("needProductData", Boolean.toString(true))).andExpect(status().isOk())
      .andExpect(jsonPath("$.success").value(true));
    verify(this.productServiceV2).getProductAndItemsForView(STORE_ID, REQUEST_ID, USERNAME,
      PRODUCT_SKU, PICKUP_POINT, true, true, true, true, true);
  }

  @Test
  public void getProductAndItemsForViewExceptionTest() throws Exception {
    Mockito.doThrow(Exception.class).when(this.productServiceV2)
      .getProductAndItemsForView(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU, PICKUP_POINT, true,
        true, true, true, true);
    this.mockMvc
      .perform(MockMvcRequestBuilders.get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_AND_ITEMS_FOR_VIEW)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID)
        .param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID)
        .param("username", USERNAME)
        .param("productSku", PRODUCT_SKU)
        .param("pickupPointCode", PICKUP_POINT)
        .param("showDeleted", Boolean.toString(true))
        .param("combineOthersBundlings", Boolean.toString(true))
        .param("off2On", Boolean.toString(true))
        .param("includeForceReview", Boolean.toString(true))
        .param("needProductData", Boolean.toString(true)))
      .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(false));
    verify(this.productServiceV2).getProductAndItemsForView(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU,
      PICKUP_POINT, true, true, true, true, true);
  }

  @Test
  public void getProductAndItemsForView_withNullStoreIDTest() throws Exception {
    productAndItemsSummaryResponseV2.setStoreId(null);
    Mockito.when(
      this.productServiceV2.getProductAndItemsForView(STORE_ID, REQUEST_ID, USERNAME, PRODUCT_SKU,
        PICKUP_POINT, true, true, true, true, true)).thenReturn(productAndItemsSummaryResponseV2);
    this.mockMvc.perform(MockMvcRequestBuilders.get(
          ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_AND_ITEMS_FOR_VIEW)
        .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
        .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
        .param("requestId", REQUEST_ID).param("username", USERNAME).param("productSku", PRODUCT_SKU)
        .param("pickupPointCode", PICKUP_POINT).param("showDeleted", Boolean.toString(true))
        .param("combineOthersBundlings", Boolean.toString(true))
        .param("off2On", Boolean.toString(true)).param("includeForceReview", Boolean.toString(true))
        .param("needProductData", Boolean.toString(true))).andExpect(status().isOk())
      .andExpect(jsonPath("$.success").value(true));
    verify(this.productServiceV2).getProductAndItemsForView(STORE_ID, REQUEST_ID, USERNAME,
      PRODUCT_SKU, PICKUP_POINT, true, true, true, true, true);
  }


  @Test
  public void getDefaultPickupPointCodeTest() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    defaultPickupPointRequest.setSkus(Arrays.asList(PRODUCT_SKU));
    String request = mapper.writeValueAsString(defaultPickupPointRequest);
    when(itemPickupPointService.getL5BasedOnProductSkuListAndOnlineOrCncFlagAndRepublishToAgp(STORE_ID, Arrays.asList(PRODUCT_SKU), false))
        .thenReturn(Arrays.asList(new ProductSkuPickupPointResponse()));
    this.mockMvc.perform(
        post(ProductV2ApiPath.BASE_PATH + ProductApiPath.GET_DEFAULT_PICKUP_POINT_CODE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(request).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(null))).andExpect(jsonPath("$.errorMessage", Matchers
        .equalTo(null)))
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));

    verify(this.itemPickupPointService).getL5BasedOnProductSkuListAndOnlineOrCncFlagAndRepublishToAgp(
        STORE_ID, Arrays.asList(PRODUCT_SKU), false);
  }

  @Test
  public void getDefaultPickupPointCodeExceptionTest() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    defaultPickupPointRequest.setSkus(Arrays.asList(PRODUCT_SKU));
    String request = mapper.writeValueAsString(defaultPickupPointRequest);
    Mockito.doThrow(RuntimeException.class).when(this.itemPickupPointService)
        .getL5BasedOnProductSkuListAndOnlineOrCncFlagAndRepublishToAgp(STORE_ID, Arrays.asList(PRODUCT_SKU), false);
    this.mockMvc.perform(
        post(ProductV2ApiPath.BASE_PATH + ProductApiPath.GET_DEFAULT_PICKUP_POINT_CODE).accept(MediaType.APPLICATION_JSON)
            .contentType(MediaType.APPLICATION_JSON).content(request).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", notNullValue())).andExpect(jsonPath("$.success", Matchers
        .equalTo(false)));

    verify(this.itemPickupPointService).getL5BasedOnProductSkuListAndOnlineOrCncFlagAndRepublishToAgp(
        STORE_ID, Arrays.asList(PRODUCT_SKU), false);
  }

  @Test
  public void getBasicProductAndItemDetailsTest() throws Exception {
    Mockito.when(productServiceV2.getBasicProductAndItemDetails(STORE_ID,
        USERNAME, REQUEST_ID, ITEM_SKU, PICKUP_POINT_CODE, false, false)).thenReturn(new BasicProductAndItemDTO());
    Mockito.when(modelConverter.toBasicProductAndItemResponse(any(), eq(null))).thenReturn(null);
    this.mockMvc.perform(
        get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_BASIC_PRODUCT_AND_ITEM_BY_ITEM_SKU_AND_PICKUP_POINT_CODE)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("username", USERNAME).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("itemSku", ITEM_SKU).param("pickupPointCode", PICKUP_POINT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo(null)))
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.value", equalTo(null))).andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));
    Mockito.verify(productServiceV2).getBasicProductAndItemDetails(STORE_ID,
            USERNAME, REQUEST_ID, ITEM_SKU, PICKUP_POINT_CODE, false, false);
    Mockito.verify(modelConverter).toBasicProductAndItemResponse(any(), eq(null));
  }

  @Test
  public void getBasicProductAndItemDetailsExceptionTest() throws Exception {
    Mockito.when(productServiceV2.getBasicProductAndItemDetails(STORE_ID,
        USERNAME, REQUEST_ID, ITEM_SKU, PICKUP_POINT_CODE, false, false)).thenReturn(new BasicProductAndItemDTO());
    Mockito.doThrow(Exception.class).when(modelConverter).toBasicProductAndItemResponse(any(), eq(null));
    this.mockMvc.perform(
        get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_BASIC_PRODUCT_AND_ITEM_BY_ITEM_SKU_AND_PICKUP_POINT_CODE)
            .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("username", USERNAME).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("itemSku", ITEM_SKU).param("pickupPointCode", PICKUP_POINT_CODE))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", notNullValue())).andExpect(jsonPath("$.success", Matchers
        .equalTo(false)));
    Mockito.verify(productServiceV2).getBasicProductAndItemDetails(STORE_ID,
        USERNAME, REQUEST_ID, ITEM_SKU, PICKUP_POINT_CODE, false, false);
    Mockito.verify(modelConverter).toBasicProductAndItemResponse(any(), eq(null));
  }

  @Test
  public void getPriceRangeForWebSkuTest() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    List<String> skuList = Arrays.asList(PRODUCT_SKU);
    String request = mapper.writeValueAsString(skuList);
    when(productServiceV2.getPriceRangeForSkus(STORE_ID, MERCHANT_CODE, Arrays.asList(PRODUCT_SKU)))
        .thenReturn(Arrays.asList(new PriceRangeResponse()));
    this.mockMvc.perform(
            post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_MIN_AND_MAX_PRICE).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(request).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("merchantCode", MERCHANT_CODE)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(null))).andExpect(jsonPath("$.errorMessage", Matchers
            .equalTo(null)))
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));

    verify(this.productServiceV2).getPriceRangeForSkus(
        STORE_ID, MERCHANT_CODE, Arrays.asList(PRODUCT_SKU));
  }

  @Test
  public void getPriceRangeForWebSkuExceptionTest() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    List<String> skuList = Arrays.asList(PRODUCT_SKU);
    String request = mapper.writeValueAsString(skuList);
    when(productServiceV2.getPriceRangeForSkus(STORE_ID, MERCHANT_CODE, Arrays.asList(PRODUCT_SKU)))
        .thenThrow(ApiIncorrectInputDataException.class);
    this.mockMvc.perform(
            post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_MIN_AND_MAX_PRICE).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(request).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
                .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).param("merchantCode", MERCHANT_CODE))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(ErrorCategory.UNSPECIFIED.getCode()))).andExpect(jsonPath("$.errorMessage", Matchers
            .equalTo(null)))
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));

    verify(this.productServiceV2).getPriceRangeForSkus(
        STORE_ID, MERCHANT_CODE, Arrays.asList(PRODUCT_SKU));
  }

  @Test
  public void getProductDetailsByProductSkuListTest() throws Exception {
    List<String> productSkusList = new ArrayList<>();
    productSkusList.add(PRODUCT_SKU);
    String bodyJson = new ObjectMapper().writeValueAsString(productSkusList);
    this.mockMvc.perform(MockMvcRequestBuilders.post(
            ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_DETAILSFOR_HALAL_PRODUCTS_BY_PRODUCT_SKU_LIST)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).content(bodyJson)).andExpect(status().isOk());
  }

  @Test
  public void getProductDetailsByProductSkuListTestForException() throws Exception {
    List<String> productSkusList = new ArrayList<>();
    productSkusList.add(PRODUCT_SKU);
    String bodyJson = new ObjectMapper().writeValueAsString(productSkusList);
    when(productServiceV2.getHalalProductResponseByProductSkus(STORE_ID, productSkusList)).thenThrow(
        NullPointerException.class);
    this.mockMvc.perform(MockMvcRequestBuilders.post(
            ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_DETAILSFOR_HALAL_PRODUCTS_BY_PRODUCT_SKU_LIST)
        .contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID).param("channelId", CHANNEL_ID)
        .param("clientId", CLIENT_ID).param("requestId", REQUEST_ID).content(bodyJson));
    verify(productServiceV2).getHalalProductResponseByProductSkus(STORE_ID, productSkusList);
  }

  @Test
  public void basicProductInfoTest() throws Exception {
    Mockito.when(productServiceV2.getBasicProductDetails(STORE_ID, PRODUCT_SKU, false)).thenReturn(new BasicProductResponse());
    this.mockMvc.perform(MockMvcRequestBuilders
            .get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.BASIC_PRODUCT_INFO, PRODUCT_SKU)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(true));
    verify(productServiceV2).getBasicProductDetails(STORE_ID, PRODUCT_SKU, false);
  }

  @Test
  public void basicProductInfoApiIncorrectInputDataExceptionTest() throws Exception {
    Mockito.when(productServiceV2.getBasicProductDetails(STORE_ID, PRODUCT_SKU, false)).thenThrow(new ApiIncorrectInputDataException());
    this.mockMvc.perform(MockMvcRequestBuilders
            .get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.BASIC_PRODUCT_INFO, PRODUCT_SKU)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(false));
    verify(productServiceV2).getBasicProductDetails(STORE_ID, PRODUCT_SKU, false);
  }

  @Test
  public void basicProductInfoExceptionTest() throws Exception {
    Mockito.when(productServiceV2.getBasicProductDetails(STORE_ID, PRODUCT_SKU, false)).thenThrow(new RuntimeException());
    this.mockMvc.perform(MockMvcRequestBuilders
            .get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.BASIC_PRODUCT_INFO, PRODUCT_SKU)
            .accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON)
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success").value(false));
    verify(productServiceV2).getBasicProductDetails(STORE_ID, PRODUCT_SKU, false);
  }

  @Test
  public void updateEditedProductAndItemPickupPointTest() throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, STORE_ID, STORE_ID, REQUEST_ID, STORE_ID,
            STORE_ID);
    ObjectMapper mapper = new ObjectMapper();
    ProductDetailPageEditRequest productDetailPageEditRequest = new ProductDetailPageEditRequest();
    String request = mapper.writeValueAsString(productDetailPageEditRequest);
    when(productWrapperService.updateEditedProductCombined(REQUEST_ID, true, productDetailPageEditRequest,
        PRODUCT_SKU, mandatoryRequestParam)).thenReturn(new CombinedEditItemResponse());
    this.mockMvc.perform(
            post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.UPDATE_PRODUCT_AND_ITEM_PICKUP_POINT, PRODUCT_SKU).accept(
                    MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(request)
                .param("storeId", STORE_ID).param("channelId", STORE_ID).param("clientId", STORE_ID)
                .param("requestId", REQUEST_ID).param("username", STORE_ID).param("updateCategory", String.valueOf(true)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", Matchers.equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", Matchers.equalTo(null)))
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    verify(this.productWrapperService).updateEditedProductCombined(REQUEST_ID, true, productDetailPageEditRequest,
        PRODUCT_SKU, mandatoryRequestParam);
  }

  @Test
  public void updateEditedProductAndItemPickupPointApiErrorCodeNonNullTest() throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, STORE_ID, STORE_ID, REQUEST_ID, STORE_ID,
            STORE_ID);
    CombinedEditItemResponse combinedEditItemResponse = new CombinedEditItemResponse();
    combinedEditItemResponse.setApiErrorCode(ApiErrorCode.L5_NOT_PRESENT);
    ObjectMapper mapper = new ObjectMapper();
    ProductDetailPageEditRequest productDetailPageEditRequest = new ProductDetailPageEditRequest();
    String request = mapper.writeValueAsString(productDetailPageEditRequest);
    when(productWrapperService.updateEditedProductCombined(REQUEST_ID, true, productDetailPageEditRequest, PRODUCT_SKU,
        mandatoryRequestParam)).thenReturn(combinedEditItemResponse);
    this.mockMvc.perform(
            post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.UPDATE_PRODUCT_AND_ITEM_PICKUP_POINT, PRODUCT_SKU).accept(
                    MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(request)
                .param("storeId", STORE_ID).param("channelId", STORE_ID).param("clientId", STORE_ID)
                .param("requestId", REQUEST_ID).param("username", STORE_ID).param("updateCategory", String.valueOf(true)))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(ApiErrorCode.L5_NOT_PRESENT.getCode())))
        .andExpect(jsonPath("$.errorMessage", Matchers.equalTo(ApiErrorCode.L5_NOT_PRESENT.getDesc())))
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    verify(this.productWrapperService).updateEditedProductCombined(REQUEST_ID, true, productDetailPageEditRequest,
        PRODUCT_SKU, mandatoryRequestParam);
  }

  @Test
  public void updateEditedProductAndItemPickupPointExceptionTest() throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, STORE_ID, STORE_ID, REQUEST_ID, STORE_ID,
            STORE_ID);
    ObjectMapper mapper = new ObjectMapper();
    ProductDetailPageEditRequest productDetailPageEditRequest = new ProductDetailPageEditRequest();
    String request = mapper.writeValueAsString(productDetailPageEditRequest);
    when(productWrapperService.updateEditedProductCombined(REQUEST_ID, true, productDetailPageEditRequest, PRODUCT_SKU,
        mandatoryRequestParam)).thenThrow(
        new ApiIncorrectInputDataException(PRODUCT_NOT_FOUND, ErrorCategory.INVALID_STATE.getCode()));
    this.mockMvc.perform(
            post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.UPDATE_PRODUCT_AND_ITEM_PICKUP_POINT, PRODUCT_SKU).accept(
                    MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(request)
                .param("storeId", STORE_ID).param("channelId", STORE_ID).param("clientId", STORE_ID)
                .param("requestId", REQUEST_ID).param("username", STORE_ID).param("updateCategory", String.valueOf(true)))
        .andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(ErrorCategory.INVALID_STATE.getCode())))
        .andExpect(jsonPath("$.errorMessage", Matchers.equalTo(PRODUCT_NOT_FOUND)))
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    verify(this.productWrapperService).updateEditedProductCombined(REQUEST_ID, true, productDetailPageEditRequest,
        PRODUCT_SKU, mandatoryRequestParam);
  }

  @Test
  public void updateEditedProductAndItemPickupPointGenericExceptionTest() throws Exception {
    MandatoryRequestParam mandatoryRequestParam =
        MandatoryRequestParam.generateMandatoryRequestParam(STORE_ID, STORE_ID, STORE_ID, REQUEST_ID, STORE_ID,
            STORE_ID);
    ObjectMapper mapper = new ObjectMapper();
    ProductDetailPageEditRequest productDetailPageEditRequest = new ProductDetailPageEditRequest();
    String request = mapper.writeValueAsString(productDetailPageEditRequest);
    when(productWrapperService.updateEditedProductCombined(REQUEST_ID, true, productDetailPageEditRequest,
        PRODUCT_SKU, mandatoryRequestParam)).thenThrow(Exception.class);
    this.mockMvc.perform(
            post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.UPDATE_PRODUCT_AND_ITEM_PICKUP_POINT, PRODUCT_SKU).accept(
                    MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(request)
                .param("storeId", STORE_ID).param("channelId", STORE_ID).param("clientId", STORE_ID)
                .param("requestId", REQUEST_ID).param("username", STORE_ID).param("updateCategory", String.valueOf(true)))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", Matchers.equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", Matchers.equalTo(null)))
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    verify(this.productWrapperService).updateEditedProductCombined(REQUEST_ID, true, productDetailPageEditRequest,
        PRODUCT_SKU, mandatoryRequestParam);
  }

  @Test
  public void getProductBasicDetailsTest() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
    String request = mapper.writeValueAsString(simpleListStringRequest);
    when(productService.findProductBasicDetailsByProductSku(STORE_ID, simpleListStringRequest, false)).thenReturn(new ArrayList<>());
    this.mockMvc.perform(
            post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_BASIC_DETAILS_BY_PRODUCT_SKU, PRODUCT_SKU).accept(
                    MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(request)
                .param("storeId", STORE_ID).param("channelId", STORE_ID).param("clientId", STORE_ID)
                .param("requestId", REQUEST_ID).param("username", STORE_ID))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", Matchers.equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", Matchers.equalTo(null)))
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    verify(productService).findProductBasicDetailsByProductSku(STORE_ID, simpleListStringRequest, false);
  }

  @Test
  public void getProductBasicDetailsExceptionTest() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
    String request = mapper.writeValueAsString(simpleListStringRequest);
    doThrow(RuntimeException.class).when(productService)
        .findProductBasicDetailsByProductSku(STORE_ID, simpleListStringRequest, false);
    this.mockMvc.perform(post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_BASIC_DETAILS_BY_PRODUCT_SKU,
            PRODUCT_SKU).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(request)
            .param("storeId", STORE_ID).param("channelId", STORE_ID).param("clientId", STORE_ID)
            .param("requestId", REQUEST_ID).param("username", STORE_ID)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    verify(productService).findProductBasicDetailsByProductSku(STORE_ID, simpleListStringRequest, false);
  }

  @Test
  public void getProductBasicDetailsByItemSkusTest() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
    String request = mapper.writeValueAsString(simpleListStringRequest);
    when(productService.findProductBasicDetailsByItemSkus(STORE_ID, simpleListStringRequest)).thenReturn(new ArrayList<>());
    this.mockMvc.perform(
            post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_BASIC_DETAILS_BY_ITEM_SKU, PRODUCT_SKU).accept(
                    MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(request)
                .param("storeId", STORE_ID).param("channelId", STORE_ID).param("clientId", STORE_ID)
                .param("requestId", REQUEST_ID).param("username", STORE_ID))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorCode", Matchers.equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", Matchers.equalTo(null)))
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    verify(productService).findProductBasicDetailsByItemSkus(STORE_ID, simpleListStringRequest);
  }

  @Test
  public void getProductBasicDetailsByItemSkusExceptionTest() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    SimpleListStringRequest simpleListStringRequest = new SimpleListStringRequest();
    String request = mapper.writeValueAsString(simpleListStringRequest);
    doThrow(RuntimeException.class).when(productService)
        .findProductBasicDetailsByItemSkus(STORE_ID, simpleListStringRequest);
    this.mockMvc.perform(post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_BASIC_DETAILS_BY_ITEM_SKU,
            PRODUCT_SKU).accept(MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(request)
            .param("storeId", STORE_ID).param("channelId", STORE_ID).param("clientId", STORE_ID)
            .param("requestId", REQUEST_ID).param("username", STORE_ID)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    verify(productService).findProductBasicDetailsByItemSkus(STORE_ID, simpleListStringRequest);
  }

  @Test
  public void findProductSkuListBySizeChartCodeTest() throws Exception {
    String sizeChartCode = "SIZE123";
    int page = 0;
    int size = 50;
    Page<ProductSkuSizeChartResponse> productSkuSizeChartResponses = new PageImpl<>(new ArrayList<>());
    GdnRestListResponse<ProductSkuSizeChartResponse> expectedResponse =
        new GdnRestListResponse<>(productSkuSizeChartResponses.getContent(),
            new PageMetaData(size, page, productSkuSizeChartResponses.getTotalElements()), REQUEST_ID);
    Mockito.when(productService.getActiveProductsByStoreIdAndSizeChartCode(sizeChartCode, page, size))
        .thenReturn(new PageImpl<>(new ArrayList<>(), PageRequest.of(0, 10), 0));
    this.mockMvc.perform(
        get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_SKU_LIST_BY_SIZE_CHART_CODE).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
            .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
            .param("page", "0").param("size", "50").param("sizeChartCode", sizeChartCode)).andExpect(status().isOk());
    Mockito.verify(productService).getActiveProductsByStoreIdAndSizeChartCode(sizeChartCode, page, size);
  }

  @Test
  public void findProductSkuListBySizeChartCodeExceptionTest() throws Exception {
    String sizeChartCode = "SIZE123";
    int page = 0;
    int size = 50;
    Mockito.doThrow(RuntimeException.class).when(productService)
        .getActiveProductsByStoreIdAndSizeChartCode(sizeChartCode, page, size);
    try {
      this.mockMvc.perform(
          get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_SKU_LIST_BY_SIZE_CHART_CODE).accept(
                  MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).param("storeId", STORE_ID)
              .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
              .param("page", "0").param("size", "50").param("sizeChartCode", sizeChartCode)).andExpect(status().isOk());

    } finally {
      Mockito.verify(productService).getActiveProductsByStoreIdAndSizeChartCode(sizeChartCode, page, size);
    }
  }

  @Test
  public void migrateProductAndL5DetailByProductSkuTest() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    ProductAndL5MigrationRequest productAndL5MigrationRequest = new ProductAndL5MigrationRequest();
    String content = mapper.writeValueAsString(productAndL5MigrationRequest);
    this.mockMvc.perform(
            post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.MIGRATE_PRODUCT_AND_L5_DETAIL_BY_PRODUCT_SKU).accept(
                    MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(content)
                .param("storeId", STORE_ID).param("channelId", STORE_ID).param("clientId", STORE_ID)
                .param("requestId", REQUEST_ID).param("username", STORE_ID)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", Matchers.equalTo(null)))
        .andExpect(jsonPath("$.success", Matchers.equalTo(true)));
    verify(productService).migrateProductAndL5DetailByProductSku(STORE_ID, productAndL5MigrationRequest);
  }

  @Test
  public void migrateProductAndL5DetailByProductSkuExceptionTest() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    ProductAndL5MigrationRequest productAndL5MigrationRequest = new ProductAndL5MigrationRequest();
    String content = mapper.writeValueAsString(productAndL5MigrationRequest);
    doThrow(RuntimeException.class).when(productService)
        .migrateProductAndL5DetailByProductSku(STORE_ID, productAndL5MigrationRequest);
    this.mockMvc.perform(
            post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.MIGRATE_PRODUCT_AND_L5_DETAIL_BY_PRODUCT_SKU).accept(
                    MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(content)
                .param("storeId", STORE_ID).param("channelId", STORE_ID).param("clientId", STORE_ID)
                .param("requestId", REQUEST_ID).param("username", STORE_ID)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorCode", Matchers.equalTo(null)))
        .andExpect(jsonPath("$.errorMessage", Matchers.equalTo(null)))
        .andExpect(jsonPath("$.success", Matchers.equalTo(false)));
    verify(productService).migrateProductAndL5DetailByProductSku(STORE_ID, productAndL5MigrationRequest);
  }

  @Test
  void getProductBasicInfoByProductSku_EmptyProductSkuList() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    ProductLevel3SummaryRequest request = new ProductLevel3SummaryRequest();
    request.setProductSkuList(Collections.emptyList());
    mockMvc.perform(
            post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_BASIC_INFO_BY_PRODUCT_SKU).param("storeId",
                    STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
                .param("username", USERNAME).contentType(MediaType.APPLICATION_JSON)
                .content(mapper.writeValueAsString(request))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(false)).andExpect(jsonPath("$.errorMessage").exists());
  }

  @Test
  void getProductBasicInfoByProductSku_ServiceException() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    ProductLevel3SummaryRequest request = new ProductLevel3SummaryRequest();
    request.setProductSkuList(Arrays.asList("SKU1", "SKU2"));
    when(productService.getProductBasicInfoByProductSkus(anyString(), any())).thenThrow(
        new RuntimeException("Service error"));
    mockMvc.perform(
            post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_BASIC_INFO_BY_PRODUCT_SKU).param("storeId",
                    STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
                .param("username", USERNAME).contentType(MediaType.APPLICATION_JSON)
                .content(mapper.writeValueAsString(request))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(false)).andExpect(jsonPath("$.errorMessage").value("Service error"));
    verify(productService).getProductBasicInfoByProductSkus(STORE_ID, request);
  }

  @Test
  void getProductBasicInfoByProductSku_Success() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    ProductLevel3SummaryRequest request = new ProductLevel3SummaryRequest();
    request.setProductSkuList(Arrays.asList("SKU1", "SKU2"));
    BulkDownloadProductBasicInfoResponse response = new BulkDownloadProductBasicInfoResponse();
    when(productService.getProductBasicInfoByProductSkus(anyString(), any())).thenReturn(response);
    mockMvc.perform(
            post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_PRODUCT_BASIC_INFO_BY_PRODUCT_SKU).param("storeId",
                    STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
                .param("username", USERNAME).contentType(MediaType.APPLICATION_JSON)
                .content(mapper.writeValueAsString(request))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(true)).andExpect(jsonPath("$.errorMessage").isEmpty())
        .andExpect(jsonPath("$.errorCode").isEmpty()).andExpect(jsonPath("$.requestId").value(REQUEST_ID));
    verify(productService).getProductBasicInfoByProductSkus(STORE_ID, request);
  }

  @Test
  void updateProductMasterFieldsInfo() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    ProductBasicMasterFieldsRequest request = new ProductBasicMasterFieldsRequest();
    request.setProductSku(PRODUCT_SKU);
    request.setInstore(false);
    mockMvc.perform(
            post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.UPDATE_PRODUCT_MASTER_FIELDS_INFO).param("storeId",
                    STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
                .param("username", USERNAME).contentType(MediaType.APPLICATION_JSON)
                .content(mapper.writeValueAsString(request))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(true)).andExpect(jsonPath("$.errorMessage").isEmpty())
        .andExpect(jsonPath("$.errorCode").isEmpty()).andExpect(jsonPath("$.requestId").value(REQUEST_ID));
    verify(productService).updateMasterDataInfo(STORE_ID, REQUEST_ID, USERNAME, request);
  }

  @Test
  void updateProductMasterFieldsInfo_EmptyProductSku() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    ProductBasicMasterFieldsRequest request = new ProductBasicMasterFieldsRequest();
    request.setProductSku("");
    mockMvc.perform(
            post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.UPDATE_PRODUCT_MASTER_FIELDS_INFO).param("storeId",
                    STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
                .param("username", USERNAME).contentType(MediaType.APPLICATION_JSON)
                .content(mapper.writeValueAsString(request))).andExpect(status().isOk())
        .andExpect(jsonPath("$.success").value(false)).andExpect(jsonPath("$.errorMessage").exists());
  }

  @Test
  public void updateCogsValueTest() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    CogsUpdateRequest cogsUpdateRequest =
        CogsUpdateRequest.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).insuredAmount(100.0).build();
    CogsUpdateListRequest request =
        CogsUpdateListRequest.builder().listRequest(Arrays.asList(cogsUpdateRequest)).build();
    String requestBody = mapper.writeValueAsString(request);

    this.mockMvc.perform(
            post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.UPDATE_COGS, PRODUCT_SKU).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(requestBody).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
                .param("username", USERNAME)).andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo("")))
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));

    verify(itemPickupPointService).updateInsuredAmountInItemPickupPoint(STORE_ID, request.getListRequest());
    Assertions.assertNotNull(PRODUCT_SKU);
  }

  @Test
  public void updateCogsValueExceptionTest() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    CogsUpdateRequest cogsUpdateRequest =
        CogsUpdateRequest.builder().itemSku(ITEM_SKU).pickupPointCode(PICKUP_POINT_CODE).insuredAmount(100.0).build();
    CogsUpdateListRequest request =
        CogsUpdateListRequest.builder().listRequest(Arrays.asList(cogsUpdateRequest)).build();
    String requestBody = mapper.writeValueAsString(request);

    doThrow(RuntimeException.class).when(itemPickupPointService)
        .updateInsuredAmountInItemPickupPoint(STORE_ID, request.getListRequest());

    this.mockMvc.perform(
            post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.UPDATE_COGS, PRODUCT_SKU).accept(MediaType.APPLICATION_JSON)
                .contentType(MediaType.APPLICATION_JSON).content(requestBody).param("storeId", STORE_ID)
                .param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID).param("requestId", REQUEST_ID)
                .param("username", USERNAME)).andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)))
        .andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));

    verify(itemPickupPointService).updateInsuredAmountInItemPickupPoint(STORE_ID, request.getListRequest());
    Assertions.assertNotNull(PRODUCT_SKU);
  }

  @Test
  public void getCogsDataTest() throws Exception {
    CogsResponse cogsResponse1 =
        CogsResponse.builder().itemSku("ITEM-001").pickupPointCode("PP001").insuredAmount(99.99).build();
    CogsResponse cogsResponse2 =
        CogsResponse.builder().itemSku("ITEM-002").pickupPointCode("PP002").insuredAmount(150.5).build();


    when(itemPickupPointService.getCogsData(STORE_ID, PRODUCT_SKU, 0, 10)).thenReturn(
        new PageImpl<>(Arrays.asList(cogsResponse1, cogsResponse2)));

    this.mockMvc.perform(
            get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_COGS, PRODUCT_SKU).accept(MediaType.APPLICATION_JSON)
                .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME).param("page", "0").param("size", "10"))
        .andExpect(status().isOk()).andExpect(jsonPath("$.errorMessage", equalTo("")))
        .andExpect(jsonPath("$.errorCode", equalTo(null))).andExpect(jsonPath("$.success", equalTo(true)))
        .andExpect(jsonPath("$.content", notNullValue()))
        .andExpect(jsonPath("$.content[0].itemSku", equalTo("ITEM-001")))
        .andExpect(jsonPath("$.content[0].pickupPointCode", equalTo("PP001")))
        .andExpect(jsonPath("$.content[0].insuredAmount", equalTo(99.99)))
        .andExpect(jsonPath("$.content[1].itemSku", equalTo("ITEM-002")))
        .andExpect(jsonPath("$.content[1].pickupPointCode", equalTo("PP002")))
        .andExpect(jsonPath("$.content[1].insuredAmount", equalTo(150.5)));

    verify(itemPickupPointService).getCogsData(STORE_ID, PRODUCT_SKU, 0, 10);
    Assertions.assertNotNull(PRODUCT_SKU);
  }

  @Test
  public void getCogsDataExceptionTest() throws Exception {
    when(itemPickupPointService.getCogsData(STORE_ID, PRODUCT_SKU, 0, 10)).thenThrow(RuntimeException.class);

    this.mockMvc.perform(
            get(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_COGS, PRODUCT_SKU).accept(MediaType.APPLICATION_JSON)
                .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
                .param("requestId", REQUEST_ID).param("username", USERNAME).param("page", "0").param("size", "10"))
        .andExpect(status().isOk()).andExpect(jsonPath("$.success", equalTo(false)));

    verify(itemPickupPointService).getCogsData(STORE_ID, PRODUCT_SKU, 0, 10);
    Assertions.assertNotNull(PRODUCT_SKU);
  }

  @Test
  public void getDistributionInfoByOmniChannelSkusTest() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    String requestBody = mapper.writeValueAsString(new DistributionInfoByOmniChannelSkusRequest());

    when(productServiceV2.checkOmniChannelSkusInSeller(STORE_ID, REQUEST_ID, USERNAME,
        new DistributionInfoByOmniChannelSkusRequest())).thenReturn(new DistributionInfoByOmniChannelSkusResponse());

    this.mockMvc.perform(post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_DISTRIBUTION_INFO_BY_OMNICHANNEL_SKUS).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.errorMessage", equalTo(null))).andExpect(jsonPath("$.errorCode", equalTo(null)))
        .andExpect(jsonPath("$.success", equalTo(true))).andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));

    verify(productServiceV2).checkOmniChannelSkusInSeller(STORE_ID, REQUEST_ID, USERNAME,
        new DistributionInfoByOmniChannelSkusRequest());
    Assertions.assertNotNull(PRODUCT_SKU);
  }

  @Test
  public void getDistributionInfoByOmniChannelSkusExceptionTest() throws Exception {
    ObjectMapper mapper = new ObjectMapper();
    String requestBody = mapper.writeValueAsString(new DistributionInfoByOmniChannelSkusRequest());

    when(productServiceV2.checkOmniChannelSkusInSeller(STORE_ID, REQUEST_ID, USERNAME,
        new DistributionInfoByOmniChannelSkusRequest())).thenThrow(RuntimeException.class);

    this.mockMvc.perform(post(ProductV2ApiPath.BASE_PATH + ProductV2ApiPath.GET_DISTRIBUTION_INFO_BY_OMNICHANNEL_SKUS).accept(
                MediaType.APPLICATION_JSON).contentType(MediaType.APPLICATION_JSON).content(requestBody)
            .param("storeId", STORE_ID).param("channelId", CHANNEL_ID).param("clientId", CLIENT_ID)
            .param("requestId", REQUEST_ID).param("username", USERNAME)).andExpect(status().isOk())
        .andExpect(jsonPath("$.success", equalTo(false))).andExpect(jsonPath("$.requestId", equalTo(REQUEST_ID)));

    verify(productServiceV2).checkOmniChannelSkusInSeller(STORE_ID, REQUEST_ID, USERNAME,
        new DistributionInfoByOmniChannelSkusRequest());
    Assertions.assertNotNull(PRODUCT_SKU);
  }
}
